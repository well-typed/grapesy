-- | Monitored threads
--
-- Intended for unqualified import.
module Network.GRPC.Util.Thread (
    ThreadState(..)
    -- * Creating threads
  , newThreadState
  , forkThread
  , threadBody
    -- * Access thread state
  , cancelThread
  , waitForThread
  , withThreadInterface
  , ThreadCancelled(..)
  , ThreadInterfaceUnavailable(..)
  ) where

import Control.Exception
import Control.Monad
import Foreign (newStablePtr, freeStablePtr)
import GHC.Stack

import Network.GRPC.Internal

import Debug.Concurrent

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

-- | State of a thread with public interface of type @a@
data ThreadState a =
    -- | The thread has not yet started
    --
    -- Critically for the design of this module, there is no guarantee that
    -- the thread /will/ be started.
    ThreadNotStarted

    -- | The externally visible thread interface is still being initialized
  | ThreadInitializing ThreadId

    -- | Thread is ready
  | ThreadRunning ThreadId a

    -- | Thread terminated normally
    --
    -- This still carries the thread interface: we may need it to query the
    -- thread's final status, for example.
  | ThreadDone a

    -- | Thread terminated with an exception
  | ThreadException SomeException
  deriving stock (Show, Functor)

{-------------------------------------------------------------------------------
  Creating threads
-------------------------------------------------------------------------------}

type ThreadBody a =
          (a -> IO ()) -- ^ Mark thread ready
       -> IO ()

newThreadState :: IO (TVar (ThreadState a))
newThreadState = newTVarIO ThreadNotStarted

forkThread :: HasCallStack => TVar (ThreadState a) -> ThreadBody a -> IO ()
forkThread state body =
    void $ mask_ $ forkIOWithUnmask $ \unmask ->
      threadBody state unmask body

-- | Wrap the thread body
--
-- This should be wrapped around the body of the thread, and should be called
-- with exceptions masked.
--
-- This is intended for integration with existing libraries (such as @http2@),
-- which might do the forking under the hood.
--
-- If the 'ThreadState' is anything other than 'ThreadNotStarted' on entry,
-- this function terminates immediately.
threadBody :: forall a.
     HasCallStack
  => TVar (ThreadState a)
  -> (forall x. IO x -> IO x) -- ^ Unmask exceptions
  -> ThreadBody a
  -> IO ()
threadBody state unmask body = do
    tid <- myThreadId
    shouldStart <- atomically $ do
      st <- readTVar state
      case st of
        ThreadNotStarted -> do
          writeTVar state $ ThreadInitializing tid
          return True
        _otherwise -> do
          return False
    when shouldStart $ do
      let markReady :: a -> STM ()
          markReady = writeTVar state . ThreadRunning tid

          markDone :: Either SomeException () -> STM ()
          markDone mDone = do
              modifyTVar state $ \oldState ->
                case (oldState, mDone) of
                  (ThreadRunning _ iface, Right ()) ->
                    ThreadDone iface
                  (_, Left e) ->
                    ThreadException e
                  _otherwise ->
                    error $ "threadBody: unexpected "
                         ++ show (const () <$> oldState)

      res <- try $ unmask $ body (atomically . markReady)
      atomically $ markDone res

{-------------------------------------------------------------------------------
  Stopping
-------------------------------------------------------------------------------}

-- | Kill thread if it is running
--
-- If the thread is in 'ThreadNotStarted' state, we merely change the thread
-- state to 'ThreadException'. We do /NOT/ block, since we cannot be sure if the
-- thread will be started at all (and so we might block indefinitely). This case
-- is taken into account in 'threadBody': if the thread is killed before it is
-- started, 'threadBody' will exit immediately.
--
-- We do not update the thread state here (except in the case of
-- 'ThreadNotStarted'); instead, we rely on the exception handler inside
-- 'threadBody' to do so (we are guaranteed that this thread handler is
-- installed if the thread state is anything other than 'ThreadNotStarted').
--
-- Returns the reason the thread was killed (either the exception passed as an
-- argument to 'cancelThread', or an earlier exception if the thread was already
-- killed), or the thread interface if the thread had already terminated.
cancelThread ::
     HasCallStack
  => TVar (ThreadState a)
  -> SomeException
  -> IO (Either SomeException ())
cancelThread state e = join . atomically $ do
    st <- readTVar state
    case st of
      ThreadNotStarted -> do
        writeTVar state $ ThreadException e
        return $ return (Left e)
      ThreadInitializing tid ->
        return $ kill tid
      ThreadRunning tid _ ->
        return $ kill tid
      ThreadException e' ->
        return $ return (Left e')
      ThreadDone _ ->
        return $ return (Right ())
  where
    kill :: ThreadId -> IO (Either SomeException a)
    kill tid = do
        throwTo tid $ ThreadCancelled callStack e
        return $ Left e

-- | Exception thrown by 'cancelThread' to the thread to the cancelled
data ThreadCancelled = ThreadCancelled {
      -- | Callstack to the call to 'cancelThread'
      threadCancelledCallStack :: CallStack

      -- | Reason the thread was cancelled
    , threadCancelledReason :: SomeException
    }
  deriving stock (Show)
  deriving Exception via ExceptionWrapper ThreadCancelled

instance HasNestedException ThreadCancelled where
  getNestedException = threadCancelledReason

{-------------------------------------------------------------------------------
  Interacting with the thread
-------------------------------------------------------------------------------}

-- | Get the thread's interface
--
-- The behaviour of this 'getThreadInterface' depends on the thread state; it
--
-- * blocks if the thread in case of 'ThreadNotStarted' or 'ThreadInitializing'
-- * throws 'ThreadInterfaceUnavailable' in case of 'ThreadException'.
-- * returns the thread interface otherwise
--
-- We do /not/ distinguish between 'ThreadDone' and 'ThreadRunning' here, as
-- doing so is inherently racy (we might return that the client is still
-- running, and then it terminates before the calling code can do anything with
-- that information).
--
-- NOTE: This turns off deadlock detection for the duration of the transaction.
-- It should therefore only be used for transactions that can never be blocked
-- indefinitely.
--
-- Usage note: in practice we use this to interact with threads that in turn
-- interact with @http2@ (running 'sendMessageLoop' or 'recvMessageLoop').
-- Although calls into @http2@ may in fact block indefinitely, we will /catch/
-- those exception and treat them as network failures. If a @grapesy@ function
-- ever throws a "blocked indefinitely" exception, this should be reported as a
-- bug in @grapesy@.
withThreadInterface :: forall a b.
     HasCallStack
  => TVar (ThreadState a)
  -> (a -> STM b)
  -> IO b
withThreadInterface state k =
    withoutDeadlockDetection . atomically $
      k =<< getThreadInterface
  where
    getThreadInterface :: STM a
    getThreadInterface = do
        st <- readTVar state
        case st of
          ThreadNotStarted     -> retry
          ThreadInitializing _ -> retry
          ThreadRunning _ a    -> return a
          ThreadDone      a    -> return a
          ThreadException e    -> throwSTM $
                                    ThreadInterfaceUnavailable callStack e

-- | Wait for the thread to terminate
--
-- This is similar to 'getThreadInterface', but retries when the thread is still
-- running.
waitForThread :: HasCallStack => TVar (ThreadState a) -> STM a
waitForThread state = do
    st <- readTVar state
    case st of
      ThreadNotStarted     -> retry
      ThreadInitializing _ -> retry
      ThreadRunning _ _    -> retry
      ThreadDone      a    -> return a
      ThreadException e    -> throwSTM $ ThreadInterfaceUnavailable callStack e

-- | Thread interface unavailable
--
-- Thrown when we task for the thread interface but the thread has died.
data ThreadInterfaceUnavailable = ThreadInterfaceUnavailable {
      -- | The 'CallStack' to the call requesting the thread interface
      threadInterfaceCallStack :: CallStack

      -- | The exception that killed the thread
    , threadInterfaceException :: SomeException
    }
  deriving stock (Show)
  deriving Exception via ExceptionWrapper ThreadInterfaceUnavailable

instance HasNestedException ThreadInterfaceUnavailable where
  getNestedException = threadInterfaceException

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Locally turn off deadlock detection
--
-- See also <https://well-typed.com/blog/2024/01/when-blocked-indefinitely-is-not-indefinite/>.
withoutDeadlockDetection :: IO a -> IO a
withoutDeadlockDetection k = do
    tid <- myThreadId
    bracket (newStablePtr tid) freeStablePtr $ \_ -> k

