-- | Monitored threads
--
-- Intended for unqualified import.
module Network.GRPC.Util.Thread (
    ThreadState(..)
    -- * Creating threads
  , ThreadBody
  , newThreadState
  , forkThread
  , threadBody
    -- * Access thread state
  , CancelResult(..)
  , cancelThread
  , waitForThread
  , withThreadInterface
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Foreign (newStablePtr, freeStablePtr)
import GHC.Stack

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
          (forall x. IO x -> IO x) -- ^ Unmask exceptions
       -> (a -> IO ())             -- ^ Mark thread ready
       -> IO ()

newThreadState :: IO (TVar (ThreadState a))
newThreadState = newTVarIO ThreadNotStarted

forkThread :: HasCallStack => TVar (ThreadState a) -> ThreadBody a -> IO ()
forkThread state body =
    void $ mask_ $ forkIOWithUnmask $ \unmask ->
      threadBody state $ body unmask

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
  -> ((a -> IO ()) -> IO ())
  -> IO ()
threadBody state body = do
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

      res <- try $ body (atomically . markReady)
      atomically $ markDone res

{-------------------------------------------------------------------------------
  Stopping
-------------------------------------------------------------------------------}

-- | Result of cancelling a thread
data CancelResult a =
    -- | The thread terminated normally before we could cancel it
    AlreadyTerminated a

    -- | The thread terminated with an exception before we could cancel it
  | AlreadyAborted SomeException

    -- | We killed the thread with the specified exception
  | Cancelled

-- | Kill thread if it is running
--
-- * If the thread is in `ThreadNotStarted` state, we merely change the state to
--   'ThreadException'. We do /NOT/ block, since we cannot be sure if the thread
--   will be started at all (and so we might block indefinitely). This case is
--   taken into account in 'threadBody': if the thread is killed before it is
--   started, 'threadBody' will exit immediately.
--
-- * If the thread is initializing or running, we update the state to
--   'ThreadException' and then throw the specified exception to the thread.
--
-- * If the thread is /already/ in 'ThreadException' state, or if the thread
--   is in 'ThreadDone' state, we do nothing.
--
-- In all cases, the caller is guaranteed that the thread state has been updated
-- even if perhaps the thread is still shutting down.
cancelThread :: forall a.
     TVar (ThreadState a)
  -> SomeException
  -> IO (CancelResult a)
cancelThread state e = do
    (result, mTid) <- atomically aux
    forM_ mTid $ flip throwTo e
    return result
  where
    aux :: STM (CancelResult a, Maybe ThreadId)
    aux = do
        st <- readTVar state
        case st of
          ThreadNotStarted -> do
            writeTVar state $ ThreadException e
            return (Cancelled, Nothing)
          ThreadInitializing tid -> do
            writeTVar state $ ThreadException e
            return (Cancelled, Just tid)
          ThreadRunning tid _ -> do
            writeTVar state $ ThreadException e
            return (Cancelled, Just tid)
          ThreadException e' ->
            return (AlreadyAborted e', Nothing)
          ThreadDone a ->
            return (AlreadyTerminated a, Nothing)

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
     TVar (ThreadState a)
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
          ThreadException e    -> throwSTM e

-- | Wait for the thread to terminate
--
-- This is similar to 'getThreadInterface', but retries when the thread is still
-- running.
waitForThread :: TVar (ThreadState a) -> STM a
waitForThread state = do
    st <- readTVar state
    case st of
      ThreadNotStarted     -> retry
      ThreadInitializing _ -> retry
      ThreadRunning _ _    -> retry
      ThreadDone      a    -> return a
      ThreadException e    -> throwSTM e

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

