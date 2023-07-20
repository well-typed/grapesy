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
  , getThreadInterface
  , ThreadCancelled(..)
  , ThreadInterfaceUnavailable(..)
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import GHC.Stack

import Network.GRPC.Util.STM

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

{-------------------------------------------------------------------------------
  Creating threads
-------------------------------------------------------------------------------}

newThreadState :: IO (TVar (ThreadState a))
newThreadState = newTVarIO ThreadNotStarted

forkThread ::
     TVar (ThreadState a)
  -> IO a         -- ^ Initialize the thread (runs with exceptions masked)
  -> (a -> IO ()) -- ^ Main thread body
  -> IO ()
forkThread state initThread body =
    void $ mask_ $ forkIOWithUnmask $ \unmask ->
      threadBody state unmask initThread body

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
threadBody ::
     TVar (ThreadState a)
  -> (forall x. IO x -> IO x) -- ^ Unmask exceptions
  -> IO a         -- ^ Initialize the thread (runs with exceptions masked)
  -> (a -> IO ()) -- ^ Main thread body
  -> IO ()
threadBody state unmask initThread body = do
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
      iface <- initThread
      atomically $ writeTVar state $ ThreadRunning tid iface
      res <- try $ unmask $ body iface
      atomically $ writeTVar state $
        case res of
          Left  e  -> ThreadException e
          Right () -> ThreadDone iface

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
  -> IO (Either SomeException a)
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
      ThreadDone a ->
        return $ return (Right a)
  where
    kill :: ThreadId -> IO (Either SomeException a)
    kill tid = do
        throwTo tid $ ThreadCancelled e callStack
        return $ Left e

-- | Exception thrown by 'cancelThread' to the thread to the cancelled
data ThreadCancelled = ThreadCancelled {
      -- | Reason the thread was cancelled
      threadCancelledReason :: SomeException

      -- | Callstack to the call to 'cancelThread'
    , threadCancelledCallStack :: CallStack
    }
  deriving stock (Show)
  deriving anyclass (Exception)

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
getThreadInterface :: HasCallStack => TVar (ThreadState a) -> STM a
getThreadInterface state = do
    st <- readTVar state
    case st of
      ThreadNotStarted     -> retry
      ThreadInitializing _ -> retry
      ThreadRunning _ a    -> return a
      ThreadDone      a    -> return a
      ThreadException e    -> throwSTM $ ThreadInterfaceUnavailable e callStack

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
      ThreadException e    -> throwSTM $ ThreadInterfaceUnavailable e callStack

-- | Thread interface unavailable
--
-- Thrown when we task for the thread interface but the thread has died.
data ThreadInterfaceUnavailable = ThreadInterfaceUnavailable {
      -- | The exception that killed the thread
      threadInterfaceException :: SomeException

      -- | The 'CallStack' to the call requesting the thread interface
    , threadInterfaceCallStack :: CallStack
    }
  deriving stock (Show)
  deriving anyclass (Exception)