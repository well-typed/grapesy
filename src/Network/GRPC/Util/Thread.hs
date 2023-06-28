-- | Monitored threads
--
-- Intended for unqualified import.
module Network.GRPC.Util.Thread (
    ThreadState(..)
  , threadBody
  , cancelThread
  , getThreadInterface
  , ThreadCancelled(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

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
  Running
-------------------------------------------------------------------------------}

-- | Wrap the thread body
--
-- If the 'ThreadState' is anything other than 'ThreadNotStarted' on entry,
-- this function terminates immediately.
--
-- == Discussion
--
-- This is intended for integration with existing libraries (such as @http2@),
-- which might do the forking under the hood. For this reason, this does /NOT/
-- do the actual fork, but should instead be wrapped around the body of the
-- thread.
--
-- It would be better if the thread would be called with exceptions
-- masked and passed an @unmask@ function, but we cannot require this for the
-- same reason. This leaves a small window of time in between the thread being
-- forked by the external code and entering the scope of the 'mask' inside
-- 'threadBody'; if the thread is now killed using 'killThread', a call to
-- 'getThreadInterface' could block indefinitely.
--
-- However, a call to 'cancelThread' is unproblematic, as it will itself
-- correctly update the 'ThreadState' if the thread is in 'ThreadNotStarted'
-- state.
--
-- TL;DR: The thread should be killed with 'cancelThread', not 'killThread'.
threadBody ::
     TVar (ThreadState a)
  -> IO a         -- ^ Initialize the thread (runs with exceptions masked)
  -> (a -> IO ()) -- ^ Main thread body
  -> IO ()
threadBody state initThread body = mask $ \unmask -> do
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
cancelThread :: Exception e => TVar (ThreadState a) -> e -> IO ()
cancelThread state e = do
    mTid <- atomically $ do
      st <- readTVar state
      case st of
        ThreadNotStarted -> do
          writeTVar state $ ThreadException (toException e)
          return Nothing
        ThreadInitializing tid   -> return $ Just tid
        ThreadRunning      tid _ -> return $ Just tid
        ThreadDone             _ -> return Nothing
        ThreadException        _ -> return Nothing

    case mTid of
      Nothing  -> return ()
      Just tid -> throwTo tid $ ThreadCancelled (toException e)

-- | Exception thrown by 'cancelThread' to the thread to the cancelled
newtype ThreadCancelled = ThreadCancelled {
      threadCancelledReason :: SomeException
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
-- * throws an exception in case of 'ThreadException'.
-- * returns the thread interface otherwise
--
-- We do /not/ distinguish between 'ThreadDone' and 'ThreadRunning' here, as
-- doing so is inherently racy (we might return that the client is still
-- running, and then it terminates before the calling code can do anything with
-- that information).
getThreadInterface :: TVar (ThreadState a) -> STM a
getThreadInterface state =  do
    st <- readTVar state
    case st of
      ThreadNotStarted     -> retry
      ThreadInitializing _ -> retry
      ThreadRunning _ a    -> return a
      ThreadDone      a    -> return a
      ThreadException e    -> throwSTM e
