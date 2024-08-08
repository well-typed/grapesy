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
    -- * Thread debug ID
  , DebugThreadId -- opaque
  , threadDebugId
    -- * Access thread state
  , CancelResult(..)
  , cancelThread
  , ThreadState_(..)
  , getThreadState_
  , unlessAbnormallyTerminated
  , withThreadInterface
  , waitForNormalThreadTermination
  , waitForNormalOrAbnormalThreadTermination
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Void (Void, absurd)
import Foreign (newStablePtr, freeStablePtr)
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

import Network.GRPC.Util.GHC

{-------------------------------------------------------------------------------
  Debug thread IDs
-------------------------------------------------------------------------------}

-- | Debug thread IDs
--
-- Unlike 'ThreadId', these do not correspond to a /running/ thread necessarily,
-- but just enable us to distinguish one thread from another.
data DebugThreadId = DebugThreadId {
      debugThreadId        :: Word
    , debugThreadCreatedAt :: CallStack
    }
  deriving stock (Show)

nextDebugThreadId :: MVar Word
{-# NOINLINE nextDebugThreadId #-}
nextDebugThreadId = unsafePerformIO $ newMVar 0

newDebugThreadId :: HasCallStack => IO DebugThreadId
newDebugThreadId =
    modifyMVar nextDebugThreadId $ \x -> do
      let !nextId = succ x
      return (
          nextId
        , DebugThreadId x (popIrrelevant callStack)
        )
  where
    -- Pop off the call to 'newDebugThreadId'
    --
    -- We leave the call to 'newThreadState' on the stack because it is useful
    -- to know where that was called /from/.
    popIrrelevant :: CallStack -> CallStack
    popIrrelevant = popCallStack

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

-- | State of a thread with public interface of type @a@
data ThreadState a =
    -- | The thread has not yet started
    --
    -- If the thread is cancelled before it is started, then the exception will
    -- be delivered once started. This is important, because it gives the thread
    -- control over /when/ the exception is delivered (that is, when it chooses
    -- to unmask async exceptions).
    --
    -- The alternative would be not to start the thread at all in this case, but
    -- this takes away the control mentioned above; if the thread /needs/ to do
    -- something before it can be killed, it must be given that chance. It may
    -- /seem/ that this alternative would give the caller (which /created/ the
    -- thread) more control, but actually that control is illusory, since the
    -- timing of async exceptions is anyway unpredictable.
    ThreadNotStarted DebugThreadId

    -- | The externally visible thread interface is still being initialized
  | ThreadInitializing DebugThreadId ThreadId

    -- | Thread is ready
  | ThreadRunning DebugThreadId ThreadId a

    -- | Thread terminated normally
    --
    -- This still carries the thread interface: we may need it to query the
    -- thread's final status, for example.
  | ThreadDone DebugThreadId a

    -- | Thread terminated with an exception
  | ThreadException DebugThreadId SomeException
  deriving stock (Show, Functor)

threadDebugId :: ThreadState a -> DebugThreadId
threadDebugId (ThreadNotStarted   debugId    ) = debugId
threadDebugId (ThreadInitializing debugId _  ) = debugId
threadDebugId (ThreadRunning      debugId _ _) = debugId
threadDebugId (ThreadDone         debugId   _) = debugId
threadDebugId (ThreadException    debugId   _) = debugId

{-------------------------------------------------------------------------------
  Creating threads
-------------------------------------------------------------------------------}

type ThreadBody a =
          (forall x. IO x -> IO x) -- ^ Unmask exceptions
       -> (a -> IO ())             -- ^ Mark thread ready
       -> DebugThreadId            -- ^ Unique identifier for this thread
       -> IO ()

newThreadState :: HasCallStack => IO (TVar (ThreadState a))
newThreadState = do
    debugId <- newDebugThreadId
    newTVarIO $ ThreadNotStarted debugId

forkThread ::
     HasCallStack
  => ThreadLabel -> TVar (ThreadState a) -> ThreadBody a -> IO ()
forkThread label state body =
    void $ mask_ $ forkIOWithUnmask $ \unmask ->
      threadBody label state $ body unmask

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
  => ThreadLabel
  -> TVar (ThreadState a)
  -> ((a -> IO ()) -> DebugThreadId -> IO ())
  -> IO ()
threadBody label state body = do
    labelThisThread label
    threadId  <- myThreadId
    initState <- atomically $ readTVar state

    -- See discussion of 'ThreadNotStarted'
    -- It's critical that async exceptions are masked at this point.
    case initState of
      ThreadNotStarted debugId -> do
        atomically $ writeTVar state $ ThreadInitializing debugId threadId
      ThreadException _ exception ->
        -- We don't change the thread status here: 'cancelThread' offers the
        -- guarantee that the thread status /will/ be in aborted or done state
        -- on return. This means that /externally/ the thread will be
        -- considered done, even if perhaps the thread must still execute some
        -- actions before it can actually terminate.
        void . forkIO $ throwTo threadId exception
      _otherwise -> do
        unexpected "initState" initState

    let markReady :: a -> STM ()
        markReady a = do
            modifyTVar state $ \oldState ->
              case oldState of
                ThreadInitializing debugId _ ->
                  ThreadRunning debugId threadId a
                ThreadException _ _ ->
                  oldState -- leave alone (see discussion above)
                _otherwise ->
                  unexpected "markReady" oldState

        markDone :: Either SomeException () -> STM ()
        markDone mDone = do
            modifyTVar state $ \oldState ->
              case (oldState, mDone) of
                (ThreadRunning debugId _ iface, Right ()) ->
                  ThreadDone debugId iface
                (ThreadException{}, _) ->
                  oldState -- record /first/ exception
                (_, Left e) ->
                  ThreadException (threadDebugId oldState) e
                _otherwise ->
                  unexpected "markDone" oldState

    res <- try $ body (atomically . markReady) (threadDebugId initState)
    atomically $ markDone res
  where
    unexpected :: String -> ThreadState a -> x
    unexpected msg st = error $ concat [
          msg
        , ": unexpected "
        , show (const () <$> st)
        ]

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
--   'ThreadException'. The thread may still be started (see discussion of
--   'ThreadNotStarted'), but /externally/ the thread will be considered to have
--   terminated.
--
-- * If the thread is initializing or running, we update the state to
--   'ThreadException' and then throw the specified exception to the thread.
--
-- * If the thread is /already/ in 'ThreadException' state, or if the thread is
--   in 'ThreadDone' state, we do nothing.
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
          ThreadNotStarted debugId -> do
            writeTVar state $ ThreadException debugId e
            return (Cancelled, Nothing)
          ThreadInitializing debugId threadId -> do
            writeTVar state $ ThreadException debugId e
            return (Cancelled, Just threadId)
          ThreadRunning debugId threadId _ -> do
            writeTVar state $ ThreadException debugId e
            return (Cancelled, Just threadId)
          ThreadException _debugId e' ->
            return (AlreadyAborted e', Nothing)
          ThreadDone _debugId a ->
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
          ThreadNotStarted   _     -> retry
          ThreadInitializing _ _   -> retry
          ThreadRunning      _ _ a -> return a
          ThreadDone         _   a -> return a
          ThreadException    _   e -> throwSTM e

-- | Wait for the thread to terminate normally
--
-- If the thread terminated with an exception, this rethrows that exception.
waitForNormalThreadTermination :: TVar (ThreadState a) -> STM ()
waitForNormalThreadTermination state =
    waitUntilInitialized state >>= \case
      ThreadNotYetRunning_ v -> absurd v
      ThreadRunning_         -> retry
      ThreadDone_            -> return ()
      ThreadException_ e     -> throwSTM e

-- | Wait for the thread to terminate normally or abnormally
waitForNormalOrAbnormalThreadTermination ::
     TVar (ThreadState a)
  -> STM (Maybe SomeException)
waitForNormalOrAbnormalThreadTermination state =
    waitUntilInitialized state >>= \case
      ThreadNotYetRunning_ v -> absurd v
      ThreadRunning_         -> retry
      ThreadDone_            -> return $ Nothing
      ThreadException_ e     -> return $ Just e

-- | Run the specified transaction, unless the thread terminated with an
-- exception
unlessAbnormallyTerminated ::
     TVar (ThreadState a)
  -> STM b
  -> STM (Either SomeException b)
unlessAbnormallyTerminated state f =
    waitUntilInitialized state >>= \case
      ThreadNotYetRunning_ v -> absurd v
      ThreadRunning_         -> Right <$> f
      ThreadDone_            -> Right <$> f
      ThreadException_ e     -> return $ Left e

waitUntilInitialized ::
     TVar (ThreadState a)
  -> STM (ThreadState_ Void)
waitUntilInitialized state = getThreadState_ state retry

-- | An abstraction of 'ThreadState' without the public interface type.
data ThreadState_ notRunning =
      ThreadNotYetRunning_ notRunning
    | ThreadRunning_
    | ThreadDone_
    | ThreadException_ SomeException

getThreadState_ ::
     TVar (ThreadState a)
  -> STM notRunning
  -> STM (ThreadState_ notRunning)
getThreadState_ state onNotRunning = do
    st <- readTVar state
    case st of
      ThreadNotStarted   _     -> ThreadNotYetRunning_ <$> onNotRunning
      ThreadInitializing _ _   -> ThreadNotYetRunning_ <$> onNotRunning
      ThreadRunning      _ _ _ -> return $ ThreadRunning_
      ThreadDone         _   _ -> return $ ThreadDone_
      ThreadException    _   e -> return $ ThreadException_ e

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Locally turn off deadlock detection
--
-- See also <https://well-typed.com/blog/2024/01/when-blocked-indefinitely-is-not-indefinite/>.
withoutDeadlockDetection :: IO a -> IO a
withoutDeadlockDetection k = do
    threadId <- myThreadId
    bracket (newStablePtr threadId) freeStablePtr $ \_ -> k

