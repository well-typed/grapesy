{-# LANGUAGE CPP #-}

-- | Monitored threads
--
-- Intended for unqualified import.
module Network.GRPC.Util.Thread (
    ThreadState(..)
  , ThreadException(..)
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

import Network.GRPC.Util.Imports

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Foreign qualified
import System.IO.Unsafe (unsafePerformIO)

import Network.GRPC.Common.Exception
import Network.GRPC.Util.GHC

#if MIN_VERSION_base(4,20,0)
import Control.Exception.Annotation
#endif

{-------------------------------------------------------------------------------
  Debug thread IDs
-------------------------------------------------------------------------------}

-- | Debug thread IDs
--
-- Unlike 'ThreadId', these do not correspond to a /running/ thread necessarily,
-- but just enable us to distinguish one thread from another.
data DebugThreadId = DebugThreadId {
      debugThreadId        :: Word
    , debugThreadCreatedAt :: Backtraces
    }
  deriving stock (Show)

nextDebugThreadId :: MVar Word
{-# NOINLINE nextDebugThreadId #-}
nextDebugThreadId = unsafePerformIO $ newMVar 0

newDebugThreadId :: HasCallStack => IO DebugThreadId
newDebugThreadId = do
    backtrace <- collectBacktraces
    modifyMVar nextDebugThreadId $ \x -> do
      let !nextId = succ x
      return (
          nextId
        , DebugThreadId x backtrace
        )

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Exception that killed a thread
data ThreadException = ThreadException{
      threadException           :: ExactException
    , threadExceptionAnnotation :: ThreadExceptionAnnotation
    }
  deriving stock (Show)

data ThreadExceptionAnnotation =
    -- | Thread body itself threw an exception
    ThreadThrewException

    -- | Thread was cancelled
    --
    -- We record the backtrace of the cancellation
  | ThreadCancelled Backtraces
  deriving stock (Show)

data ThreadInterfaceUnavailable =
    -- | Attempt to access the thread interface after the thread died
    --
    -- We record the backtrace of the attempt to access the thread interface,
    -- which will be different from the backtrace of the exception that
    -- killed the thread
    ThreadInterfaceUnavailable Backtraces
  deriving stock (Show)

#if MIN_VERSION_base(4,20,0)
instance ExceptionAnnotation ThreadExceptionAnnotation
instance ExceptionAnnotation ThreadInterfaceUnavailable
#endif

throwThreadException :: (MonadIO m, HasCallStack) => ThreadException -> m a
throwThreadException e = liftIO $ do
#if MIN_VERSION_base(4,20,0)
    backtraces <- collectBacktraces
    annotateIO (threadExceptionAnnotation e) $
      annotateIO (ThreadInterfaceUnavailable backtraces) $
#endif
        throwExact (threadException e)

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
  | ThreadDied DebugThreadId ThreadException
  deriving stock (Show, Functor)

threadDebugId :: ThreadState a -> DebugThreadId
threadDebugId (ThreadNotStarted   debugId    ) = debugId
threadDebugId (ThreadInitializing debugId _  ) = debugId
threadDebugId (ThreadRunning      debugId _ _) = debugId
threadDebugId (ThreadDone         debugId   _) = debugId
threadDebugId (ThreadDied         debugId   _) = debugId

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
    initState <- readTVarIO state

    -- See discussion of 'ThreadNotStarted'
    -- It's critical that async exceptions are masked at this point.
    case initState of
      ThreadNotStarted debugId -> do
        atomically $ writeTVar state $ ThreadInitializing debugId threadId
      ThreadDied _ ThreadException{threadException} ->
        -- We don't change the thread status here: 'cancelThread' offers the
        -- guarantee that the thread status /will/ be in aborted or done state
        -- on return. This means that /externally/ the thread will be
        -- considered done, even if perhaps the thread must still execute some
        -- actions before it can actually terminate.
        void . forkIO $ throwTo threadId threadException
      _otherwise -> do
        unexpected "initState" initState

    let markReady :: a -> STM ()
        markReady a = do
            modifyTVar state $ \oldState ->
              case oldState of
                ThreadInitializing debugId _ ->
                  ThreadRunning debugId threadId a
                ThreadDied _ _ ->
                  oldState -- leave alone (see discussion above)
                _otherwise ->
                  unexpected "markReady" oldState

        markDone :: Either ExactException () -> STM ()
        markDone mDone = do
            modifyTVar state $ \oldState ->
              case (oldState, mDone) of
                (ThreadRunning debugId _ iface, Right ()) ->
                  ThreadDone debugId iface
                (ThreadDied{}, _) ->
                  oldState -- record /first/ exception
                (_, Left e) ->
                  ThreadDied (threadDebugId oldState) ThreadException{
                      threadException = e
                    , threadExceptionAnnotation = ThreadThrewException
                    }
                _otherwise ->
                  unexpected "markDone" oldState

    res <- tryExact $ body (atomically . markReady) (threadDebugId initState)
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
  | AlreadyAborted ThreadException

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
     HasCallStack
  => TVar (ThreadState a)
  -> ExactException
  -> IO (CancelResult a)
cancelThread state e = do
    backtrace <- collectBacktraces

    let cancelled :: ThreadException
        cancelled = ThreadException{
              threadException           = e
            , threadExceptionAnnotation = ThreadCancelled backtrace
            }

    (result, mTid) <- atomically $ aux cancelled
    forM_ mTid $ flip throwTo e
    return result
  where
    aux :: ThreadException -> STM (CancelResult a, Maybe ThreadId)
    aux cancelled = do
        st <- readTVar state
        case st of
          ThreadNotStarted debugId -> do
            writeTVar state $ ThreadDied debugId cancelled
            return (Cancelled, Nothing)
          ThreadInitializing debugId threadId -> do
            writeTVar state $ ThreadDied debugId cancelled
            return (Cancelled, Just threadId)
          ThreadRunning debugId threadId _ -> do
            writeTVar state $ ThreadDied debugId cancelled
            return (Cancelled, Just threadId)
          ThreadDied _debugId e' ->
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
     HasCallStack
  => TVar (ThreadState a)
  -> (a -> STM b)
  -> IO b
withThreadInterface state k = do
    mb :: Either ThreadException b <- withoutDeadlockDetection $ atomically $ do
      ma <- getThreadInterface
      either (return . Left) (fmap Right . k) ma
    either throwThreadException return mb
  where
    getThreadInterface :: STM (Either ThreadException a)
    getThreadInterface = do
        st <- readTVar state
        case st of
          ThreadNotStarted   _     -> retry
          ThreadInitializing _ _   -> retry
          ThreadRunning      _ _ a -> return (Right a)
          ThreadDone         _   a -> return (Right a)
          ThreadDied         _   e -> return (Left e)

-- | Wait for the thread to terminate normally
--
-- If the thread terminated with an exception, this rethrows that exception.
waitForNormalThreadTermination :: HasCallStack => TVar (ThreadState a) -> IO ()
waitForNormalThreadTermination state = do
    mErr <- atomically $ waitForNormalOrAbnormalThreadTermination state
    maybe (return ()) throwThreadException mErr

-- | Wait for the thread to terminate normally or abnormally
waitForNormalOrAbnormalThreadTermination ::
     TVar (ThreadState a)
  -> STM (Maybe ThreadException)
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
  -> STM (Either ThreadException b)
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
    | ThreadException_ ThreadException

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
      ThreadDied         _   e -> return $ ThreadException_ e

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Locally turn off deadlock detection
--
-- See also <https://well-typed.com/blog/2024/01/when-blocked-indefinitely-is-not-indefinite/>.
withoutDeadlockDetection :: IO a -> IO a
withoutDeadlockDetection k = do
    threadId <- myThreadId
    bracket (Foreign.newStablePtr threadId) Foreign.freeStablePtr $ \_ -> k
