{-# LANGUAGE CPP #-}

-- | Monitored threads
--
-- Intended for unqualified import.
module Network.GRPC.Util.Thread (
    ThreadState(..)
  , ThreadException(..)
    -- * Creating threads
  , ThreadContext(..)
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
  , ThreadIface(..)
  , withThreadInterface
  , waitForNormalThreadTermination
  , waitForNormalOrAbnormalThreadTermination
  ) where

import Network.GRPC.Util.Imports

import Control.Concurrent
import Control.Concurrent.STM (STM, TVar)
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as E
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
    backtraces <- collectBacktraces
    annotateIO (threadExceptionAnnotation e) $
      annotateIO (ThreadInterfaceUnavailable backtraces) $
        throwExact (threadException e)

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

-- | State of a thread with public interface of type @a@
data ThreadState a r r' =
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
  | ThreadDone DebugThreadId a r

    -- | Trivial thread
    --
    -- See 'threadTrivial' for discussion.
  | ThreadTrivial DebugThreadId r'

    -- | Thread terminated with an exception
  | ThreadDied DebugThreadId ThreadException
  deriving stock (Show)

-- | For debugging: reduce to skeleton
showableState :: ThreadState a r r' -> ThreadState () () ()
showableState = \case
    ThreadNotStarted   did       -> ThreadNotStarted   did
    ThreadInitializing did tid   -> ThreadInitializing did tid
    ThreadRunning      did tid _ -> ThreadRunning      did tid ()
    ThreadDone         did _ _   -> ThreadDone         did () ()
    ThreadTrivial      did _     -> ThreadTrivial      did ()
    ThreadDied         did e     -> ThreadDied         did e

threadDebugId :: ThreadState a r r' -> DebugThreadId
threadDebugId (ThreadNotStarted   debugId    ) = debugId
threadDebugId (ThreadInitializing debugId _  ) = debugId
threadDebugId (ThreadRunning      debugId _ _) = debugId
threadDebugId (ThreadDone         debugId _ _) = debugId
threadDebugId (ThreadTrivial      debugId _  ) = debugId
threadDebugId (ThreadDied         debugId   _) = debugId

{-------------------------------------------------------------------------------
  Creating threads
-------------------------------------------------------------------------------}

-- | Thread context
--
-- The thread body /must/ call either 'threadMainBody' or 'threadTrivial' when
-- it is ready:
--
-- > threadBody =
-- >   .. initial setup ..
-- >   case foo of
-- >     .. -> .. threadMainBody ..
-- >     .. -> .. treadTrivial   ..
--
-- Any attempt to interact with the thread will block until it marks itself
-- ready through 'threadMainBody' or 'threadTrivial' (or it dies).
data ThreadContext a r r' = ThreadContext{
      -- | Mark thread ready, providing the main thread body
      threadMainBody :: a -> IO r -> IO ()

      -- | Terminate the thread immediately
      --
      -- We refer to this as a \"trivial\" thread: it's a thread that provides
      -- a result without having to do further work. This is a slightly odd
      -- abstraction, but useful: we may start a thread using @http2@ to
      -- receive messages, but as soon as we receive the headers realize that
      -- no further work needs to be done. We treat this separately, to allow
      -- this case to have a diferent result type.
    , threadTrivial :: r' -> IO ()

      -- | Unique identifier for this thread
    , threadId :: DebugThreadId
    }

type ThreadBody a r r' =
      (forall x. IO x -> IO x)
      -- ^ Unmask exceptions
      --
      -- If using 'forkThread', the thread is started with exceptions masked
   -> ThreadContext a r r'
      -- ^ Thread context
      --
      -- This allows the thread body to inspect and manipulate its own context.
   -> IO ()

newThreadState :: HasCallStack => IO (TVar (ThreadState a r r'))
newThreadState = do
    debugId <- newDebugThreadId
    STM.newTVarIO $ ThreadNotStarted debugId

forkThread ::
     HasCallStack
  => ThreadLabel -> TVar (ThreadState a r r') -> ThreadBody a r r' -> IO ()
forkThread label state body =
    void $ E.mask_ $ forkIOWithUnmask $ \unmask ->
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
threadBody :: forall a r r'.
     HasCallStack
  => ThreadLabel
  -> TVar (ThreadState a r r')
  -> (ThreadContext a r r' -> IO ())
  -> IO ()
threadBody label state body = do
    labelThisThread label
    threadId  <- myThreadId
    initState <- STM.readTVarIO state

    -- See discussion of 'ThreadNotStarted'
    -- It's critical that async exceptions are masked at this point.
    case initState of
      ThreadNotStarted debugId -> do
        atomically $ STM.writeTVar state $ ThreadInitializing debugId threadId
      ThreadDied _ ThreadException{threadException} ->
        -- We don't change the thread status here: 'cancelThread' offers the
        -- guarantee that the thread status /will/ be in aborted or done state
        -- on return. This means that /externally/ the thread will be
        -- considered done, even if perhaps the thread must still execute some
        -- actions before it can actually terminate.
        void . forkIO $ throwTo threadId threadException
      _otherwise -> do
        unexpected "initState" initState

    let threadMainBody :: a -> IO r -> IO ()
        threadMainBody a mainBody = do
            atomically $ STM.modifyTVar state $ \oldState ->
              case oldState of
                ThreadInitializing debugId _ ->
                  ThreadRunning debugId threadId a
                ThreadDied _ _ ->
                  oldState -- leave alone (see discussion above)
                _otherwise ->
                  unexpected "mainBody before" oldState
            r <- mainBody
            atomically $ STM.modifyTVar state $ \oldState ->
              case oldState of
                ThreadRunning debugId _ _ ->
                  ThreadDone debugId a r
                ThreadDied{} ->
                  oldState
                _otherwise ->
                  unexpected "mainBody after" oldState

        threadTrivial :: r' -> IO ()
        threadTrivial r' = do
            atomically $ STM.modifyTVar state $ \oldState ->
              case oldState of
                ThreadInitializing debugId _ ->
                  ThreadTrivial debugId r'
                ThreadDied{} ->
                  oldState
                _otherwise ->
                  unexpected "markReady before" oldState

        done :: Either ExactException () -> STM ()
        done mDone = do
            STM.modifyTVar state $ \oldState ->
              case (oldState, mDone) of
                (ThreadDied{}, _) ->
                  oldState -- record /first/ exception
                (_, Left e) ->
                  ThreadDied (threadDebugId oldState) ThreadException{
                      threadException = e
                    , threadExceptionAnnotation = ThreadThrewException
                    }
                _otherwise ->
                  oldState

    res <- tryExact $ body ThreadContext{
          threadMainBody
        , threadTrivial
        , threadId = threadDebugId initState
        }
    atomically $ done res
  where
    unexpected :: HasCallStack => String -> ThreadState a r r' -> x
    unexpected msg st = error $ concat [
          msg
        , ": unexpected "
        , show (showableState st)
        ]

{-------------------------------------------------------------------------------
  Stopping
-------------------------------------------------------------------------------}

-- | Result of cancelling a thread
data CancelResult a r r' =
    -- | The thread terminated normally before we could cancel it
    AlreadyTerminated (Either (a, r)  r')

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
cancelThread :: forall a r r'.
     HasCallStack
  => TVar (ThreadState a r r')
  -> ExactException
  -> IO (CancelResult a r r')
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
    aux :: ThreadException -> STM (CancelResult a r r', Maybe ThreadId)
    aux cancelled = do
        st <- STM.readTVar state
        case st of
          ThreadNotStarted debugId -> do
            STM.writeTVar state $ ThreadDied debugId cancelled
            return (Cancelled, Nothing)
          ThreadInitializing debugId threadId -> do
            STM.writeTVar state $ ThreadDied debugId cancelled
            return (Cancelled, Just threadId)
          ThreadRunning debugId threadId _ -> do
            STM.writeTVar state $ ThreadDied debugId cancelled
            return (Cancelled, Just threadId)
          ThreadDied _debugId e' ->
            return (AlreadyAborted e', Nothing)
          ThreadDone _debugId a r ->
            return (AlreadyTerminated (Left (a, r)), Nothing)
          ThreadTrivial _debugId r' ->
            return (AlreadyTerminated (Right r'), Nothing)

{-------------------------------------------------------------------------------
  Interacting with the thread
-------------------------------------------------------------------------------}

data ThreadIface a r' =
    -- | The thread interface is available
    --
    -- We do /not/ distinguish between 'ThreadDone' and 'ThreadRunning' here, as
    -- doing so is inherently racy (we might return that the client is still
    -- running, and then it terminates before the calling code can do anything with
    -- that information).
    IfaceAvailable a

    -- | Thread was trivial (terminated immediately)
  | IfaceTrivial r'

-- | Get the thread's interface
--
-- The behaviour of this 'getThreadInterface' depends on the thread state; it
--
-- * blocks if the thread in case of 'ThreadNotStarted' or 'ThreadInitializing'
-- * throws 'ThreadInterfaceUnavailable' in case of 'ThreadException'.
-- * returns the thread interface otherwise
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
withThreadInterface :: forall a b r r'.
     HasCallStack
  => TVar (ThreadState a r r')
  -> (ThreadIface a r' -> STM b)
  -> IO b
withThreadInterface state k = do
    mb :: Either ThreadException b <- withoutDeadlockDetection $ atomically $ do
      ma <- getThreadInterface
      either (return . Left) (fmap Right . k) ma
    either throwThreadException return mb
  where
    getThreadInterface :: STM (Either ThreadException (ThreadIface a r'))
    getThreadInterface = do
        st <- STM.readTVar state
        case st of
          ThreadNotStarted   _      -> STM.retry
          ThreadInitializing _ _    -> STM.retry
          ThreadDied         _   e  -> return $ Left e
          ThreadRunning      _ _ a  -> return $ Right $ IfaceAvailable a
          ThreadDone         _ a _  -> return $ Right $ IfaceAvailable a
          ThreadTrivial      _   r' -> return $ Right $ IfaceTrivial r'

-- | Wait for the thread to terminate normally
--
-- If the thread terminated with an exception, this rethrows that exception.
waitForNormalThreadTermination ::
     HasCallStack
  => TVar (ThreadState a r r') -> IO (Either r' r)
waitForNormalThreadTermination state = do
    mErr <- atomically $ waitForNormalOrAbnormalThreadTermination state
    either throwThreadException return mErr

-- | Wait for the thread to terminate normally or abnormally
waitForNormalOrAbnormalThreadTermination ::
     TVar (ThreadState a r r')
  -> STM (Either ThreadException (Either r' r))
waitForNormalOrAbnormalThreadTermination state =
    waitUntilInitialized state >>= \case
      ThreadNotYetRunning_ v -> absurd v
      ThreadRunning_         -> STM.retry
      ThreadDone_ r          -> return $ Right (Right r)
      ThreadTrivial_ r'      -> return $ Right (Left r')
      ThreadException_ e     -> return $ Left e

-- | Run the specified transaction, unless the thread terminated with an
-- exception
unlessAbnormallyTerminated ::
     TVar (ThreadState a r r')
  -> STM b
  -> STM (Either ThreadException b)
unlessAbnormallyTerminated state f =
    waitUntilInitialized state >>= \case
      ThreadNotYetRunning_ v -> absurd v
      ThreadRunning_         -> Right <$> f
      ThreadDone_ _          -> Right <$> f
      ThreadTrivial_ _       -> Right <$> f
      ThreadException_ e     -> return $ Left e

waitUntilInitialized ::
     TVar (ThreadState a r r')
  -> STM (ThreadState_ Void r r')
waitUntilInitialized state = getThreadState_ state STM.retry

-- | An abstraction of 'ThreadState' without the public interface type.
data ThreadState_ notRunning r r' =
      ThreadNotYetRunning_ notRunning
    | ThreadRunning_
    | ThreadDone_ r
    | ThreadTrivial_ r'
    | ThreadException_ ThreadException

getThreadState_ ::
     TVar (ThreadState a r r')
  -> STM notRunning
  -> STM (ThreadState_ notRunning r r')
getThreadState_ state onNotRunning = do
    st <- STM.readTVar state
    case st of
      ThreadNotStarted   _     -> ThreadNotYetRunning_ <$> onNotRunning
      ThreadInitializing _ _   -> ThreadNotYetRunning_ <$> onNotRunning
      ThreadRunning      _ _ _ -> return $ ThreadRunning_
      ThreadDone         _ _ r -> return $ ThreadDone_ r
      ThreadTrivial      _ r'  -> return $ ThreadTrivial_ r'
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
