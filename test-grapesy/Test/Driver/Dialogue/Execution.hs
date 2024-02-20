{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.Dialogue.Execution (
    execGlobalSteps
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.Bifunctor
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Proxy
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary

import Debug.Concurrent

import Test.Driver.ClientServer
import Test.Driver.Dialogue.Definition
import Test.Driver.Dialogue.TestClock

{-------------------------------------------------------------------------------
  Endpoints
-------------------------------------------------------------------------------}

type TestRpc1 = BinaryRpc "dialogue" "test1"
type TestRpc2 = BinaryRpc "dialogue" "test2"
type TestRpc3 = BinaryRpc "dialogue" "test3"

withProxy ::
     RPC
  -> (forall serv meth.
          IsRPC (BinaryRpc serv meth)
       => Proxy (BinaryRpc serv meth)
       -> a)
  -> a
withProxy RPC1 k = k (Proxy @TestRpc1)
withProxy RPC2 k = k (Proxy @TestRpc2)
withProxy RPC3 k = k (Proxy @TestRpc3)

{-------------------------------------------------------------------------------
  Test failures
-------------------------------------------------------------------------------}

data TestFailure = TestFailure CallStack Failure
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Exception)

data Failure =
    -- | Thrown by the server when an unexpected new RPC is initiated
    UnexpectedRequest

    -- | Received an unexpected value
  | Unexpected ReceivedUnexpected
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Exception)

data ReceivedUnexpected = forall a. Show a => ReceivedUnexpected {
      received :: a
    }

deriving stock instance Show ReceivedUnexpected

expect ::
     (MonadThrow m, Show a, HasCallStack)
  => (a -> Bool)  -- ^ Expected
  -> a            -- ^ Actually received
  -> m ()
expect isExpected received
  | isExpected received
  = return ()

  | otherwise
  = throwM $ TestFailure callStack $
      Unexpected $ ReceivedUnexpected{received}

{-------------------------------------------------------------------------------
  Health
-------------------------------------------------------------------------------}

-- | Health
--
-- When the client is expecting a response from the server, it needs to know the
-- "health" of the server, that is, is the server still alive, or did it fail
-- with some kind exception? The same is true for the server when it expects a
-- response from the client. Therefore, the client interpretation keeps track of
-- the health of the server, and vice versa.
data Health e a = Alive a | Failed e | Disappeared
  deriving stock (Show)

type ServerHealth = Health SomeServerException
type ClientHealth = Health SomeClientException

instance Functor (Health e) where
  fmap  = liftM
instance Applicative (Health e) where
  pure  = Alive
  (<*>) = ap
instance Monad (Health e) where
  return = pure
  Alive x     >>= f = f x
  Failed e    >>= _ = Failed e
  Disappeared >>= _ = Disappeared

ifAlive :: (a -> Health e b) -> Health e a -> Health e b
ifAlive = (=<<)

{-------------------------------------------------------------------------------
  Execution mode
-------------------------------------------------------------------------------}

-- | Execution mode
--
-- Each test involves one or more pairs of a client and a server engaged in an
-- RPC call. Each call consists of a series of actions ("client sends initial
-- metadata", "client sends a message", "server sends a message", etc.). We
-- categorize such actions as either " passive " (such as receiving a message)
-- or " active " (such as sending a message).
--
-- As part of the test generation, each action is assigned a (test) clock tick.
-- This imposes a specific (but randomly generated) ordering; this matches the
-- formal definition of the behaviour of concurrent systems: "for every
-- interleaving, ...". Active actions wait until the test clock (essentially
-- such an @MVar Int@) reaches their assigned tick before proceeding. The
-- /advancement/ of the test clock depends on the mode (see below).
data ExecutionMode =
    -- | Conservative mode (used when tests involve early termination)
    --
    -- == Ordering
    --
    -- Consider a test that looks something like:
    --
    -- > clock tick 1: client sends message
    -- > clock tick 2: client throws exception
    --
    -- We have to be careful to preserve ordering here: the exception thrown by
    -- the client may or may not " overtake " the earlier send: the message may
    -- or may not be send to the server, thereby making the tests flaky. In
    -- conversative mode, therefore, the /passive/ participant is the one that
    -- advances the test clock; in the example above, the /server/ advances the
    -- test clock when it receives the message. The downside of this approach
    -- is that we are excluding some valid behaviour from the tests: we're
    -- effectively making every operation synchronous.
    --
    -- == Connection isolation
    --
    -- The tests assume that different calls are independent from each other.
    -- This is mostly true, but not completely: when a client or a server
    -- terminates early, the entire connection (supporting potentially many
    -- calls) is reset. It's not entirely clear why; it feels like an
    -- unnecessary limitation in @http2@. (TODO: Actually, this might be related
    -- to an exception being thrown in 'outboundTrailersMaker'?)
    --
    -- Ideally, we would either (1) randomly assign connections to calls and
    -- then test that an early termination only affects calls using the same
    -- connection, or better yet, (2), remove this limitation from @http2@.
    --
    -- For now, we do neither: /if/ a test includes early termination, we give
    -- each call its own connection, thereby regaining independence.
    Conservative

    -- | Aggressive mode (used when tests do not involve early termination)
    --
    -- == Ordering
    --
    -- Consider a test containing:
    --
    -- > clock tick 1: client sends message A
    -- > clock tick 2: client sends message B
    --
    -- In aggressive mode it's the /active/ participant that advances the clock.
    -- This means that we may well reach clock tick 2 before the server has
    -- received message A, thus testing the asynchronous nature of the
    -- operations that @grapesy@ offers
    --
    -- ## Connection isolation
    --
    -- In aggressive mode all calls from the client to the server share the
    -- same connection.
  | Aggressive
  deriving stock (Show, Eq)

ifConservative :: Applicative m => ExecutionMode -> m () -> m ()
ifConservative Conservative k = k
ifConservative Aggressive   _ = pure ()

ifAggressive :: Applicative m => ExecutionMode -> m () -> m ()
ifAggressive Aggressive   k = k
ifAggressive Conservative _ = pure ()

-- | Advance the clock for all non-executed steps
--
-- When a client or a handler exits (due to an exception, perhaps), then it is
-- important we still step the test clock at the appropriate times, to avoid the
-- rest of the tests stalling.
skipMissedSteps ::
     TestClock
  -> ExecutionMode
  -> (ExecutionMode -> LocalStep -> Bool)
  -> [(TestClockTick, LocalStep)]
  -> IO ()
skipMissedSteps testClock mode ourStep steps =
    void $ forkIO $
      advanceTestClockAtTimes testClock $
        map fst $ filter (ourStep mode . snd) steps

{-------------------------------------------------------------------------------
  Client-side interpretation
-------------------------------------------------------------------------------}

clientLocal ::
     HasCallStack
  => TestClock
  -> ExecutionMode
  -> Client.Call (BinaryRpc meth srv)
  -> LocalSteps
  -> IO ()
clientLocal testClock mode call = \(LocalSteps steps) ->
    evalStateT (go steps) (Alive ()) `finally`
      skipMissedSteps testClock mode ourStep steps
  where
    ourStep :: ExecutionMode -> LocalStep -> Bool
    ourStep Aggressive   (ClientAction _) = True
    ourStep Aggressive   (ServerAction _) = False
    ourStep Conservative (ClientAction _) = False
    ourStep Conservative (ServerAction _) = True

    go :: [(TestClockTick, LocalStep)] -> StateT (ServerHealth ()) IO ()
    go []                     = return ()
    go ((tick, step) : steps) = do
        waitFor "client"
        case step of
          ClientAction action -> do
            _reachedTick <- liftIO $ waitForTestClockTick testClock tick
            -- TODO: We could assert that _reachedTick is True
            continue <-
              clientAct action `finally`
                liftIO (ifAggressive mode $ advanceTestClock testClock)
            when continue $ go steps
          ServerAction action -> do
            reactToServer action `finally`
              liftIO (ifConservative mode $ advanceTestClock testClock)
            go steps

    -- Client action
    --
    -- Returns 'True' if we should continue executing more actions, or
    -- exit (thereby closing the RPC call)
    clientAct ::
         Action (Metadata, RPC) NoMetadata
      -> StateT (ServerHealth ()) IO Bool
    clientAct = \case
        Initiate _ ->
          error "clientLocal: unexpected Initiate"
        Send x -> do
          isExpected <- adjustExpectation ()
          expect isExpected =<< liftIO (try $ Client.Binary.sendInput call x)
          return True
        Terminate (Just exceptionId) -> do
          throwM $ DeliberateException $ SomeClientException exceptionId
        Terminate Nothing ->
          return False
        SleepMilli n -> do
          liftIO $ threadDelay (n * 1_000)
          return True

    reactToServer ::
           Action Metadata Metadata
        -> StateT (ServerHealth ()) IO ()
    reactToServer = \case
        Initiate expectedMetadata -> liftIO $ do
          receivedMetadata <- Client.recvResponseMetadata call
          expect (== expectedMetadata) $ Set.fromList receivedMetadata
        Send (FinalElem a b) -> do
          -- Known bug (limitation in http2). See recvMessageLoop.
          reactToServer $ Send (StreamElem a)
          reactToServer $ Send (NoMoreElems b)
        Send expectedElem -> do
          expected <- adjustExpectation expectedElem
          received <- liftIO . try $
                        fmap (first Set.fromList) $
                          Client.Binary.recvOutput call
          expect expected received
        Terminate mErr -> do
          reactToServerTermination mErr
        SleepMilli _ ->
          return ()

    -- See 'reactToClientTermination' for discussion.
    reactToServerTermination ::
         Maybe ExceptionId
      -> StateT (ServerHealth ()) IO ()
    reactToServerTermination mErr = do
        liftIO $ void . timeout 5_000_000 $ atomically $ do
          healthy <- Client.isCallHealthy call
          when healthy $ retry
        modify $ ifAlive $ \() ->
          case mErr of
            Just i  -> Failed $ SomeServerException i
            Nothing -> Disappeared

    -- Adjust expectation when communicating with the server
    --
    -- If the server handler died for some reason, we won't get the regular
    -- result, but should instead see the exception reported to the client.
    adjustExpectation :: forall a.
         Eq a
      => a
      -> StateT (ServerHealth ()) IO (Either GrpcException a -> Bool)
    adjustExpectation x =
        return . aux =<< get
     where
       aux :: ServerHealth () -> Either GrpcException a -> Bool
       aux serverHealth result =
           case serverHealth of
             Failed err ->
               case result of
                 Left (GrpcException GrpcUnknown msg [])
                   | msg == Just (Text.pack $ show err)
                   -> True
                 _otherwise -> False
             Disappeared ->
               -- TODO: Not really sure what exception we get here actually
               case result of
                 Left (GrpcException GrpcUnknown msg [])
                   | msg == Just "TODO"
                   -> True
                 _otherwise -> False
             Alive () ->
               case result of
                 Right x'   -> x == x'
                 _otherwise -> False

clientGlobal ::
     TestClock
  -> ExecutionMode
  -> GlobalSteps
  -> TestClient
clientGlobal testClock mode global connParams testServer delimitTestScope =
    case mode of
      Aggressive   -> withConn $ \c -> go (Just c) [] (getGlobalSteps global)
      Conservative ->                  go Nothing  [] (getGlobalSteps global)
  where
    withConn :: (Client.Connection -> IO ()) -> IO ()
    withConn = Client.withConnection connParams testServer

    go :: Maybe Client.Connection -> [Async ()] -> [LocalSteps] -> IO ()
    go _ threads [] = do
        -- Wait for all threads to finish
        --
        -- This also ensures that if any of these threads threw an exception,
        -- that is now rethrown here in the main test. This will also cause us
        -- to leave the scope of all enclosing calls to @withAsync@, thereby
        -- cancelling all other concurrent threads.
        --
        -- (It is therefore important that we catch any /excepted/ exceptions
        -- locally; this is done by the call to @delimitTestScope@.)
        mapM_ wait threads
    go mConn threads (c:cs) =
        withAsync (runLocalSteps mConn c) $ \newThread ->
          go mConn (newThread:threads) cs

    runLocalSteps :: Maybe Client.Connection -> LocalSteps -> IO ()
    runLocalSteps mConn (LocalSteps steps) = delimitTestScope $ do
        case steps of
          (tick, ClientAction (Initiate (metadata, rpc))) : steps' -> do
            _reachedTick <- waitForTestClockTick testClock tick
            -- TODO: We could assert that _reachedTick is True

            -- Timeouts are outside the scope of these tests: it's too finicky
            -- to relate timeouts (in seconds) to specific test execution.
            -- We do test exceptions in general here; the specific exception
            -- arising from a timeout we test elsewhere.
            let params :: Client.CallParams
                params = def {
                    Client.callRequestMetadata = Set.toList metadata
                  }

            withProxy rpc $ \proxy ->
              (case mConn of
                  Just conn -> ($ conn)
                  Nothing   -> withConn) $ \conn ->
                Client.withRPC conn params proxy $ \call -> do
                  -- We wait for the /server/ to advance the test clock (so that
                  -- we are sure the next step doesn't happen until the
                  -- connection is established).
                  --
                  -- NOTE: We could instead wait for the server to send the
                  -- initial metadata; this too would provide evidence that the
                  -- connection has been established. However, doing so
                  -- precludes a class of correct behaviour: the server might
                  -- not respond with that initial metadata until the client has
                  -- sent some messages.
                  clientLocal testClock mode call (LocalSteps steps')

          _otherwise ->
            error $ "clientGlobal: expected Initiate, got " ++ show steps

{-------------------------------------------------------------------------------
  Debugging: control who is allowed to take a step
-------------------------------------------------------------------------------}

promptLock :: MVar ()
{-# NOINLINE promptLock #-}
promptLock = unsafePerformIO $ newMVar ()

promptVar :: TVar [String]
{-# NOINLINE promptVar #-}
promptVar = unsafePerformIO $ newTVarIO []

waitFor :: MonadIO m => String -> m ()
-- waitFor = _waitForEnabled
waitFor _ = return ()

_waitForEnabled :: MonadIO m => String -> m ()
_waitForEnabled label = liftIO $ do
    -- We spawn a prompt for each call to 'waitFor': @n@ calls to 'waitFor' will
    -- need @n@ prompts. The order doesn't matter, each prompt is equivalent to
    -- every other, but we don't want multiple prompts at once, so we require
    -- that we hold the 'promptLock'.
    _ <- forkIO $ do
      line <- withMVar promptLock $ \() -> do
        let loop :: IO String
            loop = do
                str <- getLine
                if null str
                  then loop
                  else return str
        loop
      atomically $ modifyTVar promptVar (line :)
    -- Only consume our own prompt
    putStrLn $ "Waiting for " ++ show label
    atomically $ do
      prompts <- readTVar promptVar
      case break (== label) prompts of
        (_ , []   ) -> retry
        (ps, _:ps') -> writeTVar promptVar (ps ++ ps')
    putStrLn $ "Got " ++ show label

{-------------------------------------------------------------------------------
  Server-side interpretation

  The server-side is slightly different, since the infrastructure spawns
  threads on our behalf (one for each incoming RPC).
-------------------------------------------------------------------------------}

serverLocal ::
     TestClock
  -> ExecutionMode
  -> Server.Call (BinaryRpc serv meth)
  -> LocalSteps -> IO ()
serverLocal testClock mode call = \(LocalSteps steps) -> do
    evalStateT (go steps) (Alive ()) `finally`
      skipMissedSteps testClock mode ourStep steps
  where
    ourStep :: ExecutionMode -> LocalStep -> Bool
    ourStep Aggressive   (ServerAction _) = True
    ourStep Aggressive   (ClientAction _) = False
    ourStep Conservative (ClientAction _) = False
    ourStep Conservative (ServerAction _) = True

    go :: [(TestClockTick, LocalStep)] -> StateT (ClientHealth ()) IO ()
    go []                     = return ()
    go ((tick, step) : steps) = do
        waitFor "server"
        case step of
          ServerAction action -> do
            _reachedTick <- liftIO $ waitForTestClockTick testClock tick
            -- TODO: We could assert that _reachedTick is True
            continue <-
              serverAct action `finally`
                liftIO (ifAggressive mode $ advanceTestClock testClock)
            when continue $ go steps
          ClientAction action -> do
            reactToClient action `finally`
              liftIO (ifConservative mode $ advanceTestClock testClock)
            go steps

    -- Server action
    --
    -- Returns 'True' if we should continue executing the other actions, or
    -- terminate (thereby terminating the handler)
    serverAct ::
         Action Metadata Metadata
      -> StateT (ClientHealth ()) IO Bool
    serverAct = \case
        Initiate metadata -> liftIO $ do
          Server.setResponseMetadata call (Set.toList metadata)
          void $ Server.initiateResponse call
          return True
        Send x -> do
          isExpected <- adjustExpectation ()
          received   <- try . liftIO $
                          Server.Binary.sendOutput call (first Set.toList x)
          expect isExpected received
          return True
        Terminate (Just exceptionId) -> do
          throwM $ DeliberateException $ SomeServerException exceptionId
        Terminate Nothing ->
          return False
        SleepMilli n -> do
          liftIO $ threadDelay (n * 1_000)
          return True

    reactToClient ::
           Action (Metadata, RPC) NoMetadata
        -> StateT (ClientHealth ()) IO ()
    reactToClient = \case
        Initiate _ ->
          error "serverLocal: unexpected ClientInitiateRequest"
        Send (FinalElem a b) -> do
          -- Known bug (limitation in http2). See recvMessageLoop.
          reactToClient $ Send (StreamElem a)
          reactToClient $ Send (NoMoreElems b)
        Send expectedElem -> do
          isExpected <- adjustExpectation expectedElem
          expect isExpected =<< liftIO (try $ Server.Binary.recvInput call)
        Terminate mErr -> do
          reactToClientTermination mErr
        SleepMilli _ ->
          return ()

    reactToClientTermination ::
         Maybe ExceptionId
      -> StateT (ClientHealth ()) IO ()
    reactToClientTermination mErr = do
        -- Wait for the client-side exception to become noticable server-side.
        -- Under normal circumstances the server will only notice this when
        -- trying to communicate; by explicitly checking we avoid
        -- non-determinism in the test (where the exception may or may not
        -- already have been come visible server-side).
        liftIO $ void . timeout 5_000_000 $ atomically $ do
          healthy <- Server.isCallHealthy call
          when healthy $ retry

        -- Update client health, /if/ the client was alive at this point
        modify $ ifAlive $ \() ->
          case mErr of
            Just i  -> Failed $ SomeClientException i
            Nothing -> Disappeared

    -- Adjust expectation when communicating with the client
    adjustExpectation :: forall a.
         Eq a
      => a
      -> StateT (ClientHealth ()) IO (Either Server.ClientDisconnected a -> Bool)
    adjustExpectation x =
        return . aux =<< get
     where
       aux :: ClientHealth () -> Either Server.ClientDisconnected a -> Bool
       aux clientHealth result =
           case clientHealth of
             Failed _err ->
               case result of
                 Left (Server.ClientDisconnected _) -> True
                 _otherwise                         -> False
             Disappeared ->
               case result of
                 Left (Server.ClientDisconnected _) -> True
                 _otherwise                         -> False
             Alive () ->
               case result of
                 Right x'   -> x == x'
                 _otherwise -> False

serverGlobal ::
     HasCallStack
  => TestClock
  -> ExecutionMode
  -> MVar GlobalSteps
    -- ^ Unlike in the client case, the grapesy infrastructure spawns a new
    -- thread for each incoming connection. To know which part of the test this
    -- particular handler corresponds to, we take the next 'LocalSteps' from
    -- this @MVar@. Since all requests are started by the client from /one/
    -- thread, the order of these incoming requests is deterministic.
  -> Server.Call (BinaryRpc serv meth)
  -> IO ()
serverGlobal testClock mode globalStepsVar call = do
    steps <- modifyMVar globalStepsVar (getNextSteps . getGlobalSteps)
    -- See discussion in clientGlobal (runLocalSteps)
    advanceTestClock testClock

    handle (annotate steps) $ do
     case getLocalSteps steps of
       -- It is important that we do this 'expect' outside the scope of the
       -- @modifyMVar@: if we do not, then if the expect fails, we'd leave the
       -- @MVar@ unchanged, and the next request would use the wrong steps.
       (_tick, ClientAction (Initiate (metadata, _rpc))) : steps' -> do
         -- We don't care about the timeout the client sets; if the server
         -- takes too long, the /client/ will check that it gets the expected
         -- exception.
         receivedMetadata <- Server.getRequestMetadata call
         expect (== metadata) $ Set.fromList receivedMetadata
         serverLocal testClock mode call $ LocalSteps steps'
       _otherwise ->
          error "serverGlobal: expected ClientInitiateRequest"
  where
    getNextSteps :: [LocalSteps] -> IO (GlobalSteps, LocalSteps)
    getNextSteps [] = do
        throwM $ TestFailure callStack $ UnexpectedRequest
    getNextSteps (LocalSteps steps:global') =
        return (GlobalSteps global', LocalSteps steps)

    annotate :: LocalSteps -> SomeException -> IO ()
    annotate steps err = throwM $ AnnotatedServerException {
          serverGlobalException          = err
        , serverGlobalExceptionSteps     = steps
        , serverGlobalExceptionCallStack = callStack
        }

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

execGlobalSteps :: GlobalSteps -> IO ClientServerTest
execGlobalSteps steps = do
    globalStepsVar <- newMVar (order steps)
    testClock      <- newTestClock

    let handler ::
             IsRPC (BinaryRpc serv meth)
          => Proxy (BinaryRpc serv meth) -> Server.RpcHandler IO
        handler rpc = Server.mkRpcHandler rpc $ \call ->
                        serverGlobal testClock mode globalStepsVar call

    return ClientServerTest {
        config = def {
            expectEarlyClientTermination = clientTerminatesEarly
          , expectEarlyServerTermination = serverTerminatesEarly
          }
      , client = clientGlobal testClock mode steps
      , server = [
            handler (Proxy @TestRpc1)
          , handler (Proxy @TestRpc2)
          , handler (Proxy @TestRpc3)
          ]
      }
  where
    clientTerminatesEarly, serverTerminatesEarly :: Bool
    (clientTerminatesEarly, serverTerminatesEarly) = hasEarlyTermination steps

    mode :: ExecutionMode
    mode =
        if serverTerminatesEarly || clientTerminatesEarly
          then Conservative
          else Aggressive

    -- For 'clientGlobal' the order doesn't matter, because it spawns a thread
    -- for each 'LocalSteps'. The server however doesn't get this option; the
    -- threads /get/ spawnwed for each incoming connection, and must feel off
    -- the appropriate steps. It's therefore important that it will get these
    -- in the order that they come in.
    order :: GlobalSteps -> GlobalSteps
    order (GlobalSteps threads) = GlobalSteps $
        sortBy (comparing firstTick) threads
     where
       firstTick :: LocalSteps -> TestClockTick
       firstTick (LocalSteps []) =
           error "execGlobalSteps: unexpected empty LocalSteps"
       firstTick (LocalSteps ((tick, _):_)) =
           tick
