{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.Dialogue.Execution (
    execGlobalSteps
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.Bifunctor
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Proxy
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import GHC.Stack
import System.Timeout (timeout)

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary

import Test.Driver.ClientServer
import Test.Driver.Dialogue.Definition
import Test.Driver.Dialogue.TestClock (TestClock)
import Test.Driver.Dialogue.TestClock qualified as TestClock
import Test.Util.Within

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

data ReceivedUnexpected = forall a b. (Show a, Show b) => ReceivedUnexpected {
      received   :: a -- The value we received
    , expectInfo :: b -- Some additional info that can help debug the issue
    }

deriving stock instance Show ReceivedUnexpected

expect ::
     (MonadThrow m, Show a, Show info, HasCallStack)
  => info
  -> (a -> Bool)  -- ^ Expected
  -> a            -- ^ Actually received
  -> m ()
expect expectInfo isExpected received
  | isExpected received
  = return ()

  | otherwise
  = throwM $ TestFailure callStack $
      Unexpected $ ReceivedUnexpected{
          received
        , expectInfo
        }

{-------------------------------------------------------------------------------
  Timeouts
-------------------------------------------------------------------------------}

-- | Timeout for waiting for the test clock
timeoutClock :: Int
timeoutClock = 5

-- | Timeout for executing all the actions in a client or handler
timeoutLocal :: Int
timeoutLocal = 20

{-------------------------------------------------------------------------------
  Health
-------------------------------------------------------------------------------}

-- | Health of the peer (server/client)
--
-- When the client is expecting a response from the server, it needs to know the
-- "health" of the server, that is, is the server still alive, or did it fail
-- with some kind exception? The same is true for the server when it expects a
-- response from the client. Therefore, the client interpretation keeps track of
-- the health of the server, and vice versa.
data PeerHealth =
    PeerAlive
  | PeerFailed DeliberateException
  | PeerDisappeared
  deriving stock (Show)

ifPeerAlive :: PeerHealth -> PeerHealth -> PeerHealth
ifPeerAlive PeerAlive  = id
ifPeerAlive peerFailed = const peerFailed

{-------------------------------------------------------------------------------
  Client-side interpretation
-------------------------------------------------------------------------------}

clientLocal ::
     HasCallStack
  => TestClock
  -> Client.Call (BinaryRpc meth srv)
  -> LocalSteps
  -> IO ()
clientLocal clock call = \(LocalSteps steps) ->
    evalStateT (go steps) PeerAlive
  where
    go :: [(TestClock.Tick, LocalStep)] -> StateT PeerHealth IO ()
    go []                     = return ()
    go ((tick, step) : steps) = do
        case step of
          ClientAction action -> do
            within timeoutClock step $ TestClock.waitForTick clock tick
            continue <- clientAct tick action `finally` TestClock.advance clock
            when continue $ go steps
          ServerAction action -> do
            TestClock.giveGreenLight clock tick
            reactToServer action
            go steps

    -- Client action
    --
    -- Returns 'True' if we should continue executing more actions, or
    -- exit (thereby closing the RPC call)
    clientAct ::
         TestClock.Tick
      -> Action (Metadata, RPC) NoMetadata
      -> StateT PeerHealth IO Bool
    clientAct tick action =
        case action of
          Initiate _ ->
            error "clientLocal: unexpected Initiate"
          Send x -> do
            isExpected <- adjustExpectation ()
            expect action isExpected =<<
              liftIO (try $ Client.Binary.sendInput call x)
            return True
          Terminate mException -> do
            -- See discussion in 'TestClock' for why we need to wait here
            TestClock.waitForGreenLight clock tick
            case mException of
              Just exceptionId ->
                throwM $ DeliberateException $ SomeClientException exceptionId
              Nothing ->
                return False

    reactToServer ::
           Action Metadata Metadata
        -> StateT PeerHealth IO ()
    reactToServer action =
        case action of
          Initiate expectedMetadata -> liftIO $ do
            receivedMetadata <- Client.recvResponseMetadata call
            expect action (== expectedMetadata) $ Map.fromList receivedMetadata
          Send (FinalElem a b) -> do
            -- Known bug (limitation in http2). See recvMessageLoop.
            reactToServer $ Send (StreamElem a)
            reactToServer $ Send (NoMoreElems b)
          Send expectedElem -> do
            expected <- adjustExpectation expectedElem
            received <- liftIO . try $
                          fmap (first Map.fromList) $
                            Client.Binary.recvOutput call
            expect action expected received
          Terminate mErr -> do
            reactToServerTermination mErr

    -- See 'reactToClientTermination' for discussion.
    reactToServerTermination ::
         Maybe ExceptionId
      -> StateT PeerHealth IO ()
    reactToServerTermination mErr = do
        liftIO $ void . timeout 5_000_000 $ atomically $ do
          healthy <- Client.isCallHealthy call
          when healthy $ retry
        modify $ ifPeerAlive $
          case mErr of
            Just i  -> PeerFailed $ DeliberateException (SomeServerException i)
            Nothing -> PeerDisappeared

    -- Adjust expectation when communicating with the server
    --
    -- If the server handler died for some reason, we won't get the regular
    -- result, but should instead see the exception reported to the client.
    adjustExpectation :: forall a.
         Eq a
      => a
      -> StateT PeerHealth IO (Either GrpcException a -> Bool)
    adjustExpectation x =
        return . aux =<< get
     where
       aux :: PeerHealth -> Either GrpcException a -> Bool
       aux serverHealth result =
           case serverHealth of
             PeerFailed err ->
               case result of
                 Left (GrpcException GrpcUnknown msg [])
                   | msg == Just (Text.pack $ show err)
                   -> True
                 _otherwise -> False
             PeerDisappeared ->
               case result of
                 Left (GrpcException GrpcUnknown msg [])
                   | msg == Just "HandlerTerminated"
                   -> True
                 _otherwise -> False
             PeerAlive ->
               case result of
                 Right x'   -> x == x'
                 _otherwise -> False

clientGlobal ::
     TestClock
  -> Bool
     -- ^ Use new connection for each RPC call?
     --
     -- Multiple RPC calls on a single connection /ought/ to be independent of
     -- each other, with something going wrong on one should not affect another.
     -- This is currently however not the case, I /think/ due to limitations of
     -- @http2@.
     --
     -- See <https://github.com/well-typed/grapesy/issues/102>.
  -> GlobalSteps
  -> TestClient
clientGlobal clock connPerRPC global connParams testServer delimitTestScope =
    if connPerRPC
      then                  go Nothing  [] (getGlobalSteps global)
      else withConn $ \c -> go (Just c) [] (getGlobalSteps global)
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
        withAsync (within timeoutLocal c $ runLocalSteps mConn c) $ \thread ->
          go mConn (thread:threads) cs

    runLocalSteps :: Maybe Client.Connection -> LocalSteps -> IO ()
    runLocalSteps mConn (LocalSteps steps) = delimitTestScope $ do
        case steps of
          (tick, ClientAction (Initiate (metadata, rpc))) : steps' -> do
            TestClock.waitForTick clock tick

            -- Timeouts are outside the scope of these tests: it's too finicky
            -- to relate timeouts (in seconds) to specific test execution.
            -- We do test exceptions in general here; the specific exception
            -- arising from a timeout we test elsewhere.
            let params :: Client.CallParams
                params = def {
                    Client.callRequestMetadata = Map.toList metadata
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
                  clientLocal clock call (LocalSteps steps')

          _otherwise ->
            error $ "clientGlobal: expected Initiate, got " ++ show steps

{-------------------------------------------------------------------------------
  Server-side interpretation

  The server-side is slightly different, since the infrastructure spawns
  threads on our behalf (one for each incoming RPC).
-------------------------------------------------------------------------------}

serverLocal ::
     TestClock
  -> Server.Call (BinaryRpc serv meth)
  -> LocalSteps -> IO ()
serverLocal clock call = \(LocalSteps steps) -> do
    evalStateT (go steps) PeerAlive
  where
    go :: [(TestClock.Tick, LocalStep)] -> StateT PeerHealth IO ()
    go []                     = return ()
    go ((tick, step) : steps) =
        case step of
          ServerAction action -> do
            within timeoutClock step $ TestClock.waitForTick clock tick
            continue <- serverAct tick action `finally` TestClock.advance clock
            when continue $ go steps
          ClientAction action -> do
            TestClock.giveGreenLight clock tick
            reactToClient action
            go steps

    -- Server action
    --
    -- Returns 'True' if we should continue executing the other actions, or
    -- terminate (thereby terminating the handler)
    serverAct ::
         TestClock.Tick
      -> Action Metadata Metadata
      -> StateT PeerHealth IO Bool
    serverAct tick action =
        case action of
          Initiate metadata -> liftIO $ do
            Server.setResponseMetadata call (Map.toList metadata)
            void $ Server.initiateResponse call
            return True
          Send x -> do
            isExpected <- adjustExpectation ()
            received   <- try . liftIO $
                            Server.Binary.sendOutput call (first Map.toList x)
            expect action isExpected received
            return True
          Terminate mException -> do
            TestClock.waitForGreenLight clock tick
            case mException of
              Just exceptionId ->
                throwM $ DeliberateException $ SomeServerException exceptionId
              Nothing ->
                return False

    reactToClient ::
           Action (Metadata, RPC) NoMetadata
        -> StateT PeerHealth IO ()
    reactToClient action =
        case action of
          Initiate _ ->
            error "serverLocal: unexpected ClientInitiateRequest"
          Send (FinalElem a b) -> do
            -- Known bug (limitation in http2). See recvMessageLoop.
            reactToClient $ Send (StreamElem a)
            reactToClient $ Send (NoMoreElems b)
          Send expectedElem -> do
            isExpected <- adjustExpectation expectedElem
            expect action isExpected =<<
              liftIO (try $ Server.Binary.recvInput call)
          Terminate mErr -> do
            reactToClientTermination mErr

    reactToClientTermination ::
         Maybe ExceptionId
      -> StateT PeerHealth IO ()
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
        modify $ ifPeerAlive $
          case mErr of
            Just i  -> PeerFailed $ DeliberateException (SomeClientException i)
            Nothing -> PeerDisappeared

    -- Adjust expectation when communicating with the client
    adjustExpectation :: forall a.
         Eq a
      => a
      -> StateT PeerHealth IO (Either Server.ClientDisconnected a -> Bool)
    adjustExpectation x =
        return . aux =<< get
     where
       aux :: PeerHealth -> Either Server.ClientDisconnected a -> Bool
       aux clientHealth result =
           case clientHealth of
             PeerFailed _err ->
               case result of
                 Left Server.ClientDisconnected{} -> True
                 _otherwise                       -> False
             PeerDisappeared ->
               case result of
                 Left Server.ClientDisconnected{} -> True
                 _otherwise                       -> False
             PeerAlive ->
               case result of
                 Right x'   -> x == x'
                 _otherwise -> False

serverGlobal ::
     HasCallStack
  => TestClock
  -> MVar GlobalSteps
    -- ^ Unlike in the client case, the grapesy infrastructure spawns a new
    -- thread for each incoming connection. To know which part of the test this
    -- particular handler corresponds to, we take the next 'LocalSteps' from
    -- this @MVar@. Since all requests are started by the client from /one/
    -- thread, the order of these incoming requests is deterministic.
  -> Server.Call (BinaryRpc serv meth)
  -> IO ()
serverGlobal clock globalStepsVar call = do
    steps <- modifyMVar globalStepsVar (getNextSteps . getGlobalSteps)

    -- See discussion in clientGlobal (runLocalSteps)
    TestClock.advance clock

    case getLocalSteps steps of
      (tick, step@(ClientAction (Initiate (metadata, _rpc)))) : steps' -> do
        receivedMetadata <- Server.getRequestMetadata call
        -- It is important that we do this 'expect' outside the scope of the
        -- @modifyMVar@: if we do not, then if the expect fails, we'd leave the
        -- @MVar@ unchanged, and the next request would use the wrong steps.
        expect (tick, step) (== metadata) $ Map.fromList receivedMetadata
        within timeoutLocal steps' $ serverLocal clock call (LocalSteps steps')
      _otherwise ->
         error "serverGlobal: expected ClientInitiateRequest"
  where
    getNextSteps :: [LocalSteps] -> IO (GlobalSteps, LocalSteps)
    getNextSteps [] = do
        throwM $ TestFailure callStack $ UnexpectedRequest
    getNextSteps (LocalSteps steps:global') =
        return (GlobalSteps global', LocalSteps steps)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

execGlobalSteps :: GlobalSteps -> IO ClientServerTest
execGlobalSteps steps = do
    globalStepsVar <- newMVar (order steps)
    clock          <- TestClock.new

    let handler ::
             IsRPC (BinaryRpc serv meth)
          => Proxy (BinaryRpc serv meth) -> Server.RpcHandler IO
        handler rpc = Server.mkRpcHandler rpc $ \call ->
                        serverGlobal clock globalStepsVar call

    return ClientServerTest {
        config = def {
            expectEarlyClientTermination = clientTerminatesEarly
          , expectEarlyServerTermination = serverTerminatesEarly
          }
      , client = clientGlobal clock connPerRPC steps
      , server = [
            handler (Proxy @TestRpc1)
          , handler (Proxy @TestRpc2)
          , handler (Proxy @TestRpc3)
          ]
      }
  where
    clientTerminatesEarly, serverTerminatesEarly :: Bool
    (clientTerminatesEarly, serverTerminatesEarly) = hasEarlyTermination steps

    connPerRPC :: Bool
    connPerRPC = serverTerminatesEarly || clientTerminatesEarly

    -- For 'clientGlobal' the order doesn't matter, because it spawns a thread
    -- for each 'LocalSteps'. The server however doesn't get this option; the
    -- threads /get/ spawnwed for each incoming connection, and must feel off
    -- the appropriate steps. It's therefore important that it will get these
    -- in the order that they come in.
    order :: GlobalSteps -> GlobalSteps
    order (GlobalSteps threads) = GlobalSteps $
        sortBy (comparing firstTick) threads
     where
       firstTick :: LocalSteps -> TestClock.Tick
       firstTick (LocalSteps []) =
           error "execGlobalSteps: unexpected empty LocalSteps"
       firstTick (LocalSteps ((tick, _):_)) =
           tick
