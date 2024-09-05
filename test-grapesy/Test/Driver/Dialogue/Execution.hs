{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Driver.Dialogue.Execution (
    ConnUsage(..)
  , execGlobalSteps
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Proxy
import Data.Text qualified as Text
import GHC.Stack
import GHC.TypeLits
import Network.HTTP2.Client qualified as HTTP2.Client

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
import Test.Util

{-------------------------------------------------------------------------------
  Endpoints
-------------------------------------------------------------------------------}

type TestProtocol meth = RawRpc "dialogue" meth

type instance RequestMetadata          (TestProtocol meth) = TestMetadata
type instance ResponseInitialMetadata  (TestProtocol meth) = TestMetadata
type instance ResponseTrailingMetadata (TestProtocol meth) = TestMetadata

type TestRpc1 = TestProtocol "test1"
type TestRpc2 = TestProtocol "test2"
type TestRpc3 = TestProtocol "test3"

withClientProxy ::
     RPC
  -> (forall meth.
          SupportsClientRpc (TestProtocol meth)
       => Proxy meth
       -> a)
  -> a
withClientProxy RPC1 k = k (Proxy @"test1")
withClientProxy RPC2 k = k (Proxy @"test2")
withClientProxy RPC3 k = k (Proxy @"test3")

{-------------------------------------------------------------------------------
  Test failures
-------------------------------------------------------------------------------}

data TestFailure = TestFailure CallStack Failure
  deriving stock (Show)
  deriving anyclass (Exception)

data Failure =
    -- | Thrown by the server when an unexpected new RPC is initiated
    UnexpectedRequest

    -- | Received an unexpected value
  | Unexpected ReceivedUnexpected
  deriving stock (Show)
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

-- | Timeout for waiting for the green liht
timeoutGreenLight :: Int
timeoutGreenLight = 5

-- | Timeout for executing all the actions in a client or handler
timeoutLocal :: Int
timeoutLocal = 20

-- | Timeout for waiting for a call to fail
timeoutFailure :: Int
timeoutFailure = 5

-- | Timeout for receiving a stream element
timeoutReceive :: Int
timeoutReceive = 5

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

    -- | Peer terminated
    --
    -- The peer might have thrown a deliberate exception, or simply terminated
    -- early without properly closing the connection.
  | PeerTerminated (Maybe DeliberateException)
  deriving stock (Show)

ifPeerAlive :: PeerHealth -> PeerHealth -> PeerHealth
ifPeerAlive PeerAlive             = id
ifPeerAlive (PeerTerminated mErr) = const (PeerTerminated mErr)

{-------------------------------------------------------------------------------
  Client-side interpretation
-------------------------------------------------------------------------------}

clientLocal ::
     HasCallStack
  => TestClock
  -> Client.Call (TestProtocol meth)
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
            reactToServer tick action
            go steps

    -- Client action
    --
    -- Returns 'True' if we should continue executing more actions, or
    -- exit (thereby closing the RPC call)
    clientAct :: TestClock.Tick -> ClientAction -> StateT PeerHealth IO Bool
    clientAct tick action =
        case action of
          Initiate _ ->
            error "clientLocal: unexpected Initiate"
          Send x -> do
            peerHealth <- get
            case peerHealth of
              PeerAlive        -> Client.Binary.sendInput call x
              PeerTerminated _ -> liftIO $ waitForServerDisconnect
            return True
          Terminate mException -> do
            -- See discussion in 'TestClock' for why we need to wait here
            peerHealth <- get
            case peerHealth of
              PeerTerminated _ -> return ()
              PeerAlive        -> within timeoutGreenLight action $
                                    TestClock.waitForGreenLight clock tick
            case mException of
              Just ex -> throwM $ DeliberateException ex
              Nothing -> return False

    reactToServer :: TestClock.Tick -> ServerAction -> StateT PeerHealth IO ()
    reactToServer tick action =
        case action of
          Initiate expectedMetadata -> liftIO $ do
            receivedMetadata <- within timeoutReceive action $
                                  Client.recvResponseInitialMetadata call
            expect (tick, action) (== ResponseInitialMetadata expectedMetadata) $
              receivedMetadata
          Send (FinalElem a b) -> do
            -- On the client side, when the server sends the final message, we
            -- will receive that final message in one HTTP data frame, and then
            -- the trailers in another. This means that when we get the message,
            -- we do not yet know if this is in fact the last.
            -- (This is different on the server side, because gRPC does not
            -- support trailers on the client side.)
            reactToServer tick $ Send (StreamElem a)
            reactToServer tick $ Send (NoMoreElems b)
          Send expectedElem -> do
            mOut <- try $ within timeoutReceive action $
                      Client.Binary.recvOutput call
            expect (tick, action) (isExpectedElem expectedElem) mOut
          Terminate mErr -> do
            mOut <- try $ within timeoutReceive action $
                      Client.Binary.recvOutput call
            let mErr'       = DeliberateException <$> mErr
                expectation = isGrpcException mErr'
            expect (tick, action) expectation mOut
            modify $ ifPeerAlive $ PeerTerminated mErr'

    -- Wait for the server disconnect to become visible
    --
    -- In principle we could check if we can still /receive/ messages from the
    -- server to see if we can /send/ messages to the server: gRPC does not
    -- allow the server to half-close the connection (only the client). For
    -- consistency, however, we simply wait until sending fails.
    --
    -- See 'waitForClientDisconnect' for additional discussion.
    waitForServerDisconnect :: IO ()
    waitForServerDisconnect =
        within timeoutFailure () $ loop
      where
        loop :: IO ()
        -- We only do this when we know the client has terminated, so the
        -- /type/ of the message we send here as a probe does not matter.
        loop = do
            mFailed <- try $ Client.Binary.sendNextInput call ()
            case mFailed of
              Left (_ :: GrpcException) ->
                return ()
              Right () -> do
                threadDelay 10_000
                loop

    isExpectedElem ::
         StreamElem TestMetadata Int
      -> Either GrpcException (StreamElem TestMetadata Int)
      -> Bool
    isExpectedElem _ (Left _) = False
    isExpectedElem expectedElem (Right streamElem) = expectedElem == streamElem

    isGrpcException ::
         Maybe DeliberateException
      -> Either GrpcException (StreamElem TestMetadata Int)
      -> Bool
    isGrpcException mErr (Left err) = and [
          grpcError        err == GrpcUnknown
        , grpcErrorMessage err == Just (case mErr of
                                    Nothing   -> "HandlerTerminated"
                                    Just err' -> Text.pack $ show err')
        ]
    isGrpcException _ (Right _) = False

clientGlobal ::
     TestClock
  -> ConnUsage
     -- ^ Use new connection for each RPC call?
     --
     -- Multiple RPC calls on a single connection /ought/ to be independent of
     -- each other. Something going wrong on one should not affect another.
  -> GlobalSteps
  -> TestClient
clientGlobal clock connUsage global connParams testServer delimitTestScope =
    case connUsage of
      ConnPerRPC ->                  go Nothing  [] (getGlobalSteps global)
      SharedConn -> withConn $ \c -> go (Just c) [] (getGlobalSteps global)
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

            withClientProxy rpc $ startCall mConn metadata steps'
          _otherwise ->
            error $ "clientGlobal: expected Initiate, got " ++ show steps

    startCall :: forall (meth :: Symbol).
         SupportsClientRpc (TestProtocol meth)
      => Maybe Client.Connection
      -> TestMetadata
      -> [(TestClock.Tick, LocalStep)]
      -> Proxy meth -> IO ()
    startCall mConn metadata steps' _ = do
        (case mConn of
            Just conn -> ($ conn)
            Nothing   -> withConn) $ \conn ->
          Client.withRPC conn params (Proxy @(TestProtocol meth)) $ \call -> do
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
     where
       -- Timeouts are outside the scope of these tests: it's too finicky
       -- to relate timeouts (in seconds) to specific test execution. We
       -- do test exceptions in general here; the specific exception
       -- arising from a timeout we test elsewhere.
       params = def {
           Client.callRequestMetadata = metadata
         }

{-------------------------------------------------------------------------------
  Server-side interpretation

  The server-side is slightly different, since the infrastructure spawns
  threads on our behalf (one for each incoming RPC).
-------------------------------------------------------------------------------}

serverLocal ::
     TestClock
  -> Server.Call (TestProtocol meth)
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
            reactToClient tick action
            go steps

    -- Server action
    --
    -- Returns 'True' if we should continue executing the other actions, or
    -- terminate (thereby terminating the handler)
    serverAct :: TestClock.Tick -> ServerAction -> StateT PeerHealth IO Bool
    serverAct tick action =
        case action of
          Initiate metadata -> liftIO $ do
            Server.setResponseInitialMetadata call metadata
            void $ Server.initiateResponse call
            return True
          Send x -> do
            peerHealth <- get
            case peerHealth of
              PeerAlive        -> liftIO $ Server.Binary.sendOutput call x
              PeerTerminated _ -> liftIO $ waitForClientDisconnect
            return True
          Terminate mException -> do
            peerHealth <- get
            case peerHealth of
              PeerTerminated _ -> return ()
              PeerAlive        -> within timeoutGreenLight action $
                                    TestClock.waitForGreenLight clock tick
            case mException of
              Just ex -> throwM $ DeliberateException ex
              Nothing -> return False

    reactToClient :: TestClock.Tick -> ClientAction -> StateT PeerHealth IO ()
    reactToClient tick action =
        case action of
          Initiate _ ->
            error "serverLocal: unexpected ClientInitiateRequest"
          Send expectedElem -> do
            mInp <- liftIO $ try $ within timeoutReceive action $
                      Server.Binary.recvInput call
            expect (tick, action) (isExpectedElem expectedElem) mInp
          Terminate mErr -> do
            mInp <- liftIO $ try $ within timeoutReceive action $
                      Server.Binary.recvInput call
            expect (tick, action) isClientDisconnected mInp
            modify $ ifPeerAlive $ PeerTerminated $ DeliberateException <$> mErr

    -- Wait for the client disconnect to become visible
    --
    -- The only way to know that we cannot send messages anymore to a client
    -- that has terminated is by trying. Although the /receiving/ thread may
    -- terminate more-or-less immediately, this does not necessarily indicate
    -- any kind of failure: the client may simply have put the call in
    -- half-closed mode.
    waitForClientDisconnect :: IO ()
    waitForClientDisconnect =
        within timeoutFailure () $ loop
      where
        loop :: IO ()
        -- We only do this when we know the client has terminated, so the
        -- /type/ of the message we send here as a probe does not matter.
        loop = do
            mFailed <- try $ Server.Binary.sendNextOutput call ()
            case mFailed of
              Left (_ :: Server.ClientDisconnected) ->
                return ()
              Right () -> do
                threadDelay 10_000
                loop

    isExpectedElem ::
         StreamElem NoMetadata Int
      -> Either Server.ClientDisconnected (StreamElem NoMetadata Int)
      -> Bool
    isExpectedElem _ (Left _) = False
    isExpectedElem expectedElem (Right streamElem) = expectedElem == streamElem

    isClientDisconnected ::
         Either Server.ClientDisconnected (StreamElem NoMetadata Int)
      -> Bool
    isClientDisconnected (Left (Server.ClientDisconnected e _))
      | Just HTTP2.Client.ConnectionIsClosed <- fromException e
      = True
      | otherwise
      = False
    isClientDisconnected _ = False

serverGlobal ::
     HasCallStack
  => TestClock
  -> MVar GlobalSteps
    -- ^ Unlike in the client case, the grapesy infrastructure spawns a new
    -- thread for each incoming connection. To know which part of the test this
    -- particular handler corresponds to, we take the next 'LocalSteps' from
    -- this @MVar@. Since all requests are started by the client from /one/
    -- thread, the order of these incoming requests is deterministic.
  -> Server.Call (TestProtocol meth)
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
        expect (tick, step) (== metadata) $ receivedMetadata
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

data ConnUsage = SharedConn | ConnPerRPC

execGlobalSteps :: ConnUsage -> GlobalSteps -> IO ClientServerTest
execGlobalSteps connUsage steps = do
    globalStepsVar <- newMVar (order steps)
    clock          <- TestClock.new

    let handler :: forall (meth :: Symbol).
             SupportsServerRpc (TestProtocol meth)
          => Proxy (TestProtocol meth)
          -> Server.SomeRpcHandler IO
        handler _ = Server.someRpcHandler $
          Server.mkRpcHandler @(TestProtocol meth) $ \call ->
            serverGlobal clock globalStepsVar call

    return ClientServerTest {
        config = def {
            expectEarlyClientTermination = clientTerminatesEarly
          , expectEarlyServerTermination = serverTerminatesEarly
          }
      , client = clientGlobal clock connUsage steps
      , server = [
            handler (Proxy @TestRpc1)
          , handler (Proxy @TestRpc2)
          , handler (Proxy @TestRpc3)
          ]
      }
  where
    clientTerminatesEarly, serverTerminatesEarly :: Bool
    (clientTerminatesEarly, serverTerminatesEarly) = hasEarlyTermination steps

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
