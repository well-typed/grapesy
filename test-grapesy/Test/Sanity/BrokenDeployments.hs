module Test.Sanity.BrokenDeployments (tests) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as HTTP2
import Network.Run.TCP qualified as NetworkRun
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.API.Ping
import Network.Socket

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.BrokenDeployments" [
      testCase "non200" test_non200
    ]

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Test HTTP to gRPC status code mapping
--
-- We don't test all codes here; we'd just end up duplicating the logic in
-- 'classifyServerResponse'. We just check one representative value.
test_non200 :: Assertion
test_non200 = do
    serverPort <- newEmptyMVar
    withAsync (respondWith HTTP.badRequest400 serverPort) $ \_server -> do
      port <- readMVar serverPort
      let addr :: Client.Address
          addr = Client.Address {
                addressHost      = "127.0.0.1"
              , addressPort      = port
              , addressAuthority = Nothing
              }
      mResp :: Either GrpcException (Proto PongMessage) <- try $
        Client.withConnection def (Client.ServerInsecure addr) $ \conn ->
          Client.withRPC conn def (Proxy @Ping) $ \call -> do
            Client.sendFinalInput call defMessage
            fst <$> Client.recvFinalOutput call
      case mResp of
        Left err | grpcError err == GrpcInternal ->
          return ()
        _otherwise ->
          assertFailure $ "Unexpected response: " ++ show mResp

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Server that always responses with given status to every request
respondWith :: HTTP.Status -> MVar PortNumber -> IO ()
respondWith status = testServer $ \_req _aux respond ->
    respond (HTTP2.responseNoBody status []) []

-- | Low-level test server
--
-- We bypass the entire grapesy machinery for constructing the server, because
-- we need to mock a broken deployment.
--
-- The grapesy client can auto reconnect when the server is not (yet) up and
-- running, but to keep things simple, and since the server anyway runs in the
-- same process, we just signal when the server is ready. This also allows us
-- to avoid binding to a specific port in the tests (which might already be in
-- use on the machine running the tests, leading to spurious test failures).
testServer :: HTTP2.Server -> MVar PortNumber -> IO ()
testServer server serverPort = do
    addr <- NetworkRun.resolve Stream (Just "127.0.0.1") "0" [AI_PASSIVE]
    bracket (NetworkRun.openTCPServerSocket addr) close $ \listenSock -> do
      addr' <- getSocketName listenSock
      port  <- case addr' of
                 SockAddrInet  port   _host   -> return port
                 SockAddrInet6 port _ _host _ -> return port
                 SockAddrUnix{} -> error "respondWith: unexpected unix socket"
      putMVar serverPort port
      NetworkRun.runTCPServerWithSocket listenSock $ \clientSock ->
        bracket (HTTP2.allocSimpleConfig clientSock 4096)
                HTTP2.freeSimpleConfig $ \config ->
          HTTP2.run HTTP2.defaultServerConfig config server
