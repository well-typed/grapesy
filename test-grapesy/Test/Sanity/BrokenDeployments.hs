{-# LANGUAGE OverloadedStrings #-}

module Test.Sanity.BrokenDeployments (tests) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Data.ByteString.Builder qualified as ByteString (Builder)
import Data.Text qualified as Text
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as HTTP2
import Network.Run.TCP qualified as NetworkRun
import Network.Socket
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.API.Ping

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.BrokenDeployments" [
      testGroup "status" [
          testCase "non200"     test_statusNon200
        , testCase "non200Body" test_statusNon200Body
        ]
    , testGroup "ContentType" [
          testCase "nonGrpcRegular"      test_nonGrpcContentTypeRegular
        , testCase "missingRegular"      test_missingContentTypeRegular
        , testCase "nonGrpcTrailersOnly" test_nonGrpcContentTypeTrailersOnly
        , testCase "missingTrailersOnly" test_missingContentTypeTrailersOnly
      ]
    , testGroup "Omit" [
         testCase "status"        test_omitStatus
       , testCase "statusMessage" test_omitStatusMessage
       , testCase "allTrailers"   test_omitAllTrailers
       ]
    ]

{-------------------------------------------------------------------------------
  HTTP Status
-------------------------------------------------------------------------------}

-- | Test HTTP to gRPC status code mapping
--
-- We don't test all codes here; we'd just end up duplicating the logic in
-- 'classifyServerResponse'. We just check one representative value.
test_statusNon200 :: Assertion
test_statusNon200 = respondWith response $ \addr -> do
    mResp :: Either GrpcException (Proto PongMessage) <- try $
      Client.withConnection connParams (Client.ServerInsecure addr) $ \conn ->
        Client.withRPC conn def (Proxy @Ping) $ \call -> do
          Client.sendFinalInput call defMessage
          fst <$> Client.recvFinalOutput call
    case mResp of
      Left err | grpcError err == GrpcInternal ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp
  where
    response :: Response
    response = def {
          responseStatus = HTTP.badRequest400
        }

-- | Ensure that we include the response body for errors, if any
test_statusNon200Body :: Assertion
test_statusNon200Body = respondWith response $ \addr -> do
    mResp :: Either GrpcException (Proto PongMessage) <- try $
      Client.withConnection connParams (Client.ServerInsecure addr) $ \conn ->
        Client.withRPC conn def (Proxy @Ping) $ \call -> do
          Client.sendFinalInput call defMessage
          fst <$> Client.recvFinalOutput call
    case mResp of
      Left err
        | grpcError err == GrpcInternal
        , Just msg <- grpcErrorMessage err
        , "Server supplied custom error" `Text.isInfixOf` msg ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp
  where
    response :: Response
    response = def {
          responseStatus = HTTP.badRequest400
        , responseBody   = "Server supplied custom error"
        }

{-------------------------------------------------------------------------------
  Content-type
-------------------------------------------------------------------------------}

test_invalidContentType :: Response -> Assertion
test_invalidContentType response = respondWith response $ \addr -> do
    mResp <- try $
      Client.withConnection connParams (Client.ServerInsecure addr) $ \conn ->
        Client.withRPC conn def (Proxy @Ping) $ \call -> do
          Client.sendFinalInput call defMessage
          fst <$> Client.recvFinalOutput call
    case mResp of
      Left GrpcException{grpcError = GrpcUnknown} ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp

test_nonGrpcContentTypeRegular :: Assertion
test_nonGrpcContentTypeRegular = test_invalidContentType def {
      responseHeaders = [ ("content-type", "someInvalidContentType") ]
    }

test_missingContentTypeRegular :: Assertion
test_missingContentTypeRegular = test_invalidContentType def {
      responseHeaders = [ ]
    }

test_nonGrpcContentTypeTrailersOnly :: Assertion
test_nonGrpcContentTypeTrailersOnly = test_invalidContentType def {
      responseHeaders = [ ("grpc-status", "0")
                        , ("content-type", "someInvalidContentType")
                        ]
    }

test_missingContentTypeTrailersOnly :: Assertion
test_missingContentTypeTrailersOnly = test_invalidContentType def {
      responseHeaders = [ ("grpc-status", "0") ]
    }

{-------------------------------------------------------------------------------
  Omit trailers
-------------------------------------------------------------------------------}

test_omitStatus :: Assertion
test_omitStatus = respondWith response $ \addr -> do
    mResp :: Either GrpcException
                    (StreamElem NoMetadata (Proto PongMessage)) <- try $
      Client.withConnection connParams (Client.ServerInsecure addr) $ \conn ->
        Client.withRPC conn def (Proxy @Ping) $ \call -> do
          Client.sendFinalInput call defMessage
          Client.recvOutput call
    case mResp of
      Left err
        | grpcError err == GrpcUnknown
        , Just msg <- grpcErrorMessage err
        , "grpc-status" `Text.isInfixOf` msg ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp
  where
    response :: Response
    response = def {
          responseTrailers = [
              ("grpc-message", "Message but no status")
            ]
        }

test_omitStatusMessage :: Assertion
test_omitStatusMessage = respondWith response $ \addr -> do
    mResp :: Either GrpcException
                    (StreamElem NoMetadata (Proto PongMessage)) <- try $
      Client.withConnection connParams (Client.ServerInsecure addr) $ \conn ->
        Client.withRPC conn def (Proxy @Ping) $ \call -> do
          Client.sendFinalInput call defMessage
          Client.recvOutput call
    case mResp of
      Right (NoMoreElems _) ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp
  where
    response :: Response
    response = def {
          responseTrailers = [
              ("grpc-status", "0")
            ]
        }

test_omitAllTrailers :: Assertion
test_omitAllTrailers = respondWith response $ \addr -> do
    mResp :: Either GrpcException
                    (StreamElem NoMetadata (Proto PongMessage)) <- try $
      Client.withConnection connParams (Client.ServerInsecure addr) $ \conn ->
        Client.withRPC conn def (Proxy @Ping) $ \call -> do
          Client.sendFinalInput call defMessage
          Client.recvOutput call
    case mResp of
      Left err
        | grpcError err == GrpcUnknown
        , Just msg <- grpcErrorMessage err
        , "closed without trailers" `Text.isInfixOf` msg ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp
  where
    response :: Response
    response = def {
          responseTrailers = []
        }

{-------------------------------------------------------------------------------
  Test server

  This allows us to simulate broken /servers/.

  TODO: <https://github.com/well-typed/grapesy/issues/22>
  We should also simulate broken /clients/.
-------------------------------------------------------------------------------}

data Response = Response {
      responseStatus   :: HTTP.Status
    , responseHeaders  :: [HTTP.Header]
    , responseBody     :: ByteString.Builder
    , responseTrailers :: [HTTP.Header]
    }

instance Default Response where
  def = Response {
        responseStatus   = HTTP.ok200
      , responseHeaders  = [ ("content-type", "application/grpc") ]
      , responseBody     = mempty
      , responseTrailers = [ ("grpc-status", "0") ]
      }

-- | Server that responds with the given 'Response', independent of the request
respondWith :: Response -> (Client.Address -> IO a) -> IO a
respondWith response = withTestServer $ \_req _aux respond ->
    respond http2Response []
  where
    http2Response :: HTTP2.Response
    http2Response =
        flip HTTP2.setResponseTrailersMaker trailersMaker $
          HTTP2.responseBuilder
            (responseStatus  response)
            (responseHeaders response)
            (responseBody    response)

    trailersMaker :: HTTP2.TrailersMaker
    trailersMaker Nothing  = return $ HTTP2.Trailers (responseTrailers response)
    trailersMaker (Just _) = return $ HTTP2.NextTrailersMaker trailersMaker

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

withTestServer :: HTTP2.Server -> (Client.Address -> IO a) -> IO a
withTestServer server k = do
    serverPort <- newEmptyMVar
    withAsync (testServer server serverPort) $ \_serverThread -> do
      port <- readMVar serverPort
      let addr :: Client.Address
          addr = Client.Address {
                addressHost      = "127.0.0.1"
              , addressPort      = port
              , addressAuthority = Nothing
              }
      k addr

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

connParams :: Client.ConnParams
connParams = def {
      Client.connVerifyHeaders = True
    }

