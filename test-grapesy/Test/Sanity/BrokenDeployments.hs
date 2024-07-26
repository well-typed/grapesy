-- Intentionally /NOT/ enabling OverloadedStrings.
-- This forces us to be precise about encoding issues.

module Test.Sanity.BrokenDeployments (tests) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Builder qualified as BS.Builder
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8
import Data.String (fromString)
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
    , testGroup "Invalid" [
          testCase "statusMessage"   test_invalidStatusMessage
        , testCase "requestMetadata" test_invalidRequestMetadata
        , testCase "trailerMetadata" test_invalidTrailerMetadata
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
        , Text.pack "Server supplied custom error" `Text.isInfixOf` msg ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp
  where
    response :: Response
    response = def {
          responseStatus = HTTP.badRequest400
        , responseBody   = BS.Strict.Char8.pack customError
        }

    customError :: String
    customError = "Server supplied custom error"

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
      responseHeaders = [
          asciiHeader "content-type" "someInvalidContentType"
        ]
    }

test_missingContentTypeRegular :: Assertion
test_missingContentTypeRegular = test_invalidContentType def {
      responseHeaders = [ ]
    }

test_nonGrpcContentTypeTrailersOnly :: Assertion
test_nonGrpcContentTypeTrailersOnly = test_invalidContentType def {
      responseHeaders = [
          asciiHeader "grpc-status" "0"
        , asciiHeader "content-type" "someInvalidContentType"
        ]
    }

test_missingContentTypeTrailersOnly :: Assertion
test_missingContentTypeTrailersOnly = test_invalidContentType def {
      responseHeaders = [
          asciiHeader "grpc-status" "0"
        ]
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
        , grpcMessageContains err "grpc-status" ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp
  where
    response :: Response
    response = def {
          responseTrailers = [
              asciiHeader "grpc-message" "Message but no status"
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
              asciiHeader "grpc-status" "0"
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
        , grpcMessageContains err "closed without trailers" ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp
  where
    response :: Response
    response = def {
          responseTrailers = []
        }

{-------------------------------------------------------------------------------
  Invalid headers

  The gRPC spec mandates that we /MUST NOT/ throw away invalid headers. This
  is done as a matter of default for all headers in grapesy, except the ones
  that it really needs to operate. To access these invalid values, users do
  however need to use the low-level API.
-------------------------------------------------------------------------------}

test_invalidStatusMessage :: Assertion
test_invalidStatusMessage = respondWith response $ \addr -> do
    mResp :: StreamElem
               Client.ProperTrailers'
               (InboundMeta, Proto PongMessage) <-
      Client.withConnection connParams (Client.ServerInsecure addr) $ \conn ->
        Client.withRPC conn def (Proxy @Ping) $ \call -> do
          Client.sendFinalInput call defMessage
          Client.recvOutputWithMeta call
    case mResp of
      NoMoreElems trailers
        | Left invalid <- Client.properTrailersGrpcMessage trailers
        , [ (_, headerValue) ] <- invalidHeaders invalid
        , headerValue == BS.Strict.Char8.pack someInvalidMessage
        ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp
  where
    response :: Response
    response = def {
          responseTrailers = [
              asciiHeader "grpc-status" "13" -- 'GrpcInternal'
            , asciiHeader "grpc-message" someInvalidMessage
            ]
        }

    someInvalidMessage :: String
    someInvalidMessage = "This is invalid: %X"

test_invalidRequestMetadata :: Assertion
test_invalidRequestMetadata = respondWith response $ \addr -> do
    mResp :: Either
               (Client.TrailersOnly'    HandledSynthesized)
               (Client.ResponseHeaders' HandledSynthesized) <-
      Client.withConnection connParams' (Client.ServerInsecure addr) $ \conn ->
        Client.withRPC conn def (Proxy @Ping) $ \call -> do
          Client.recvInitialResponse call
    case mResp of
      Right headers
        | Left invalid <- Client.responseUnrecognized headers
        , [ (_, headerValue) ] <- invalidHeaders invalid
        , headerValue == BS.Strict.UTF8.fromString someInvalidMetadata
        ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp
  where
    -- In this case we do /NOT/ want to verify all headers
    -- (the whole point is that we can access the invalid header value)
    connParams' :: Client.ConnParams
    connParams' = def { Client.connVerifyHeaders = False }

    response :: Response
    response = def {
          responseHeaders = [
              asciiHeader "content-type" "application/grpc"
            , utf8Header "some-custom-header" someInvalidMetadata
            ]
        }

    someInvalidMetadata :: String
    someInvalidMetadata = "This is invalid: 你好"

test_invalidTrailerMetadata :: Assertion
test_invalidTrailerMetadata = respondWith response $ \addr -> do
    mResp :: StreamElem
               Client.ProperTrailers'
               (InboundMeta, Proto PongMessage) <-
      Client.withConnection connParams (Client.ServerInsecure addr) $ \conn ->
        Client.withRPC conn def (Proxy @Ping) $ \call -> do
          Client.sendFinalInput call defMessage
          Client.recvOutputWithMeta call
    case mResp of
      NoMoreElems trailers
        | Left invalid <- Client.properTrailersUnrecognized trailers
        , [ (_, headerValue) ] <- invalidHeaders invalid
        , headerValue == BS.Strict.UTF8.fromString someInvalidMetadata
        ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected response: " ++ show mResp
  where
    response :: Response
    response = def {
          responseTrailers = [
              asciiHeader "grpc-status" "0"
            , utf8Header "some-custom-trailer" someInvalidMetadata
            ]
        }

    someInvalidMetadata :: String
    someInvalidMetadata = "This is invalid: 你好"

{-------------------------------------------------------------------------------
  Test server

  This allows us to simulate broken /servers/.
-------------------------------------------------------------------------------}

data Response = Response {
      responseStatus   :: HTTP.Status
    , responseHeaders  :: [HTTP.Header]
    , responseBody     :: Strict.ByteString
    , responseTrailers :: [HTTP.Header]
    }

instance Default Response where
  def = Response {
        responseStatus   = HTTP.ok200
      , responseHeaders  = [ asciiHeader "content-type" "application/grpc" ]
      , responseBody     = BS.Strict.empty
      , responseTrailers = [ asciiHeader "grpc-status" "0" ]
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
            (BS.Builder.byteString $ responseBody response)

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

-- | Header with ASCII value
--
-- (Header /names/ are always ASCII.)
asciiHeader :: String -> String -> HTTP.Header
asciiHeader name value = (fromString name, BS.Strict.Char8.pack value)

-- | Header with UTF-8 encoded value
utf8Header :: String -> String -> HTTP.Header
utf8Header name value = (fromString name, BS.Strict.UTF8.fromString value)

grpcMessageContains :: GrpcException -> String -> Bool
grpcMessageContains GrpcException{grpcErrorMessage} str =
    case grpcErrorMessage of
      Just msg -> Text.pack str `Text.isInfixOf` msg
      Nothing  -> False
