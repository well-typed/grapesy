-- Intentionally /NOT/ enabling OverloadedStrings.
-- This forces us to be precise about encoding issues.

{-# LANGUAGE OverloadedLabels #-}

module Test.Sanity.BrokenDeployments (tests) where

import Control.Concurrent
import Control.Exception
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8
import Data.IORef
import Data.Text qualified as Text
import Network.HTTP.Types qualified as HTTP
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.StreamType.IO qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server.StreamType qualified as Server

import Test.Driver.ClientServer
import Test.Util.RawTestServer

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
    , testGroup "Undefined" [
          testCase "output" test_undefinedOutput
        ]
    , testGroup "Timeout" [
          testCase "serverIgnoresTimeout" test_serverIgnoresTimeout
        ]
    ]

connParams :: Client.ConnParams
connParams = def {
      Client.connVerifyHeaders = True
    }

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
          Client.sendEndOfInput call
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

grpcMessageContains :: GrpcException -> String -> Bool
grpcMessageContains GrpcException{grpcErrorMessage} str =
    case grpcErrorMessage of
      Just msg -> Text.pack str `Text.isInfixOf` msg
      Nothing  -> False

{-------------------------------------------------------------------------------
  Undefined values
-------------------------------------------------------------------------------}

test_undefinedOutput :: Assertion
test_undefinedOutput = do
    st <- newIORef 0
    testClientServer $ ClientServerTest {
        config = def {
            isExpectedServerException = isDeliberateException
          }
      , server = [Server.fromMethod @Ping $ Server.mkNonStreaming (handler st)]
      , client = simpleTestClient $ \conn -> do

          -- The first time the handler is invoked, it attempts to enqueue a
          -- an undefined message (one containing a pure exception). Prior to
          -- #235 this would result in undefined behaviour, probably the server
          -- disconnecting. What should happen instead is that this exception
          -- is thrown in the handler, caught, sent to the client as a
          -- 'GrpcException', and re-raised in the client.
          mResp1 :: Either GrpcException (Proto PongMessage) <- try $
            Client.nonStreaming conn (Client.rpc @Ping) (defMessage & #id .~ 1)
          case mResp1 of
            Left err | Just msg <- grpcErrorMessage err ->
              assertBool "" $ Text.pack "uhoh" `Text.isInfixOf` msg
            _otherwise ->
              assertFailure "Unexpected response"

          -- Meanwhile, the server should just continue running; the /second/
          -- invocation of the handler should succeed normally.
          mResp2 :: Either GrpcException (Proto PongMessage) <- try $
            Client.nonStreaming conn (Client.rpc @Ping) (defMessage & #id .~ 2)
          case mResp2 of
            Right resp ->
              assertEqual "" 2 $ resp ^. #id
            _otherwise ->
              assertFailure "Unexpected response"
      }
  where
    -- Server handler attempts to enqueue an undefined message
    handler :: IORef Int -> Proto PingMessage -> IO (Proto PongMessage)
    handler st req = do
        isFirst <- atomicModifyIORef st $ \i -> (succ i, i == 0)
        if isFirst
          then return $ throw $ DeliberateException (userError "uhoh")
          else return $ defMessage & #id .~ req ^. #id

{-------------------------------------------------------------------------------
  Timeouts
-------------------------------------------------------------------------------}

-- | Check that timeouts don't depend on the server
--
-- When a timeout is set for an RPC, the server should respect it, but the
-- client should not /depend/ on the server respecting it.
--
-- See also <https://github.com/well-typed/grapesy/issues/221>.
test_serverIgnoresTimeout :: Assertion
test_serverIgnoresTimeout = respondWithIO response $ \addr -> do
    mResp :: Either GrpcException
                    (StreamElem NoMetadata (Proto PongMessage)) <- try $
      Client.withConnection connParams (Client.ServerInsecure addr) $ \conn ->
        Client.withRPC conn callParams (Proxy @Ping) $ \call -> do
          Client.sendFinalInput call defMessage
          Client.recvOutput call
    case mResp of
      Left e | grpcError e == GrpcDeadlineExceeded ->
        return ()
      Left e ->
        assertFailure $ "unexpected error: " ++ show e
      Right _ ->
        assertFailure "Timeout did not trigger"
  where
    response :: IO Response
    response = do
        threadDelay 10_000_000
        return def

    callParams :: Client.CallParams Ping
    callParams = def {
          Client.callTimeout = Just $
            Client.Timeout Client.Millisecond (Client.TimeoutValue 100)
        }

