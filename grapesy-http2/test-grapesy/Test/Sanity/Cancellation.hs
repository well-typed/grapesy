{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

-- | Client cancellation (RST_STREAM)
--
-- Four very similar tests: in each, a client initiates a request and exchanges a message
-- with a server handler. The server handler then starts messages to the client
-- indefinitely, until it is cancelled. Variations along two different axes:
--
-- * The message from the client is marked as final or not; if final, that
--   leaves the client in half-closed state.
-- * The client terminates the scope from withRPC normally or with an exception;
--   this affects whether we send a RST_STREAM with CANCEL or INTERNAL_ERROR.
--
-- We also verify that the server handler is eventually cancelled; that is, that
-- is does not block indefinitely when it tries to send a message that a client
-- is not listening for anymore.
--
-- See also
--
-- * <https://github.com/well-typed/grapesy/issues/349>
-- * HTTP2 PR
module Test.Sanity.Cancellation (tests) where

import Control.Concurrent
import Control.Exception qualified as E
import Control.Monad
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Network.HTTP2.Client qualified as HTTP
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Common.Exception
import Network.GRPC.Server qualified as Server

import Test.Driver.ClientServer

tests :: TestTree
tests = testGroup "Test.Sanity.Cancellation" [
      testGroup "noException" [
          testCase "beforeHalfClosed" $
            testReset
              Client.sendNextInput
              (return ())
        , testCase "afterHalfClosed" $
            testReset
              Client.sendFinalInput
              (return ())
        ]
    , testGroup "withException" [
          testCase "beforeHalfClosed" $
            testReset
              Client.sendNextInput
              (throwIO $ DeliberateClientException 1)
        , testCase "afterHalfClosed" $
            testReset
              Client.sendFinalInput
              (throwIO $ DeliberateClientException 1)
        ]
    ]

{-------------------------------------------------------------------------------
  Test client
-------------------------------------------------------------------------------}

testReset ::
     (Client.Call EchoUntilCancelled -> Lazy.ByteString -> IO ())
     -- ^ How should we send the input?
     -- ('sendNextInput', 'sendFinalInput')
  -> IO ()
     -- ^ How should the client exit the scope of 'withRPC'?
     -- (@return ()@, @throwIO@)
  -> Assertion
testReset sendInput leaveScope = do
    resultVar <- newEmptyMVar
    testClientServer ClientServerTest{
        config = def
      , server = [Server.someRpcHandler $ handleEchoUntilCancelled resultVar]
      , client = simpleTestClient $ \conn -> do
          checkClientException $ do
            Client.withRPC conn def (Proxy @EchoUntilCancelled) $ \call -> do
              sendInput call "ABCDE"
              resp <- Client.recvOutput call
              assertEqual "" (StreamElem "ABCDE") $ resp
              leaveScope

          handlerResult <- readMVar resultVar
          case handlerResult of
            Left e | checkServerException e -> return ()
            _otherwise -> assertFailure $ "Unexpected " ++ show handlerResult
      }

{-------------------------------------------------------------------------------
  Server handler
-------------------------------------------------------------------------------}

type EchoUntilCancelled = RawRpc "Test" "EchoUntilCancelled"

type instance RequestMetadata          EchoUntilCancelled = NoMetadata
type instance ResponseInitialMetadata  EchoUntilCancelled = NoMetadata
type instance ResponseTrailingMetadata EchoUntilCancelled = NoMetadata

-- | Server handler
--
-- The handler expects a single message of type 'Text', which may or may not
-- be marked final; it then starts echoing back that message indefinitely until
-- it is cancelled.
handleEchoUntilCancelled ::
     MVar (Either ExactException ())
     -- ^ The server's own result is reported back to the client out-of-band,
     -- so that we can check it as part of the test ('checkServerException').
     --
     -- NOTE: The regular 'isExpectedServerException' is less useful here,
     -- because if the test simply terminates without the client waiting for the
     -- server, the server handler might simply see the entire connection
     -- disappear and reported a different exception.
  -> Server.RpcHandler IO EchoUntilCancelled
handleEchoUntilCancelled resultVar = Server.mkRpcHandler $ \call -> do
    let echoUntilCancelled :: Lazy.ByteString -> IO ()
        echoUntilCancelled msg = forever $ do
            Server.sendOutput call $ StreamElem msg
            threadDelay 10_000

        handlerBody :: IO ()
        handlerBody = do
            inp <- Server.recvInput call
            case inp of
              StreamElem msg            -> echoUntilCancelled msg
              FinalElem  msg NoMetadata -> echoUntilCancelled msg
              NoMoreElems    NoMetadata -> assertFailure "Unexpected NoMoreElems"

    putMVar resultVar =<< E.try handlerBody

{-------------------------------------------------------------------------------
  Expected exceptions
-------------------------------------------------------------------------------}

-- | Check client-side exception
--
-- We expected 'GrpcCancelled' unless the client threw an exception itself.
checkClientException :: IO () -> IO ()
checkClientException client = do
    clientResult :: Either ExactException () <- E.try client
    case clientResult of
      Right () ->
        assertFailure "Expected client exception"
      Left e | Just e'
                 <- E.fromException (unwrapExactException e)
             , grpcError e' == GrpcCancelled ->
        return ()
      Left e | Just DeliberateClientException{}
                 <- E.fromException (unwrapExactException e) ->
        return ()
      _otherwise ->
        assertFailure $ "Unexpected " ++ show clientResult

-- | Check server-side exception
--
-- The exact nature of the exception on whether inside of @http2@ the RST_STREAM
-- frame is handled.
--
-- TODO <https://github.com/well-typed/grapesy/issues/339>
-- It might be better if we made the presence of RST_STREAM explicitly visible
-- in the @grapesy@-side exception.
checkServerException :: ExactException -> Bool
checkServerException e
  | Just ClientDisconnected{clientDisconnectedException}
      <- E.fromException (unwrapExactException e)
  , Just HTTP.StreamResetIsReceived{}
      <- E.fromException (unwrapExactException clientDisconnectedException)
  = True

  | Just ClientDisconnected{clientDisconnectedException}
      <- E.fromException (unwrapExactException e)
  , Just HTTP.StreamRemoteReset{}
      <- E.fromException (unwrapExactException clientDisconnectedException)
  = True

  | otherwise
  = False