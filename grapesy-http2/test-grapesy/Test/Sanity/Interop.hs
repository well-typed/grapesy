{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test functionality required by the gRPC interop tests
module Test.Sanity.Interop (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS.Strict
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Client.StreamType.IO qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Binary (RawRpc)
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary
import Network.GRPC.Server.StreamType (ServerHandler'(..))
import Network.GRPC.Server.StreamType qualified as Server

import Proto.API.Interop
import Proto.API.Ping

import Test.Driver.ClientServer

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.Interop" [
      testGroup "preliminary" [
          testCase "callAfterException" test_callAfterException
        ]
    , testGroup "cancellation" [
          testCase "client" test_cancellation_client
        , testCase "server" test_cancellation_server
        ]
    , testGroup "official" [
          testCase "emptyUnary"                test_emptyUnary
        , testCase "serverCompressedStreaming" test_serverCompressedStreaming
        ]
    ]

{-------------------------------------------------------------------------------
  Preliminary: verify that we can do a second call on the same connection,
  even if the first failed with a gRPC exception
-------------------------------------------------------------------------------}

test_callAfterException :: IO ()
test_callAfterException =
    testClientServer $ ClientServerTest {
        config = def
      , client = simpleTestClient $ \conn -> do
          resp1 <- ping conn $ defMessage & #id .~ 0
          case resp1 of
            Left _  -> return ()
            Right _ -> assertFailure "Expected gRPC exception"

          resp2 <- ping conn $ defMessage & #id .~ 1
          case resp2 of
            Left  _ -> assertFailure "Expected pong"
            Right i -> assertEqual "pong" i (defMessage & #id .~ 1)
      , server = [
            Server.someRpcHandler $
              Server.mkRpcHandler @Ping $ \call -> do
                pingMsg <- Server.recvFinalInput call
                if pingMsg ^. #id > 0 then do
                  let pongMsg :: Proto PongMessage
                      pongMsg = defMessage & #id .~ (pingMsg ^. #id)
                  Server.sendFinalOutput call (pongMsg, NoMetadata)
                else
                  Server.sendGrpcException call $ GrpcException {
                      grpcError         = GrpcInvalidArgument
                    , grpcErrorMessage  = Just "Expected non-zero ping"
                    , grpcErrorDetails  = Nothing
                    , grpcErrorMetadata = []
                    }
          ]
      }
  where
    ping ::
         Client.Connection
      -> Proto PingMessage
      -> IO (Either SomeException (Proto PongMessage))
    ping conn = try . Client.nonStreaming conn (Client.rpc @Ping)

{-------------------------------------------------------------------------------
  @empty_unary@

  <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#empty_unary>
-------------------------------------------------------------------------------}

-- | Test that the empty message has an empty encoding
--
-- This test fails if we unconditionally compress (the /compressed/ form of the
-- empty message is larger than the uncompressed form, as compression introduces
-- minor overhead).
test_emptyUnary :: IO ()
test_emptyUnary =
    testClientServer $ ClientServerTest {
        config = def
      , client = simpleTestClient $ \conn ->
          Client.withRPC conn def (Proxy @EmptyCall) $ \call -> do
            Client.sendFinalInput call defMessage
            streamElem <- Client.recvOutputWithMeta call
            case StreamElem.value streamElem of
              Nothing         -> fail "Expected answer"
              Just (meta, _x) -> verifyMeta meta
      , server = [
            Server.fromMethod @EmptyCall $ ServerHandler $ \_empty ->
              return defMessage
          ]
      }
  where
    -- We don't /expect/ the empty message to be compressed, due to the overhead
    -- mentioned above. However, /if/ it is compressed, perhaps using a custom
    -- zero-overhead compression algorithm, it's size should be zero.
    verifyMeta :: InboundMeta -> IO ()
    verifyMeta meta = do
        assertEqual "uncompressed size" (inboundUncompressedSize meta) 0
        case inboundCompressedSize meta of
          Nothing   -> return ()
          Just size -> assertEqual "compressed size" size 0

{-------------------------------------------------------------------------------
  @server_compressed_streaming@

  <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#server_compressed_streaming>
-------------------------------------------------------------------------------}

-- | Test that we can enable and disable compression per message
test_serverCompressedStreaming :: IO ()
test_serverCompressedStreaming =
    testClientServer ClientServerTest {
        config = def
      , client = simpleTestClient $ \conn ->
          Client.withRPC conn def (Proxy @StreamingOutputCall) $ \call -> do
            Client.sendFinalInput call $ defMessage & #responseParameters .~ [
                defMessage
                  & #compressed .~ (defMessage & #value .~ True)
                  & #size .~ 31415
              , defMessage
                  & #compressed .~ (defMessage & #value .~ False)
                  & #size .~ 92653
              ]
            output1 <- Client.recvOutputWithMeta call
            output2 <- Client.recvOutputWithMeta call
            verifyOutputs (StreamElem.value output1, StreamElem.value output2)
      , server = [
            Server.someRpcHandler $
              Server.mkRpcHandler @StreamingOutputCall $ \call -> do
                handleStreamingOutputCall call
          ]
      }
  where
    handleStreamingOutputCall :: Server.Call StreamingOutputCall -> IO ()
    handleStreamingOutputCall call = do
        -- Wait for request
        request <- Server.recvFinalInput call

        -- Send all requested messages
        forM_ (request ^. #responseParameters) $ \responseParams -> do

          let shouldCompress :: Bool
              shouldCompress = responseParams ^. #compressed . #value

              size :: Int
              size = fromIntegral $ responseParams ^. #size

              meta :: OutboundMeta
              meta = def { outboundEnableCompression = shouldCompress }

              -- Payload matters for the test, because for messages that are too
              -- small no compression is used even when enabled.
              payload :: Proto Payload
              payload = defMessage & #body .~ BS.Strict.pack (replicate size 0)

              response :: Proto StreamingOutputCallResponse
              response = defMessage & #payload .~ payload

          Server.sendOutputWithMeta call $ StreamElem (meta, response)

        -- No further output
        Server.sendTrailers call def

    verifyOutputs ::
         ( Maybe (InboundMeta, Proto StreamingOutputCallResponse)
         , Maybe (InboundMeta, Proto StreamingOutputCallResponse)
         )
      -> IO ()
    verifyOutputs = \case
        (Just (meta1, _), Just (meta2, _)) -> do
          case inboundCompressedSize meta1 of
            Nothing -> assertFailure "First output should be compressed"
            Just _  -> return ()
          case inboundCompressedSize meta2 of
            Nothing -> return ()
            Just _  -> assertFailure "First output should not be compressed"
        _otherwise ->
          assertFailure "Expected value"

{-------------------------------------------------------------------------------
  Cancellation
-------------------------------------------------------------------------------}

type StreamNats = RawRpc "test" "nats"

test_cancellation_client :: IO ()
test_cancellation_client =
    testClientServer ClientServerTest {
        config = def {
            isExpectedServerException = isClientDisconnected
          }
      , client = simpleTestClient $ \conn -> do
          -- We wait for the first input, but then cancel the request
          result :: Either GrpcException (Maybe Int) <- try $
            Client.withRPC conn def (Proxy @StreamNats) $ \call -> do
              StreamElem.value <$> Client.Binary.recvOutput call

          -- Since we did not tell the server that we have sent our final
          -- outbound message, the client should receive a CANCELLED exception.
          case result of
            Left err ->
              assertEqual "grpcError" GrpcCancelled $ grpcError err
            Right _ ->
              assertFailure "Expected exception"
      , server = [
          Server.someRpcHandler $
            Server.mkRpcHandler @StreamNats $ \call -> do
              forM_ [1 .. 100] $ \(i :: Int) -> do
                Server.Binary.sendNextOutput call i
                threadDelay 100_000
              Server.sendTrailers call NoMetadata
        ]
      }

test_cancellation_server :: IO ()
test_cancellation_server =
    testClientServer ClientServerTest {
        config = def {
            isExpectedServerException = isHandlerTerminated
          }
      , client = simpleTestClient $ \conn -> do
          result :: Either GrpcException [Int] <- try $
            Client.withRPC conn def (Proxy @StreamNats) $ \call -> do
              Client.sendEndOfInput call
              let loop :: [Int] -> IO [Int]
                  loop acc = do
                      mx <- StreamElem.value <$> Client.Binary.recvOutput call
                      case mx of
                        Nothing -> return $ reverse acc
                        Just x  -> loop (x:acc)
              loop []
          case result of
            Left err -> do
              assertEqual "grpcError" GrpcUnknown $
                grpcError err
              assertEqual "grpcErrorMessage" (Just "Server-side exception: HandlerTerminated") $
                grpcErrorMessage err
            Right _ ->
              assertFailure "Expected exception"
      , server = [
          Server.someRpcHandler $
            -- The server sends only one value, then gives up
            Server.mkRpcHandler @StreamNats $ \call -> do
              Server.Binary.sendNextOutput call (1 :: Int)
        ]
      }

{-------------------------------------------------------------------------------
  Internal: we don't care about metadata in these tests
-------------------------------------------------------------------------------}

type instance RequestMetadata          (RawRpc "test" meth) = NoMetadata
type instance ResponseInitialMetadata  (RawRpc "test" meth) = NoMetadata
type instance ResponseTrailingMetadata (RawRpc "test" meth) = NoMetadata
