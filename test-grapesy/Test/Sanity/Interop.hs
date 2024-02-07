{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test functionality required by the gRPC interop tests
module Test.Sanity.Interop (tests) where

import Control.Exception
import Control.Lens ((^.), (.~))
import Control.Monad
import Data.Bifunctor
import Data.ByteString qualified as BS.Strict
import Data.Default
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.StreamType.IO.Binary qualified as Client.Binary
import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Internal
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary
import Network.GRPC.Server.StreamType qualified as Server
import Network.GRPC.Spec

import Proto.Src.Proto.Grpc.Testing.Empty
import Proto.Src.Proto.Grpc.Testing.Messages
import Proto.Src.Proto.Grpc.Testing.Test

import Test.Driver.ClientServer

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.Interop" [
      -- Preliminaries
      testGroup "preliminary" [
          testCaseInfo "callAfterException" test_callAfterException
        ]

      -- Tests from the gRPC interop suite
      --
      -- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#client>
    , testGroup "official" [
          testCaseInfo "emptyUnary"                test_emptyUnary
        , testCaseInfo "serverCompressedStreaming" test_serverCompressedStreaming
        ]
    ]

{-------------------------------------------------------------------------------
  Preliminary: verify that we can do a second call on the same connection,
  even if the first failed with a gRPC exception
-------------------------------------------------------------------------------}

type Ping = BinaryRpc "test" "ping"

test_callAfterException :: IO String
test_callAfterException =
    testClientServer expectInvalidArgument def {
        client = \withConn -> withConn $ \conn -> do
          resp1 <- call conn 0
          case resp1 of
            Left _  -> return ()
            Right _ -> assertFailure "Expected gRPC exception"

          resp2 <- call conn 1
          case resp2 of
            Left  _ -> assertFailure "Expected pong"
            Right i -> assertEqual "pong" i 1
      , server = [
            Server.streamingRpcHandler (Proxy @Ping) $
              Server.Binary.mkNonStreaming $ \(i :: Word) ->
                if i > 0 then
                  return i
                else
                  throwIO $ GrpcException {
                      grpcError         = GrpcInvalidArgument
                    , grpcErrorMessage  = Just "Expected non-zero ping"
                    , grpcErrorMetadata = []
                    }
          ]
      }
  where
    call :: Client.Connection -> Word -> IO (Either SomeException Word)
    call conn =
        fmap (first (innerNestedException :: SomeException -> SomeException))
      . try
      . Client.Binary.nonStreaming conn (Client.rpc @Ping)

    expectInvalidArgument :: SomeException -> Maybe ()
    expectInvalidArgument e
      | Just GrpcException{grpcError = GrpcInvalidArgument} <- fromException e
      = Just ()

      | otherwise
      = Nothing

{-------------------------------------------------------------------------------
  @empty_unary@

  <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#empty_unary>
-------------------------------------------------------------------------------}

type EmptyCall = Protobuf TestService "emptyCall"

-- | Test that the empty message has an empty encoding
--
-- This test fails if we unconditionally compress (the /compressed/ form of the
-- empty message is larger than the uncompressed form, as compression introduces
-- minor overhead).
test_emptyUnary :: IO String
test_emptyUnary =
    testClientServer noCustomExceptions def {
        client = \withConn -> withConn $ \conn ->
          Client.withRPC conn def (Proxy @EmptyCall) $ \call -> do
            Client.sendFinalInput call (defMessage :: Empty)
            streamElem <- Client.recvOutputWithEnvelope call
            case StreamElem.value streamElem of
              Nothing                      -> fail "Expected answer"
              Just (envelope, _x :: Empty) -> verifyEnvelope envelope
      , server = [
            Server.streamingRpcHandler (Proxy @EmptyCall) $
              Server.mkNonStreaming $ \(_ ::Empty) ->
                return (defMessage :: Empty)
          ]
      }
  where
    -- We don't /expect/ the empty message to be compressed, due to the overhead
    -- mentioned above. However, /if/ it is compressed, perhaps using a custom
    -- zero-overhead compression algorithm, it's size should be zero.
    verifyEnvelope :: InboundEnvelope -> IO ()
    verifyEnvelope envelope = do
        assertEqual "uncompressed size" (inboundUncompressedSize envelope) 0
        case inboundCompressedSize envelope of
          Nothing   -> return ()
          Just size -> assertEqual "compressed size" size 0

{-------------------------------------------------------------------------------
  @server_compressed_streaming@

  <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#server_compressed_streaming>
-------------------------------------------------------------------------------}

type StreamingOutputCall = Protobuf TestService "streamingOutputCall"

-- | Test that we can enable and disable compression per message
test_serverCompressedStreaming :: IO String
test_serverCompressedStreaming =
    testClientServer noCustomExceptions def {
        client = \withConn -> withConn $ \conn ->
          Client.withRPC conn def (Proxy @StreamingOutputCall) $ \call -> do
            Client.sendFinalInput call $ defMessage & #responseParameters .~ [
                defMessage
                  & #compressed .~ (defMessage & #value .~ True)
                  & #size .~ 31415
              , defMessage
                  & #compressed .~ (defMessage & #value .~ False)
                  & #size .~ 92653
              ]
            output1 <- Client.recvOutputWithEnvelope call
            output2 <- Client.recvOutputWithEnvelope call
            verifyOutputs (StreamElem.value output1, StreamElem.value output2)
      , server = [
            Server.mkRpcHandler (Proxy @StreamingOutputCall) $ \call -> do
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

              envelope :: OutboundEnvelope
              envelope = def { outboundEnableCompression = shouldCompress }

              -- Payload matters for the test, because for messages that are too
              -- small no compression is used even when enabled.
              payload :: Payload
              payload = defMessage & #body .~ BS.Strict.pack (replicate size 0)

              response :: StreamingOutputCallResponse
              response = defMessage & #payload .~ payload

          Server.sendOutputWithEnvelope call $ StreamElem (envelope, response)

        -- No further output
        Server.sendTrailers call []

    verifyOutputs ::
         ( Maybe (InboundEnvelope, StreamingOutputCallResponse)
         , Maybe (InboundEnvelope, StreamingOutputCallResponse)
         )
      -> IO ()
    verifyOutputs = \case
        (Just (envelope1, _), Just (envelope2, _)) -> do
          case inboundCompressedSize envelope1 of
            Nothing -> assertFailure "First output should be compressed"
            Just _  -> return ()
          case inboundCompressedSize envelope2 of
            Nothing -> return ()
            Just _  -> assertFailure "First output should not be compressed"
        _otherwise ->
          assertFailure "Expected value"

{-------------------------------------------------------------------------------
  @client_compressed_streaming@

  <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#client_compressed_streaming>
-------------------------------------------------------------------------------}

-- TODO
