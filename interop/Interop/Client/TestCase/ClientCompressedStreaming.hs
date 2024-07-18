module Interop.Client.TestCase.ClientCompressedStreaming (runTest) where

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec

import Interop.Client.Common
import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

import Proto.API.Interop

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#client_compressed_streaming>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection enableInitCompression (testServer cmdline) $ \conn -> do
      -- 1. Call StreamingInputCall and send the feature-probing message
      checkServerSupportsCompressedRequest conn

      withRPC conn def (Proxy @StreamingInputCall) $ \call -> do
        sendInputWithEnvelope call $ StreamElem compressed
        sendInputWithEnvelope call $ FinalElem uncompressed NoMetadata

        (resp, _metadata) <- recvFinalOutput call
        assertEqual 73086 $ resp ^. #aggregatedPayloadSize
  where
    -- Expect compressed, and /is/ compressed
    compressed :: (OutboundEnvelope, Proto StreamingInputCallRequest)
    compressed = (
          def { outboundEnableCompression = True }
        , mkStreamingInputCallRequest True 27182
        )

    -- Expect uncompressed, and /is/ uncompressed
    uncompressed :: (OutboundEnvelope, Proto StreamingInputCallRequest)
    uncompressed = (
          def { outboundEnableCompression = False }
        , mkStreamingInputCallRequest False 45904
        )

-- | Check whether the server supports CompressedRequest
--
-- If it does not, throws 'TestSkipped'.
--
-- This is similar to the defintiion in
-- "Interop.Client.TestCase.ClientCompressedUnary", but (per the test spec) not
-- identical; that one uses the "unaryCall" endpoint, here we use
-- "streamingInputCall". The probe that we send is also slightly different.
checkServerSupportsCompressedRequest :: Connection -> IO ()
checkServerSupportsCompressedRequest conn =
    withRPC conn def (Proxy @StreamingInputCall) $ \call -> do
      sendInputWithEnvelope call $ FinalElem featureProbe NoMetadata
      expectInvalidArgument $ recvFinalOutput call
  where
    -- Expect compressed, but is /not/ actually compressed
    featureProbe :: (OutboundEnvelope, Proto StreamingInputCallRequest)
    featureProbe = (
          def { outboundEnableCompression = False }
        , mkStreamingInputCallRequest True 27182
        )
