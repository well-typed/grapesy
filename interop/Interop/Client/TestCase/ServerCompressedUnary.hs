module Interop.Client.TestCase.ServerCompressedUnary (runTest) where

import Data.Maybe (isJust)

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Spec

import Interop.API
import Interop.Client.Common
import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions
import Interop.Util.Messages

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#server_compressed_unary>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn -> do
      withRPC conn def (Proxy @UnaryCall) $ \call -> do
        sendInputWithEnvelope call $ FinalElem (request True) NoMetadata
        resp <- recvOutputWithEnvelope call
        verifyResponse True (StreamElem.value resp)

      withRPC conn def (Proxy @UnaryCall) $ \call -> do
        sendInputWithEnvelope call $ FinalElem (request False) NoMetadata
        resp <- recvOutputWithEnvelope call
        verifyResponse False (StreamElem.value resp)
  where
    -- To keep the test simple, we disable /outbound/ compression
    -- (this test is testing /inbound/ compression)
    request :: Bool -> (OutboundEnvelope, Proto SimpleRequest)
    request expectCompressed = (
          def { outboundEnableCompression = False }
        , mkSimpleRequest False
            & #responseCompressed .~ boolValue expectCompressed
        )

verifyResponse ::
     HasCallStack
  => Bool -> Maybe (InboundEnvelope, Proto SimpleResponse) -> IO ()
verifyResponse _expectCompressed Nothing =
    assertFailure "Expected response"
verifyResponse expectCompressed (Just (envelope, resp)) = do
    assertEqual expectCompressed $ isJust (inboundCompressedSize envelope)
    verifySimpleResponse resp

