module Interop.Client.TestCase.ServerCompressedUnary (runTest) where

import Data.Maybe (isJust)

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.StreamElem qualified as StreamElem

import Interop.Client.Common
import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions
import Interop.Util.Messages

import Proto.API.Interop

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#server_compressed_unary>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn -> do
      withRPC conn def (Proxy @UnaryCall) $ \call -> do
        sendInputWithMeta call $ FinalElem (request True) NoMetadata
        resp <- recvOutputWithMeta call
        verifyResponse True (StreamElem.value resp)

      withRPC conn def (Proxy @UnaryCall) $ \call -> do
        sendInputWithMeta call $ FinalElem (request False) NoMetadata
        resp <- recvOutputWithMeta call
        verifyResponse False (StreamElem.value resp)
  where
    -- To keep the test simple, we disable /outbound/ compression
    -- (this test is testing /inbound/ compression)
    request :: Bool -> (OutboundMeta, Proto SimpleRequest)
    request expectCompressed = (
          def { outboundEnableCompression = False }
        , mkSimpleRequest False
            & #responseCompressed .~ boolValue expectCompressed
        )

verifyResponse ::
     HasCallStack
  => Bool -> Maybe (InboundMeta, Proto SimpleResponse) -> IO ()
verifyResponse _expectCompressed Nothing =
    assertFailure "Expected response"
verifyResponse expectCompressed (Just (meta, resp)) = do
    assertEqual expectCompressed $ isJust (inboundCompressedSize meta)
    verifySimpleResponse resp

