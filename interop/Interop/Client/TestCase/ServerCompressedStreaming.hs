module Interop.Client.TestCase.ServerCompressedStreaming (runTest) where

import Data.Maybe (isJust)

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Spec

import Interop.Client.Common
import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

import Proto.API.Interop

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#server_compressed_streaming>
runTest :: Cmdline -> IO ()
runTest cmdline = do
    withConnection def (testServer cmdline) $ \conn ->
      withRPC conn def (Proxy @StreamingOutputCall) $ \call -> do
        sendFinalInput call $ mkStreamingOutputCallRequest expected Nothing
        verifyStreamingOutputs call (\_ -> return ()) $ [
            \(envelope, resp) -> do
               assertEqual compressed $ isJust (inboundCompressedSize envelope)
               verifyStreamingOutputCallResponse sz resp
          | (compressed, sz) <- expected
          ]
  where
    expected :: [(Bool, Int)]
    expected = [(True, 31415), (False, 92653)]
