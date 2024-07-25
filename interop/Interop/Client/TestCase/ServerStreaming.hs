module Interop.Client.TestCase.ServerStreaming (runTest) where

import Network.GRPC.Client
import Network.GRPC.Common

import Interop.Client.Common
import Interop.Client.Connect
import Interop.Cmdline

import Proto.API.Interop

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#server_streaming>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn ->
      withRPC conn def (Proxy @StreamingOutputCall) $ \call -> do
        sendFinalInput call $ mkStreamingOutputCallRequest expected Nothing
        verifyStreamingOutputs call (\_ -> return ()) $ [
            \(_meta, resp) -> verifyStreamingOutputCallResponse sz resp
          | (_compressed, sz) <- expected
          ]
  where
    expected :: [(Bool, Int)]
    expected = map (False,) $ [31415, 9, 2653, 58979]
