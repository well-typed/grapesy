module Interop.Client.TestCase.CancelAfterFirstResponse (runTest) where

import Network.GRPC.Client
import Network.GRPC.Common

import Interop.API
import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions
import Interop.Client.Common (mkStreamingOutputCallRequest)

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#cancel_after_first_response>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn -> do
      assertThrows (assertEqual GrpcCancelled . grpcError) $
        withRPC conn def (Proxy @FullDuplexCall) $ \call -> do
          sendNextInput call req
          -- Cancel request after receiving a response
          _resp <- recvNextOutput call
          return ()
  where
    req :: Proto StreamingOutputCallRequest
    req = mkStreamingOutputCallRequest [(False, 31415)] (Just 27182)

