module Interop.Client.TestCase.UnimplementedMethod (runTest) where

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Client.StreamType.IO

import Interop.API
import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#unimplemented_method>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn -> do
      assertThrows (assertEqual GrpcUnimplemented . grpcError) $
        nonStreaming conn (rpc @UnimplementedCall) defMessage
