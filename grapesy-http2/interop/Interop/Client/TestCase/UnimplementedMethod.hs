module Interop.Client.TestCase.UnimplementedMethod (runTest) where

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Client.StreamType.IO

import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

import Proto.API.Interop

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#unimplemented_method>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn -> do
      assertThrows (assertEqual GrpcUnimplemented . grpcError) $
        nonStreaming conn (rpc @UnimplementedCall) defMessage
