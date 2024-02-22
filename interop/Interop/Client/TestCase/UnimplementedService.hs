module Interop.Client.TestCase.UnimplementedService (runTest) where

import Control.Exception

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Client.StreamType.IO

import Proto.Test

import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

type UnimplementedCall = Protobuf UnimplementedService "unimplementedCall"

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#unimplemented_service>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn -> do
      resp <- try $ nonStreaming conn (rpc @UnimplementedCall) defMessage
      case resp of
        Right _empty ->
          assertFailure "Expected UNIMPLEMENTED"
        Left err ->
          assertEqual GrpcUnimplemented $ grpcError err