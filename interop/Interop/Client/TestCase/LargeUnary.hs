module Interop.Client.TestCase.LargeUnary (runTest) where

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common

import Interop.API
import Interop.Client.Common
import Interop.Client.Connect
import Interop.Cmdline

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#large_unary>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn -> do
      resp :: SimpleResponse <- nonStreaming conn (rpc @UnaryCall) req
      verifySimpleResponse resp
  where
    req :: SimpleRequest
    req = mkSimpleRequest False
