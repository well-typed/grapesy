module Interop.Client.TestCase.CancelAfterBegin (runTest) where

import Network.GRPC.Client
import Network.GRPC.Common

import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

import Proto.API.Interop

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#cancel_after_begin>
--
-- This is not really testing anything about the server, but rather about how
-- cancellation gets reported by the grapesy client library itself.
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn ->
      assertThrows (assertEqual GrpcCancelled . grpcError) $
        withRPC conn def (Proxy @StreamingInputCall) $ \_call ->
          -- Immediately cancel request
          return ()

