module Interop.Client.TestCase.CancelAfterFirstResponse (runTest) where

import Interop.Cmdline
import Interop.Util.Exceptions

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#cancel_after_first_response>
runTest :: Cmdline -> IO ()
runTest _ = throwIO TestUnimplemented