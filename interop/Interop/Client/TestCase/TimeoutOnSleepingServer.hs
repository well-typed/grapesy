module Interop.Client.TestCase.TimeoutOnSleepingServer (runTest) where

import Interop.Cmdline
import Interop.Util.Exceptions

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#timeout_on_sleeping_server>
runTest :: Cmdline -> IO ()
runTest _ = throwIO TestUnimplemented