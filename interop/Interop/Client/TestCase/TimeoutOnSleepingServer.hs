module Interop.Client.TestCase.TimeoutOnSleepingServer (runTest) where

import Network.GRPC.Client
import Network.GRPC.Common

import Interop.API
import Interop.Client.Common
import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#timeout_on_sleeping_server>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn -> do
      assertThrows (assertEqual GrpcDeadlineExceeded . grpcError) $
        assertTerminatesWithinSeconds 1 $
          withRPC conn callParams (Proxy @FullDuplexCall) $ \call -> do
            sendNextInput call req
            -- We wait for a response, but we didn't request any, so we never
            -- get one; meanwhile, the server is waiting for the /next/
            -- 'StreamingOutputCallRequest'. Therefore, the call is only closed
            -- once the timeout is reached.
            _resp <- recvNextOutput call
            return ()
  where
    callParams :: CallParams FullDuplexCall
    callParams = def {
          callTimeout = Just $ Timeout Millisecond (TimeoutValue 1)
        }

    req :: StreamingOutputCallRequest
    req = mkStreamingOutputCallRequest [] (Just 27182)
