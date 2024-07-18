module Interop.Client.TestCase.PingPong (runTest) where

import Control.Monad

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Interop.API
import Interop.Client.Common
import Interop.Client.Connect
import Interop.Cmdline

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#ping_pong>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn ->
      withRPC conn def (Proxy @FullDuplexCall) $ \call -> do
        resps <- collectResponses call [
                     mkStreamingOutputCallRequest
                       [(False, expectedResponseSize)]
                       (Just payloadSize)
                   | (expectedResponseSize, payloadSize) <- steps
                   ]

        -- 'collectResponses' will throw an error if we don't receive the
        -- expected number of responses, so we don't need to check that again
        zipWithM_ (verifyStreamingOutputCallResponse . fst) steps resps
  where
    -- Expected response size and payload size of each step of the test
    steps :: [(Int, Int)]
    steps = [
          (31415, 27182)
        , (9, 8)
        , (2653, 1828)
        , (58979, 45904)
        ]

    collectResponses ::
         Call FullDuplexCall
      -> [Proto StreamingOutputCallRequest]
      -> IO [Proto StreamingOutputCallResponse]
    collectResponses call = go
      where
        go ::
             [Proto StreamingOutputCallRequest]
          -> IO [Proto StreamingOutputCallResponse]
        go []     = error "impossible"
        go [r]    = do sendFinalInput call r
                       (resp, _metadata) <- recvFinalOutput call
                       return [resp]
        go (r:rs) = do sendNextInput call r
                       resp <- recvNextOutput call
                       (resp:) <$> go rs
