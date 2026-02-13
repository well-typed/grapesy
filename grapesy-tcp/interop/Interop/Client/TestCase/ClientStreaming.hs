module Interop.Client.TestCase.ClientStreaming (runTest) where

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions
import Interop.Util.Messages

import Proto.API.Interop

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#client_streaming>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn ->
      withRPC conn def (Proxy @StreamingInputCall) $ \call -> do
        sendNextInput  call $ request 27182
        sendNextInput  call $ request 8
        sendNextInput  call $ request 1828
        sendFinalInput call $ request 45904
        (resp, _metadata) <- recvFinalOutput call
        assertEqual 74922 $ resp ^. #aggregatedPayloadSize
  where
    request :: Int -> Proto StreamingInputCallRequest
    request sz = defMessage & #payload .~ payloadOfZeroes sz
