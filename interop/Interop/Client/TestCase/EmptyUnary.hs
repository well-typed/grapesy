module Interop.Client.TestCase.EmptyUnary (runTest) where

import Data.Proxy

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Spec

import Interop.API
import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#empty_unary>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn ->
      withRPC conn def (Proxy @EmptyCall) $ \call -> do
        sendFinalInput call empty
        streamElem :: StreamElem ProperTrailers' (InboundEnvelope, Proto Empty)
          <- recvOutputWithEnvelope call

        -- The test description asks us to also verify the size of the /outgoing/
        -- message if possible. This information is not readily available in
        -- @grapesy@, but we will test it implicitly when running the @grapesy@
        -- interop client against the @grapesy@ interop server.

        case StreamElem.value streamElem of
          Just (envelope, resp) -> do
            assertEqual empty $ resp
            assertEqual 0     $ inboundUncompressedSize envelope
          Nothing ->
            assertFailure "Expected response"
  where
    empty :: Proto Empty
    empty = defMessage
