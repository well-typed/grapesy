module Interop.Client.TestCase.EmptyStream (runTest) where

import Control.Monad

import Network.GRPC.Client
import Network.GRPC.Common

import Interop.API
import Interop.Client.Connect
import Interop.Cmdline

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#empty_stream>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn ->
      withRPC conn def (Proxy @FullDuplexCall) $ \call -> do
        sendEndOfInput call
        void $ recvTrailers call

