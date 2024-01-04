module Demo.Client.API.Core.Greeter (
    sayHelloStreamReply
  ) where

import Data.Default
import Data.Proxy

import Network.GRPC.Client

import Proto.Helloworld

import Demo.Common.Logging

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHelloStreamReply :: Connection -> HelloRequest -> IO ()
sayHelloStreamReply conn name =
    withRPC conn def (Proxy @(Protobuf Greeter "sayHelloStreamReply")) $ \call -> do
      -- The server only sends a response once we send an input
      sendFinalInput call name

      -- We should see the response metadata immediately, and the first output
      -- after a delay.
      initMetadata <- recvResponseMetadata call
      logMsg initMetadata

      -- For completeness, we also show the final metadata, although the
      -- example does not include any.
      finalMetadata <- recvAllOutputs call logMsg
      logMsg finalMetadata
