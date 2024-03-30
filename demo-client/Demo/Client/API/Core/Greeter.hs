module Demo.Client.API.Core.Greeter (
    sayHelloStreamReply
  ) where

import Network.GRPC.Client
import Network.GRPC.Common

import Demo.Common.API
import Demo.Common.Logging

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHelloStreamReply :: Connection -> HelloRequest -> IO ()
sayHelloStreamReply conn name =
    withRPC conn def (Proxy @SayHelloStreamReply) $ \call -> do
      -- The server only sends a response once we send an input
      sendFinalInput call name

      -- We should see the response metadata immediately, and the first output
      -- after a delay.
      initMetadata <- recvResponseInitialMetadata call
      logMsg initMetadata

      -- For completeness, we also show the final metadata, although the
      -- example does not include any.
      finalMetadata <- recvAllOutputs call logMsg
      logMsg finalMetadata
