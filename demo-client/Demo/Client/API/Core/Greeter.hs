module Demo.Client.API.Core.Greeter (
    sayHelloStreamReply
  ) where

import Control.Concurrent.STM
import Data.Default

import Network.GRPC.Client
import Network.GRPC.Client.Protobuf (RPC(..))

import Proto.Helloworld

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHelloStreamReply :: Connection -> HelloRequest -> IO ()
sayHelloStreamReply conn name =
    withRPC conn def (RPC @Greeter @"sayHelloStreamReply") $ \call -> do
      -- The server only sends a response once we send an input
      sendOnlyInput call name

      -- We should see the response metadata immediately, and the first output
      -- after a delay.
      initMetadata <- atomically $ callResponseMetadata call
      print initMetadata

      -- For completeness, we also show the final metadata, although the
      -- example does not include any.
      finalMetadata <- recvAllOutputs call print
      print finalMetadata
