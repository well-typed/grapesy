module Demo.Client.API.Protobuf.Greeter (
    sayHello
  , sayHelloStreamReply
  ) where

import Prelude hiding (log)

import Network.GRPC.Client
import Network.GRPC.Client.Protobuf

import Proto.Helloworld

import Demo.Client.Driver.Logging

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHello :: Connection -> CallParams -> HelloRequest -> IO ()
sayHello conn params n = log =<<
    nonStreaming conn params (RPC @Greeter @"sayHello") n

sayHelloStreamReply :: Connection -> CallParams -> HelloRequest -> IO ()
sayHelloStreamReply conn params n = log =<<
    serverStreaming conn params (RPC @Greeter @"sayHelloStreamReply") n log
