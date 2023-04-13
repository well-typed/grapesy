module Demo.GRPC.Protobuf.Greeter (
    sayHello
  , sayHelloStreamReply
  ) where

import Prelude hiding (log)

import Network.GRPC.Client
import Network.GRPC.Protobuf
import Network.GRPC.Spec

import Proto.Helloworld

import Demo.Driver.Logging

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHello :: Connection -> RequestMeta -> HelloRequest -> IO ()
sayHello conn meta n = log =<<
    nonStreaming conn meta (RPC @Greeter @"sayHello") n

sayHelloStreamReply :: Connection -> RequestMeta -> HelloRequest -> IO ()
sayHelloStreamReply conn meta n = log =<<
    serverStreaming conn meta (RPC @Greeter @"sayHelloStreamReply") n log
