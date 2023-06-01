module Demo.Client.API.Protobuf.Greeter (
    sayHello
  , sayHelloStreamReply
  ) where

import Prelude hiding (log)

import Network.GRPC.Client
import Network.GRPC.Client.Protobuf

import Proto.Helloworld

import Demo.Common.Logging

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHello :: Connection -> HelloRequest -> IO ()
sayHello conn name = do
    reply <- nonStreaming @Greeter @"sayHello" (rpc conn) name
    log reply

sayHelloStreamReply :: Connection -> HelloRequest -> IO ()
sayHelloStreamReply conn name = do
    metadata <- serverStreaming @Greeter @"sayHelloStreamReply" (rpc conn) name print
    log (metadata :: [CustomMetadata])
