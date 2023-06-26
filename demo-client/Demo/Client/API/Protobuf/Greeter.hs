module Demo.Client.API.Protobuf.Greeter (
    sayHello
  , sayHelloStreamReply
  ) where

import Network.GRPC.Client
import Network.GRPC.Client.StreamType
import Network.GRPC.Common.StreamType

import Proto.Helloworld

import Demo.Common.Logging

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHello :: Connection -> HelloRequest -> IO ()
sayHello conn name = do
    reply <- nonStreaming (rpc @(Protobuf Greeter "sayHello") conn) name
    logMsg reply

sayHelloStreamReply :: Connection -> HelloRequest -> IO ()
sayHelloStreamReply conn name =
    serverStreaming (rpc @(Protobuf Greeter "sayHelloStreamReply") conn) name $
      logMsg
