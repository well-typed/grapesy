module Demo.Client.API.Protobuf.IO.Greeter (
    sayHello
  , sayHelloStreamReply
  ) where

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO

import Proto.Helloworld

import Demo.Common.Logging

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHello :: Connection -> HelloRequest -> IO ()
sayHello conn name = do
    reply <- nonStreaming conn (rpc @(Protobuf Greeter "sayHello")) name
    logMsg reply

sayHelloStreamReply :: Connection -> HelloRequest -> IO ()
sayHelloStreamReply conn name =
    serverStreaming conn (rpc @(Protobuf Greeter "sayHelloStreamReply")) name $
      logMsg
