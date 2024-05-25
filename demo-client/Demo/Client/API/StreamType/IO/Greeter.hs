module Demo.Client.API.StreamType.IO.Greeter (
    sayHello
  , sayHelloStreamReply
  ) where

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common.Protobuf

import Demo.Common.API
import Demo.Common.Logging

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHello :: Connection -> Proto HelloRequest -> IO ()
sayHello conn name = do
    reply <- nonStreaming conn (rpc @SayHello) name
    logMsg reply

sayHelloStreamReply :: Connection -> Proto HelloRequest -> IO ()
sayHelloStreamReply conn name =
    serverStreaming conn (rpc @SayHelloStreamReply) name logMsg
