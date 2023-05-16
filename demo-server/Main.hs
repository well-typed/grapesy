module Main (main) where

import Control.Exception
import Control.Monad.STM
import Network.HTTP2.Server qualified as HTTP2
import Network.Run.TCP

import Network.GRPC.Server
import Network.GRPC.Server.Protobuf

import Proto.Helloworld

handlers :: ServiceHandlers '[Greeter]
handlers =
      HandleService handleGreeter
    $ AllServicesHandled

handleGreeter :: ServiceHandler Greeter
handleGreeter = ServiceHandler $
      HandleMethod (HandleNonStreaming sayHello)
    $ HandleMethod (HandleServerStreaming sayHelloStreamReply)
    $ AllMethodsHandled

sayHello :: HelloRequest -> IO (HelloReply, Trailers)
sayHello _ = do
    putStrLn "sayHello"
    return undefined

sayHelloStreamReply :: HelloRequest -> (HelloReply -> STM ()) -> IO Trailers
sayHelloStreamReply _ _ = do
    putStrLn "sayHelloStreamReply"
    return undefined

main :: IO ()
main =
    runTCPServer Nothing "50051" $ \s ->
      bracket (HTTP2.allocSimpleConfig s 4096)
              HTTP2.freeSimpleConfig $ \config ->
        HTTP2.run config $ server $ protobufHandlers handlers
