module Main (main) where

import Network.GRPC.Server
import Network.HTTP2.Server qualified as HTTP2
import Network.Run.TCP

import Proto.Helloworld
import Control.Exception

handlersGreeter :: MethodHandlersFor Greeter
handlersGreeter =
      HandleMethod handleSayHello
    $ HandleMethod handleSayHelloStreamReply
    $ AllMethodsHandled

handleSayHello :: MethodHandler Greeter "sayHello"
handleSayHello = MethodHandler $ do
    putStrLn "handleSayHello"

handleSayHelloStreamReply :: MethodHandler Greeter "sayHelloStreamReply"
handleSayHelloStreamReply = MethodHandler $ do
    putStrLn "handleSayHelloStreamReply"

serviceHandlers :: ServiceHandlers '[Greeter]
serviceHandlers =
     HandleService handlersGreeter
   $ AllServicesHandled

main :: IO ()
main =
    runTCPServer Nothing "50051" $ \s ->
      bracket (HTTP2.allocSimpleConfig s 4096)
              HTTP2.freeSimpleConfig
              (\config -> HTTP2.run config $ server serviceHandlers)
