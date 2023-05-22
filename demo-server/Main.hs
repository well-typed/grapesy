module Main (main) where

import Control.Exception
import Control.Monad.STM
import Network.HTTP2.Server qualified as HTTP2
import Network.Run.TCP

import Network.GRPC.Server
import Network.GRPC.Server.Protobuf

import Proto.Helloworld
import Proto.RouteGuide

{-------------------------------------------------------------------------------
  Greeter
-------------------------------------------------------------------------------}

handleGreeter :: MethodsOf Greeter
handleGreeter = Methods $
      Method handleSayHello
    $ Method handleSayHelloStreamReply
    $ NoMoreMethods

handleSayHello :: HelloRequest -> IO (HelloReply, Trailers)
handleSayHello _req = do
    putStrLn "sayHello"
    return undefined

handleSayHelloStreamReply ::
     HelloRequest
  -> (HelloReply -> STM ())
  -> IO Trailers
handleSayHelloStreamReply _req _respond = do
    putStrLn "sayHelloStreamReply"
    return undefined

{-------------------------------------------------------------------------------
  Route Guide
-------------------------------------------------------------------------------}

handleRouteGuide :: MethodsOf RouteGuide
handleRouteGuide = undefined

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

services :: Services '[Greeter, RouteGuide]
services =
      Service handleGreeter
    $ Service handleRouteGuide
    $ NoMoreServices

main :: IO ()
main =
    runTCPServer Nothing "50051" $ \s ->
      bracket (HTTP2.allocSimpleConfig s 4096)
              HTTP2.freeSimpleConfig $ \config ->
        HTTP2.run config $ server $ protobufServices services


