module Main (main) where

import Control.Exception
import Control.Tracer
import Data.Aeson
import Data.Default
import Data.ProtoLens.Labels ()
import Network.HTTP2.Server qualified as HTTP2
import Network.Run.TCP

import Network.GRPC.Server
import Network.GRPC.Server.Protobuf

import Demo.Common.Logging
import Demo.Server.Aux.RouteGuide ()

import Demo.Server.API.Protobuf.Greeter    qualified as Greeter
import Demo.Server.API.Protobuf.RouteGuide qualified as RouteGuide

import Proto.Helloworld
import Proto.RouteGuide

import Paths_grapesy

{-------------------------------------------------------------------------------
  All services
-------------------------------------------------------------------------------}

services :: [Feature] -> Services IO '[Greeter, RouteGuide]
services db =
      Service Greeter.handlers
    $ Service (RouteGuide.handlers db)
    $ NoMoreServices

{-------------------------------------------------------------------------------
  Application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    db <- getRouteGuideDb
    runTCPServer Nothing "50051" $ \s ->
      bracket (HTTP2.allocSimpleConfig s 4096)
              HTTP2.freeSimpleConfig $ \config ->
        withServer serverParams (protobufServices (services db)) $ \server ->
          HTTP2.run config server

getRouteGuideDb :: IO [Feature]
getRouteGuideDb = do
    path <- getDataFileName "route_guide_db.json"
    mDb  <- decodeFileStrict path
    case mDb of
      Just db -> return db
      Nothing -> error "Could not parse the route guide DB"

serverParams :: ServerParams
serverParams = ServerParams {
      serverTracer      = contramap show threadSafeTracer
    , serverCompression = def
    }


