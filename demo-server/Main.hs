{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Tracer
import Data.Aeson
import Data.Default

import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run

import Demo.Common.Logging

import Demo.Server.API.Protobuf.Greeter    qualified as Greeter
import Demo.Server.API.Protobuf.RouteGuide qualified as RouteGuide
import Demo.Server.Cmdline

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
    cmdline <- getCmdline
    db      <- getRouteGuideDb

    let serverConfig :: ServerConfig
        serverConfig = ServerConfig {
            serverTracer   = contramap show threadSafeTracer
          , serverInsecure = cmdInsecure cmdline
          , serverSecure   = cmdSecure   cmdline
          }

    withServer serverParams (protobufServices (services db)) $
      runServer serverConfig

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

