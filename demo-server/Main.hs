{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Tracer
import Data.Aeson
import Data.Default

import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Demo.Common.Logging

import Demo.Server.Cmdline
import Demo.Server.Service.Greeter    qualified as Greeter
import Demo.Server.Service.RouteGuide qualified as RouteGuide

import Proto.Helloworld
import Proto.RouteGuide

import Paths_grapesy

{-------------------------------------------------------------------------------
  All services
-------------------------------------------------------------------------------}

services ::
     Cmdline
  -> [Feature]
  -> Services IO (ProtobufServices '[Greeter, RouteGuide])
services cmdline db =
      Service Greeter.handlers
    $ Service (RouteGuide.handlers cmdline db)
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
            serverTracer =
              if cmdDebug cmdline
                then contramap show threadSafeTracer
                else nullTracer
          , serverInsecure =
              cmdInsecure cmdline
          , serverSecure   =
              cmdSecure cmdline
          }

    withServer (serverParams cmdline) (fromServices $ services cmdline db) $
      runServer serverConfig

getRouteGuideDb :: IO [Feature]
getRouteGuideDb = do
    path <- getDataFileName "route_guide_db.json"
    mDb  <- decodeFileStrict path
    case mDb of
      Just db -> return db
      Nothing -> error "Could not parse the route guide DB"

serverParams :: Cmdline -> ServerParams
serverParams cmd = ServerParams {
      serverDebugTracer =
        if cmdDebug cmd
          then contramap show threadSafeTracer
          else serverDebugTracer def

    , serverCompression =
        def
    }

