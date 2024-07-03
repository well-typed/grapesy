{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Demo.Server.Cmdline
import Demo.Server.Service.Greeter    qualified as Greeter
import Demo.Server.Service.Ping       qualified as Ping
import Demo.Server.Service.RouteGuide qualified as RouteGuide

import Demo.Common.API

import Paths_grapesy

{-------------------------------------------------------------------------------
  All services
-------------------------------------------------------------------------------}

services ::
     Cmdline
  -> [Proto Feature]
  -> Services IO [
         ProtobufMethodsOf Greeter
       , ProtobufMethodsOf RouteGuide
       , '[Ping]
       ]
services cmdline db =
      Service Greeter.handlers
    $ Service (hoistMethods runHandler $ RouteGuide.handlers cmdline)
    $ Service Ping.handlers
    $ NoMoreServices
  where
    runHandler :: RouteGuide.Handler a -> IO a
    runHandler = RouteGuide.runHandler db

{-------------------------------------------------------------------------------
  Application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline <- getCmdline
    db      <- getRouteGuideDb

    let serverConfig :: ServerConfig
        serverConfig = ServerConfig {
            serverInsecure = cmdInsecure cmdline
          , serverSecure   = cmdSecure cmdline
          }

    runServerWithHandlers
      (serverParams cmdline)
      serverConfig
      (fromServices $ services cmdline db)

getRouteGuideDb :: IO [Proto Feature]
getRouteGuideDb = do
    path <- getDataFileName "route_guide_db.json"
    mDb  <- decodeFileStrict path
    case mDb of
      Just db -> return db
      Nothing -> error "Could not parse the route guide DB"

serverParams :: Cmdline -> ServerParams
serverParams cmd = def {
      serverCompression =
        if cmdDisableCompression cmd
          then Compression.none
          else serverCompression def
    }

