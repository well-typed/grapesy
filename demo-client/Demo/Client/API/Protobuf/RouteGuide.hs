module Demo.Client.API.Protobuf.RouteGuide (
    getFeature
  , listFeatures
  , recordRoute
  , routeChat
  ) where

import Prelude hiding (log)

import Network.GRPC.Client
import Network.GRPC.Client.Protobuf

import Proto.RouteGuide

import Demo.Client.Driver.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide
-------------------------------------------------------------------------------}

getFeature :: Connection -> CallParams -> Point -> IO ()
getFeature conn params p = log =<<
    nonStreaming conn params (RPC @RouteGuide @"getFeature") p

listFeatures :: Connection -> CallParams -> Rectangle -> IO ()
listFeatures conn params r = log =<<
    serverStreaming conn params (RPC @RouteGuide @"listFeatures") r log

recordRoute :: Connection -> CallParams -> IO (IsFinal, Maybe Point) -> IO ()
recordRoute conn params ps = log =<<
    clientStreaming conn params (RPC @RouteGuide @"recordRoute") ps

routeChat :: Connection -> CallParams -> IO (IsFinal, Maybe RouteNote) -> IO ()
routeChat conn params notes = log =<<
    biDiStreaming conn params (RPC @RouteGuide @"routeChat") notes log
