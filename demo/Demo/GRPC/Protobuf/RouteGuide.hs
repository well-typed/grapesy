module Demo.GRPC.Protobuf.RouteGuide (
    getFeature
  , listFeatures
  , recordRoute
  , routeChat
  ) where

import Prelude hiding (log)

import Network.GRPC.Client
import Network.GRPC.Protobuf

import Proto.RouteGuide

import Demo.Driver.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide
-------------------------------------------------------------------------------}

getFeature :: Connection -> PerCallParams -> Point -> IO ()
getFeature conn params p = log =<<
    nonStreaming conn params (RPC @RouteGuide @"getFeature") p

listFeatures :: Connection -> PerCallParams -> Rectangle -> IO ()
listFeatures conn params r = log =<<
    serverStreaming conn params (RPC @RouteGuide @"listFeatures") r log

recordRoute :: Connection -> PerCallParams -> IO (IsFinal, Point) -> IO ()
recordRoute conn params ps = log =<<
    clientStreaming conn params (RPC @RouteGuide @"recordRoute") ps

routeChat :: Connection -> PerCallParams -> IO (IsFinal, RouteNote)-> IO ()
routeChat conn params notes = log =<<
    biDiStreaming conn params (RPC @RouteGuide @"routeChat") notes log
