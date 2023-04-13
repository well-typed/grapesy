module Demo.GRPC.Protobuf.RouteGuide (
    getFeature
  , listFeatures
  , recordRoute
  , routeChat
  ) where

import Prelude hiding (log)

import Network.GRPC.Client
import Network.GRPC.Protobuf
import Network.GRPC.Spec

import Proto.RouteGuide

import Demo.Driver.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide
-------------------------------------------------------------------------------}

getFeature :: Connection -> RequestMeta -> Point -> IO ()
getFeature conn meta p = log =<<
    nonStreaming conn meta (RPC @RouteGuide @"getFeature") p

listFeatures :: Connection -> RequestMeta -> Rectangle -> IO ()
listFeatures conn meta r = log =<<
    serverStreaming conn meta (RPC @RouteGuide @"listFeatures") r log

recordRoute :: Connection -> RequestMeta -> IO (IsFinal, Point) -> IO ()
recordRoute conn meta ps = log =<<
    clientStreaming conn meta (RPC @RouteGuide @"recordRoute") ps

routeChat :: Connection -> RequestMeta -> IO (IsFinal, RouteNote)-> IO ()
routeChat conn meta notes = log =<<
    biDiStreaming conn meta (RPC @RouteGuide @"routeChat") notes log
