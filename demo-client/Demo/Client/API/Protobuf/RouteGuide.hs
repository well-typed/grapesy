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

import Demo.Common.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide
-------------------------------------------------------------------------------}

getFeature :: Connection -> Point -> IO ()
getFeature conn point = do
    features <- nonStreaming @RouteGuide @"getFeature" (rpc conn) point
    log features

listFeatures :: Connection -> Rectangle -> IO ()
listFeatures conn rect = do
    metadata <- serverStreaming @RouteGuide @"listFeatures" (rpc conn) rect log
    log (metadata :: [CustomMetadata])

recordRoute :: Connection -> IO (StreamElem () Point) -> IO ()
recordRoute conn getPoint = do
    summary <- clientStreaming @RouteGuide @"recordRoute" (rpc conn) getPoint
    log summary

routeChat :: Connection -> IO (StreamElem () RouteNote) -> IO ()
routeChat conn getNote = do
    metadata <- biDiStreaming @RouteGuide @"routeChat" (rpc conn) getNote log
    log (metadata :: [CustomMetadata])
