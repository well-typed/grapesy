module Demo.Client.API.Protobuf.IO.RouteGuide (
    getFeature
  , listFeatures
  , recordRoute
  , routeChat
  ) where

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.RouteGuide

import Demo.Common.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide
-------------------------------------------------------------------------------}

getFeature :: Connection -> Point -> IO ()
getFeature conn point = do
    features <-
      nonStreaming conn (rpc @(Protobuf RouteGuide "getFeature")) point
    logMsg features

listFeatures :: Connection -> Rectangle -> IO ()
listFeatures conn rect = do
    serverStreaming conn (rpc @(Protobuf RouteGuide "listFeatures")) rect $
      logMsg

recordRoute :: Connection -> IO (StreamElem NoMetadata Point) -> IO ()
recordRoute conn getPoint = do
    summary <-
      clientStreaming conn (rpc @(Protobuf RouteGuide "recordRoute")) getPoint
    logMsg summary

routeChat :: Connection -> IO (StreamElem NoMetadata RouteNote) -> IO ()
routeChat conn getNote = do
    biDiStreaming conn (rpc @(Protobuf RouteGuide "routeChat")) getNote $
      logMsg
