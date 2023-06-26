module Demo.Client.API.Protobuf.RouteGuide (
    getFeature
  , listFeatures
  , recordRoute
  , routeChat
  ) where

import Network.GRPC.Client
import Network.GRPC.Client.StreamType
import Network.GRPC.Common.StreamType

import Proto.RouteGuide

import Demo.Common.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide
-------------------------------------------------------------------------------}

getFeature :: Connection -> Point -> IO ()
getFeature conn point = do
    features <-
      nonStreaming (rpc @(Protobuf RouteGuide "getFeature") conn) point
    logMsg features

listFeatures :: Connection -> Rectangle -> IO ()
listFeatures conn rect = do
    serverStreaming (rpc @(Protobuf RouteGuide "listFeatures") conn) rect $
      logMsg

recordRoute :: Connection -> IO (StreamElem () Point) -> IO ()
recordRoute conn getPoint = do
    summary <-
      clientStreaming (rpc @(Protobuf RouteGuide "recordRoute") conn) getPoint
    logMsg summary

routeChat :: Connection -> IO (StreamElem () RouteNote) -> IO ()
routeChat conn getNote = do
    biDiStreaming (rpc @(Protobuf RouteGuide "routeChat") conn) getNote $
      logMsg
