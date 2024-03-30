module Demo.Client.API.Protobuf.IO.RouteGuide (
    getFeature
  , listFeatures
  , recordRoute
  , routeChat
  ) where

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common

import Demo.Common.API
import Demo.Common.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide
-------------------------------------------------------------------------------}

getFeature :: Connection -> Point -> IO ()
getFeature conn point = do
    features <- nonStreaming conn (rpc @GetFeature) point
    logMsg features

listFeatures :: Connection -> Rectangle -> IO ()
listFeatures conn rect =
    serverStreaming conn (rpc @ListFeatures) rect logMsg

recordRoute :: Connection -> IO (StreamElem NoMetadata Point) -> IO ()
recordRoute conn getPoint = do
    summary <- clientStreaming conn (rpc @RecordRoute) getPoint
    logMsg summary

routeChat :: Connection -> IO (StreamElem NoMetadata RouteNote) -> IO ()
routeChat conn getNote =
    biDiStreaming conn (rpc @RouteChat) getNote logMsg
