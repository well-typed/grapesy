module Demo.Client.API.StreamType.IO.RouteGuide (
    getFeature
  , listFeatures
  , recordRoute
  , routeChat
  ) where

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Demo.Common.API
import Demo.Common.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide
-------------------------------------------------------------------------------}

getFeature :: Connection -> Proto Point -> IO ()
getFeature conn point = do
    features <- nonStreaming conn (rpc @GetFeature) point
    logMsg features

listFeatures :: Connection -> Proto Rectangle -> IO ()
listFeatures conn rect =
    serverStreaming conn (rpc @ListFeatures) rect logMsg

recordRoute :: Connection -> IO (StreamElem NoMetadata (Proto Point)) -> IO ()
recordRoute conn getPoint = do
    summary <- clientStreaming conn (rpc @RecordRoute) getPoint
    logMsg summary

routeChat :: Connection -> IO (StreamElem NoMetadata (Proto RouteNote)) -> IO ()
routeChat conn getNote =
    biDiStreaming conn (rpc @RouteChat) getNote logMsg
