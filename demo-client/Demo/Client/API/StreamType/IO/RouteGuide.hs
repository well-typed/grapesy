module Demo.Client.API.StreamType.IO.RouteGuide (
    getFeature
  , listFeatures
  , recordRoute
  , routeChat
  ) where

import Control.Concurrent.Async (concurrently_)

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common
import Network.GRPC.Common.NextElem qualified as NextElem
import Network.GRPC.Common.Protobuf

import Demo.Client.Util.DelayOr (DelayOr)
import Demo.Client.Util.DelayOr qualified as DelayOr
import Demo.Client.Util.Logging
import Demo.Common.API

{-------------------------------------------------------------------------------
  routeguide.RouteGuide
-------------------------------------------------------------------------------}

getFeature :: Connection -> Proto Point -> IO ()
getFeature conn point = do
    features <- nonStreaming conn (rpc @GetFeature) point
    logMsg features

listFeatures :: Connection -> Proto Rectangle -> IO ()
listFeatures conn rect =
    serverStreaming conn (rpc @ListFeatures) rect $ \recv ->
      NextElem.whileNext_ recv logMsg

recordRoute :: Connection -> [DelayOr (Proto Point)] -> IO ()
recordRoute conn points = do
    summary <- clientStreaming_ conn (rpc @RecordRoute) $ \send -> do
                 DelayOr.forM_ points (send . NextElem)
                 send NoNextElem
    logMsg summary

routeChat :: Connection -> [DelayOr (Proto RouteNote)] -> IO ()
routeChat conn notes =
    biDiStreaming conn (rpc @RouteChat) $ \send recv ->
      concurrently_
        (DelayOr.mapM_ (send . NextElem) notes >> send NoNextElem)
        (NextElem.whileNext_ recv print)
