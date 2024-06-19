module Demo.Client.API.StreamType.Conduit.RouteGuide (
    listFeatures
  , recordRoute
  , routeChat
  ) where

import Control.Concurrent.Async (concurrently_)
import Data.Conduit
import Data.Conduit.Combinators qualified as Conduit (mapM_)

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.Conduit
import Network.GRPC.Common.Protobuf

import Demo.Client.Util.DelayOr (DelayOr)
import Demo.Client.Util.DelayOr qualified as DelayOr
import Demo.Common.API
import Demo.Common.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide

  We do not include the 'getFeature' method, as it does not do any streaming.
-------------------------------------------------------------------------------}

listFeatures :: Connection -> Proto Rectangle -> IO ()
listFeatures conn r =
    serverStreaming conn (rpc @ListFeatures) r $ \source ->
      runConduit $ source .| Conduit.mapM_ logMsg

recordRoute :: Connection -> [DelayOr (Proto Point)] -> IO ()
recordRoute conn ps = do
    summary <- clientStreaming_ conn (rpc @RecordRoute) $ \sink ->
       runConduit $ DelayOr.source ps .| sink
    logMsg summary

routeChat :: Connection -> [DelayOr (Proto RouteNote)] -> IO ()
routeChat conn ns = do
    biDiStreaming conn (rpc @RouteChat) $ \sink source ->
      concurrently_
        (runConduit $ DelayOr.source ns .| sink)
        (runConduit $ source .| Conduit.mapM_ logMsg)
