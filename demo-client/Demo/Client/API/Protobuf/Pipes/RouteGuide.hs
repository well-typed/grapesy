module Demo.Client.API.Protobuf.Pipes.RouteGuide (
    listFeatures
  , recordRoute
  , routeChat
  ) where

import Control.Concurrent.Async (concurrently_)
import Pipes hiding (Proxy)
import Pipes.Prelude qualified as Pipes
import Pipes.Safe

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.Pipes
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.RouteGuide

import Demo.Common.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide

  We do not include the 'getFeature' method, as it does not do any streaming.
-------------------------------------------------------------------------------}

listFeatures :: Connection -> Rectangle -> IO ()
listFeatures conn r = runSafeT . runEffect $
    (prod >>= logMsg) >-> Pipes.mapM_ logMsg
  where
    prod :: Producer Feature (SafeT IO) ()
    prod = serverStreaming conn (rpc @(Protobuf RouteGuide "listFeatures")) r

recordRoute ::
     Connection
  -> Producer' (StreamElem NoMetadata Point) (SafeT IO) ()
  -> IO ()
recordRoute conn ps = runSafeT . runEffect $
    ps >-> (cons >>= logMsg)
  where
    cons :: Consumer (StreamElem NoMetadata Point) (SafeT IO) RouteSummary
    cons = clientStreaming conn (rpc @(Protobuf RouteGuide "recordRoute"))

routeChat ::
     Connection
  -> Producer' (StreamElem NoMetadata RouteNote) IO ()
  -> IO ()
routeChat conn ns =
    biDiStreaming conn def (Proxy @(Protobuf RouteGuide "routeChat")) aux
  where
    aux ::
         Consumer' (StreamElem NoMetadata RouteNote) IO ()
      -> Producer' RouteNote IO ()
      -> IO ()
    aux cons prod =
        concurrently_
          (runEffect $ ns >-> cons)
          (runEffect $ (prod >>= logMsg) >-> Pipes.mapM_ logMsg)
