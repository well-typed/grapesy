module Demo.Client.API.StreamType.Pipes.RouteGuide (
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

import Demo.Common.API
import Demo.Common.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide

  We do not include the 'getFeature' method, as it does not do any streaming.
-------------------------------------------------------------------------------}

listFeatures :: Connection -> Proto Rectangle -> IO ()
listFeatures conn r = runSafeT . runEffect $
    (prod >>= logMsg) >-> Pipes.mapM_ logMsg
  where
    prod :: Producer (Proto Feature) (SafeT IO) ()
    prod = serverStreaming conn (rpc @ListFeatures) r

recordRoute ::
     Connection
  -> Producer' (StreamElem NoMetadata (Proto Point)) (SafeT IO) ()
  -> IO ()
recordRoute conn ps = runSafeT . runEffect $
    ps >-> (cons >>= logMsg)
  where
    cons :: Consumer
              (StreamElem NoMetadata (Proto Point))
              (SafeT IO)
              (Proto RouteSummary)
    cons = clientStreaming conn (rpc @RecordRoute)

routeChat ::
     Connection
  -> Producer' (StreamElem NoMetadata (Proto RouteNote)) IO ()
  -> IO ()
routeChat conn ns =
    biDiStreaming conn def (Proxy @RouteChat) aux
  where
    aux ::
         Consumer' (StreamElem NoMetadata (Proto RouteNote)) IO ()
      -> Producer' (Proto RouteNote) IO ()
      -> IO ()
    aux cons prod =
        concurrently_
          (runEffect $ ns >-> cons)
          (runEffect $ (prod >>= logMsg) >-> Pipes.mapM_ logMsg)
