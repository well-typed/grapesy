module Demo.Client.API.Protobuf.Pipes.RouteGuide (
    listFeatures
  , recordRoute
  , routeChat
  ) where

import Prelude hiding (log)

import Control.Concurrent.Async
import Data.Default
import Pipes
import Pipes.Prelude qualified as Pipes
import Pipes.Safe

import Network.GRPC.Client
import Network.GRPC.Client.Protobuf.Pipes

import Proto.RouteGuide

import Demo.Common.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide

  We do not include the 'getFeature' method, as it does not do any streaming.
-------------------------------------------------------------------------------}

listFeatures :: Connection -> Rectangle -> IO ()
listFeatures conn r = runSafeT . runEffect $
    let prod = serverStreaming conn def (RPC @RouteGuide @"listFeatures") r
    in (prod >>= log) >-> Pipes.mapM_ log

recordRoute ::
     Connection
  -> Producer' (StreamElem () Point) (SafeT IO) ()
  -> IO ()
recordRoute conn ps = runSafeT . runEffect $
    let cons = clientStreaming conn def (RPC @RouteGuide @"recordRoute")
    in ps >-> (cons >>= log)

routeChat ::
     Connection
  -> Producer' (StreamElem () RouteNote) IO ()
  -> IO ()
routeChat conn ns =
    biDiStreaming conn def (RPC @RouteGuide @"routeChat") $ \cons prod ->
      concurrently_
        (runEffect $ ns >-> cons)
        (runEffect $ (prod >>= log) >-> Pipes.mapM_ log)
