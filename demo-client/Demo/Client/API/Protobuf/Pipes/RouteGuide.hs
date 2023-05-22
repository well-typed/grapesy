module Demo.Client.API.Protobuf.Pipes.RouteGuide (
    listFeatures
  , recordRoute
  , routeChat
  ) where

import Prelude hiding (log)

import Control.Concurrent.Async
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

listFeatures :: Connection -> CallParams -> Rectangle -> IO ()
listFeatures conn params r = runSafeT . runEffect $
    let prod = serverStreaming conn params (RPC @RouteGuide @"listFeatures") r
    in (prod >>= log) >-> Pipes.mapM_ log

recordRoute ::
     Connection
  -> CallParams
  -> Producer' (StreamElem () Point) (SafeT IO) ()
  -> IO ()
recordRoute conn params ps = runSafeT . runEffect $
    let cons = clientStreaming conn params (RPC @RouteGuide @"recordRoute")
    in ps >-> (cons >>= log)

routeChat ::
     Connection
  -> CallParams
  -> Producer' (StreamElem () RouteNote) IO ()
  -> IO ()
routeChat conn params ns =
    biDiStreaming conn params (RPC @RouteGuide @"routeChat") $ \cons prod ->
      concurrently_
        (runEffect $ ns >-> cons)
        (runEffect $ (prod >>= log) >-> Pipes.mapM_ log)
