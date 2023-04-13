module Demo.GRPC.Protobuf.Pipes.RouteGuide (
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
import Network.GRPC.Protobuf.Pipes
import Network.GRPC.Spec

import Proto.RouteGuide

import Demo.Driver.Logging

{-------------------------------------------------------------------------------
  routeguide.RouteGuide

  We do not include the 'getFeature' method, as it does not do any streaming.
-------------------------------------------------------------------------------}

listFeatures ::
     Connection
  -> RequestMeta
  -> Rectangle
  -> IO ()
listFeatures conn meta r = runSafeT . runEffect $
    let prod = serverStreaming conn meta (RPC @RouteGuide @"listFeatures") r
    in (prod >>= log) >-> Pipes.mapM_ log

recordRoute ::
     Connection
  -> RequestMeta
  -> Producer' (IsFinal, Point) (SafeT IO) ()
  -> IO ()
recordRoute conn meta ps = runSafeT . runEffect $
    let cons = clientStreaming conn meta (RPC @RouteGuide @"recordRoute")
    in ps >-> (cons >>= log)

routeChat ::
     Connection
  -> RequestMeta
  -> Producer' (IsFinal, RouteNote) IO ()
  -> IO ()
routeChat conn meta ns =
    biDiStreaming conn meta (RPC @RouteGuide @"routeChat") $ \cons prod ->
      concurrently_
        (runEffect $ ns >-> cons)
        (runEffect $ (prod >>= log) >-> Pipes.mapM_ log)
