{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}

module Demo.Server.Service.RouteGuide (handlers) where

import Control.Lens (view, (^.))
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State qualified as State
import Data.Default
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.ProtoLens
import Data.ProtoLens.Labels ()
import Data.Proxy
import Data.Time

import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Common.StreamType
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.StreamType

import Proto.RouteGuide

import Demo.Server.Aux.RouteGuide
import Demo.Server.Cmdline

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handlers :: Cmdline -> [Feature] -> Methods IO (ProtobufMethodsOf RouteGuide)
handlers cmdline db =
      Method (mkNonStreaming    $ getFeature   db)
    $ ( if cmdTrailersOnlyShortcut cmdline
          then RawMethod $ mkRpcHandler
                             (Proxy @(Protobuf RouteGuide "listFeatures"))
                             (trailersOnlyShortcut db)
          else Method (mkServerStreaming $ listFeatures db)
      )
    $ Method (mkClientStreaming $ recordRoute  db)
    $ Method (mkBiDiStreaming   $ routeChat    db)
    $ NoMoreMethods

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

getFeature :: [Feature] -> Point -> IO Feature
getFeature db p = return $ fromMaybe defMessage $ featureAt db p

listFeatures :: [Feature] -> Rectangle -> (Feature -> IO ()) -> IO ()
listFeatures db r send = mapM_ send $ filter (inRectangle r . view #location) db

recordRoute :: [Feature] -> IO (StreamElem NoMetadata Point) -> IO RouteSummary
recordRoute db recv = do
    start <- getCurrentTime
    ps    <- StreamElem.collect recv
    stop  <- getCurrentTime
    return $ summary db (stop `diffUTCTime` start) ps

routeChat ::
     [Feature]
  -> IO (StreamElem NoMetadata RouteNote)
  -> (RouteNote -> IO ())
  -> IO ()
routeChat _db recv send = do
    flip State.evalStateT Map.empty $
      StreamElem.mapM_ (lift recv) $ \n -> modifyM $ \acc -> do
        mapM_ send $ reverse $ Map.findWithDefault [] (n ^. #location) acc
        return $ Map.alter (Just . (n:) . fromMaybe []) (n ^. #location) acc

{-------------------------------------------------------------------------------
  Trailers-Only shortcut

  See discussion in @demo-server.md@.
-------------------------------------------------------------------------------}

trailersOnlyShortcut ::
     [Feature]
  -> Call (Protobuf RouteGuide "listFeatures")
  -> IO ()
trailersOnlyShortcut db call = do
    r <- recvFinalInput call
    let features = filter (inRectangle r . view #location) db
    if null features then
      sendTrailersOnly call []
    else do
      mapM_ (sendOutput call . StreamElem) features
      sendTrailers call def

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

modifyM :: Monad m => (s -> m s) -> StateT s m ()
#if MIN_VERSION_transformers(0,6,1)
modifyM = State.modifyM
#else
modifyM f = State.StateT $ \s -> ((),) <$> f s
#endif
