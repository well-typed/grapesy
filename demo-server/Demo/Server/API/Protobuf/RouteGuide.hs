{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}

module Demo.Server.API.Protobuf.RouteGuide (handlers) where

import Control.Lens (view, (^.))
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State qualified as State
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.ProtoLens
import Data.ProtoLens.Labels ()
import Data.Time

import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server.Protobuf

import Proto.RouteGuide

import Demo.Server.Aux.RouteGuide

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handlers :: [Feature] -> MethodsOf IO RouteGuide
handlers db =
     Method (getFeature   db)
   $ Method (listFeatures db)
   $ Method (recordRoute  db)
   $ Method (routeChat    db)
   $ NoMoreMethods

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

getFeature :: [Feature] -> NonStreamingHandler IO RouteGuide "getFeature"
getFeature db = nonStreaming $ \p ->
    return $ fromMaybe defMessage $ featureAt db p

listFeatures :: [Feature] -> ServerStreamingHandler IO RouteGuide "listFeatures"
listFeatures db = serverStreaming $ \r send -> do
    mapM_ send $ filter (inRectangle r . view #location) db

recordRoute :: [Feature] -> ClientStreamingHandler IO RouteGuide "recordRoute"
recordRoute db = clientStreaming $ \recv -> do
    start <- getCurrentTime
    ps    <- StreamElem.collect recv
    stop  <- getCurrentTime
    return $ summary db (stop `diffUTCTime` start) ps

routeChat :: [Feature] -> BiDiStreamingHandler IO RouteGuide "routeChat"
routeChat _db = biDiStreaming $ \recv send -> flip State.evalStateT Map.empty $
    StreamElem.mapM_ (lift recv) $ \n -> modifyM $ \acc -> do
      mapM_ send $ reverse $ Map.findWithDefault [] (n ^. #location) acc
      return $ Map.alter (Just . (n:) . fromMaybe []) (n ^. #location) acc

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

modifyM :: Monad m => (s -> m s) -> StateT s m ()
#if MIN_VERSION_transformers(0,6,1)
modifyM = State.modifyM
#else
modifyM f = State.StateT $ \s -> ((),) <$> f s
#endif
