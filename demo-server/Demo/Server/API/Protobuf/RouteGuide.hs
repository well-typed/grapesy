{-# LANGUAGE OverloadedLabels #-}

module Demo.Server.API.Protobuf.RouteGuide (handlers) where

import Control.Lens (view, (^.))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.ProtoLens
import Data.ProtoLens.Labels ()
import Data.Time

import Network.GRPC.Server
import Network.GRPC.Server.Protobuf

import Proto.RouteGuide

import Demo.Server.Aux.RouteGuide

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handlers :: [Feature] -> MethodsOf RouteGuide
handlers db = Methods $
     Method (getFeature   db)
   $ Method (listFeatures db)
   $ Method (recordRoute  db)
   $ Method (routeChat    db)
   $ NoMoreMethods

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

getFeature ::
     [Feature]
  -> Point
  -> IO (Feature, [CustomMetadata])
getFeature db p =
    return (fromMaybe defMessage $ featureAt db p, [])

listFeatures ::
     [Feature]
  -> Rectangle
  -> (Feature -> IO ())
  -> IO [CustomMetadata]
listFeatures db r send = do
    mapM_ send $ filter (inRectangle r . view #location) db
    return []

recordRoute ::
     [Feature]
  -> IO (StreamElem () Point)
  -> IO (RouteSummary, [CustomMetadata])
recordRoute db recv = do
    start <- getCurrentTime
    go start []
  where
    go :: UTCTime -> [Point] -> IO (RouteSummary, [CustomMetadata])
    go start acc = do
        mp <- recv
        case mp of
          StreamElem p    -> go       start (p:acc)
          FinalElem  p () -> finalise start (p:acc)
          NoMoreElems  () -> finalise start    acc

    finalise :: UTCTime -> [Point] -> IO (RouteSummary, [CustomMetadata])
    finalise start acc = do
        stop <- getCurrentTime
        return (summary db (stop `diffUTCTime` start) (reverse acc), [])

routeChat ::
     [Feature]
  -> IO (StreamElem () RouteNote)
  -> (RouteNote -> IO ())
  -> IO [CustomMetadata]
routeChat _db recv send = go Map.empty
  where
    go :: Map Point [RouteNote] -> IO [CustomMetadata]
    go acc = do
        mNote <- recv
        case mNote of
          StreamElem n    -> goNote acc n >>= go
          FinalElem  n () -> goNote acc n >>  return []
          NoMoreElems  () ->                  return []

    goNote :: Map Point [RouteNote] -> RouteNote -> IO (Map Point [RouteNote])
    goNote acc n = do
        mapM_ send $ Map.findWithDefault [] (n ^. #location) acc
        return $ Map.alter (Just . (n:) . fromMaybe []) (n ^. #location) acc





