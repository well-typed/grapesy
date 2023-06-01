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
    go recv start
  where
    go :: IO (StreamElem () Point) -> UTCTime -> IO RouteSummary
    go recv start = loop []
      where
        loop :: [Point] -> IO RouteSummary
        loop acc = do
            mp <- recv
            case mp of
              StreamElem p    -> loop     (p:acc)
              FinalElem  p () -> finalise (p:acc)
              NoMoreElems  () -> finalise    acc

        finalise :: [Point] -> IO RouteSummary
        finalise acc = do
            stop <- getCurrentTime
            return $ summary db (stop `diffUTCTime` start) (reverse acc)

routeChat :: [Feature] -> BiDiStreamingHandler IO RouteGuide "routeChat"
routeChat _db = biDiStreaming go
  where
    go :: IO (StreamElem () RouteNote) -> (RouteNote -> IO ()) -> IO ()
    go recv send = loop Map.empty
      where
        loop :: Map Point [RouteNote] -> IO ()
        loop acc = do
            mNote <- recv
            case mNote of
              StreamElem n    -> goNote acc n >>= loop
              FinalElem  n () -> goNote acc n >>  return ()
              NoMoreElems  () ->                  return ()

        goNote ::
             Map Point [RouteNote]
          -> RouteNote
          -> IO (Map Point [RouteNote])
        goNote acc n = do
            mapM_ send $ Map.findWithDefault [] (n ^. #location) acc
            return $ Map.alter (Just . (n:) . fromMaybe []) (n ^. #location) acc
