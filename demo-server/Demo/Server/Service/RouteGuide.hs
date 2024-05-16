{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}

module Demo.Server.Service.RouteGuide (handlers) where

import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State qualified as State
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Time

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.StreamType

import Demo.Common.API
import Demo.Server.Aux.RouteGuide
import Demo.Server.Cmdline

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handlers ::
     Cmdline
  -> [Proto Feature]
  -> Methods IO (ProtobufMethodsOf RouteGuide)
handlers cmdline db
  | cmdTrailersOnlyShortcut cmdline
  =   Method    (mkNonStreaming    $ getFeature           db)
    $ RawMethod (mkRpcHandler      $ trailersOnlyShortcut db)
    $ Method    (mkClientStreaming $ recordRoute          db)
    $ Method    (mkBiDiStreaming   $ routeChat            db)
    $ NoMoreMethods

  -- demonstrate the use of 'simpleMethods'
  | otherwise
  = simpleMethods
      (mkNonStreaming    $ getFeature   db)
      (mkServerStreaming $ listFeatures db)
      (mkClientStreaming $ recordRoute  db)
      (mkBiDiStreaming   $ routeChat    db)

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

getFeature :: [Proto Feature] -> Proto Point -> IO (Proto Feature)
getFeature db p =
    return $ fromMaybe defMessage $ featureAt db p

listFeatures :: [Proto Feature] -> Proto Rectangle -> (Proto Feature -> IO ()) -> IO ()
listFeatures db r send =
    mapM_ send $
      filter (\f -> inRectangle r (f ^. #location)) db

recordRoute ::
     [Proto Feature]
  -> IO (StreamElem NoMetadata (Proto Point))
  -> IO (Proto RouteSummary)
recordRoute db recv = do
    start <- getCurrentTime
    ps    <- StreamElem.collect recv
    stop  <- getCurrentTime
    return $ summary db (stop `diffUTCTime` start) ps

routeChat ::
     [Proto Feature]
  -> IO (StreamElem NoMetadata (Proto RouteNote))
  -> (Proto RouteNote -> IO ())
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

trailersOnlyShortcut :: [Proto Feature] -> Call ListFeatures -> IO ()
trailersOnlyShortcut db call = do
    r <- recvFinalInput call
    let features = filter (\f -> inRectangle r (f ^. #location)) db
    if null features then
      sendTrailersOnly call def
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
