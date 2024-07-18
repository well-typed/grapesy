{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}

module Demo.Server.Service.RouteGuide (
    Handler -- opaque
  , runHandler
  , handlers
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.State qualified as State
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Time

import Network.GRPC.Common
import Network.GRPC.Common.NextElem qualified as NextElem
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.StreamType

import Demo.Server.Aux.RouteGuide
import Demo.Server.Cmdline

import Proto.API.RouteGuide

{-------------------------------------------------------------------------------
  Custom handler monad

  This isn't really necessary, but demonstrates that we can.
-------------------------------------------------------------------------------}

newtype Handler a = Wrap {
      unwrap :: ReaderT [Proto Feature] IO a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadIO
    )

runHandler :: [Proto Feature] -> Handler a -> IO a
runHandler db = flip runReaderT db . unwrap

asksDb :: ([Proto Feature] -> a) -> Handler a
asksDb = Wrap . asks

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handlers :: Cmdline -> Methods Handler (ProtobufMethodsOf RouteGuide)
handlers cmdline
  | cmdTrailersOnlyShortcut cmdline
  =   Method    (mkNonStreaming    getFeature          )
    $ RawMethod (mkRpcHandler      trailersOnlyShortcut)
    $ Method    (mkClientStreaming recordRoute         )
    $ Method    (mkBiDiStreaming   routeChat           )
    $ NoMoreMethods

  -- demonstrate the use of 'simpleMethods'
  | otherwise
  = simpleMethods
      (mkNonStreaming    getFeature  )
      (mkServerStreaming listFeatures)
      (mkClientStreaming recordRoute )
      (mkBiDiStreaming   routeChat   )

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

getFeature :: Proto Point -> Handler (Proto Feature)
getFeature p = asksDb $ fromMaybe defMessage . featureAt p

listFeatures ::
     Proto Rectangle
  -> (NextElem (Proto Feature) -> IO ())
  -> Handler ()
listFeatures r send = do
    ps <- asksDb $ filter (\f -> inRectangle r (f ^. #location))
    liftIO $ NextElem.mapM_ send ps

recordRoute ::
     IO (NextElem (Proto  Point))
  -> Handler (Proto RouteSummary)
recordRoute recv = do
    mkSummary <- asksDb summary
    liftIO $ do
      start <- getCurrentTime
      ps    <- NextElem.collect recv
      stop  <- getCurrentTime
      return $ mkSummary (stop `diffUTCTime` start) ps

routeChat ::
     IO (NextElem (Proto RouteNote))
  -> (NextElem (Proto RouteNote) -> IO ())
  -> Handler ()
routeChat recv send = liftIO $ flip evalStateT Map.empty $ do
    NextElem.whileNext_ (liftIO recv) $ \n -> modifyM $ \acc -> do
      let notes = Map.findWithDefault [] (n ^. #location) acc
      mapM_ (send . NextElem) $ reverse notes
      return $ Map.alter (Just . (n:) . fromMaybe []) (n ^. #location) acc
    liftIO $ send NoNextElem

{-------------------------------------------------------------------------------
  Trailers-Only shortcut

  See discussion in @demo-server.md@.
-------------------------------------------------------------------------------}

trailersOnlyShortcut :: Call ListFeatures -> Handler ()
trailersOnlyShortcut call = do
    r <- liftIO $ recvFinalInput call
    features <- asksDb $ filter (\f -> inRectangle r (f ^. #location))
    liftIO $
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
