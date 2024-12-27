module Server (main) where

import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Time

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType
import Network.GRPC.Common.NextElem qualified as NextElem

import Proto.API.RouteGuide

import RouteGuide

{-------------------------------------------------------------------------------
  Custom monad for handlers

  The only requirement here as far as @grapesy@ is concerned is that we can
  ultimately hoist these into @IO@ ('hoistMethods').
-------------------------------------------------------------------------------}

newtype Handler a = WrapHandler {
      unwrapHandler :: ReaderT DB IO a
    }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader DB)

runHandler :: DB -> Handler a -> IO a
runHandler db = flip runReaderT db . unwrapHandler

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

getFeature :: Proto Point -> Handler (Proto Feature)
getFeature p = do
    db <- ask
    return $ fromMaybe (defMessage & #location .~ p) (featureAt db p)

listFeatures ::
     Proto Rectangle
  -> (NextElem (Proto Feature) -> IO ())
  -> Handler ()
listFeatures r send = do
    db <- ask
    liftIO $ NextElem.forM_ (featuresIn db r) send

recordRoute ::
     IO (NextElem (Proto Point))
  -> Handler (Proto RouteSummary)
recordRoute recv = do
    db <- ask
    liftIO $ do
      start <- getCurrentTime
      ps    <- NextElem.collect recv
      stop  <- getCurrentTime
      return $ summary db (stop `diffUTCTime` start) ps

routeChat ::
     IO (NextElem (Proto RouteNote))
  -> (NextElem (Proto RouteNote) -> IO ())
  -> Handler ()
routeChat recv send = liftIO $ do
    st :: IORef Chat <- newIORef emptyChat
    NextElem.whileNext_ recv $ \note -> do
      prev <- atomicModifyIORef st $ \chat -> (
          recordNote note chat
        , getNotes chat (note ^. #location)
        )
      -- Can't use NextElem.forM_ here: we don't want to send 'NoNextElem' yet
      forM_ prev $ send . NextElem
    send NoNextElem

{-------------------------------------------------------------------------------
  Server top-level
-------------------------------------------------------------------------------}

methods :: Methods Handler (ProtobufMethodsOf RouteGuide)
methods =
      Method (mkNonStreaming    getFeature  )
    $ Method (mkServerStreaming listFeatures)
    $ Method (mkClientStreaming recordRoute )
    $ Method (mkBiDiStreaming   routeChat   )
    $ NoMoreMethods

main :: IO ()
main = do
    db <- getDB
    runServerWithHandlers def config $ fromMethods $
      hoistMethods (runHandler db) methods
  where
    config :: ServerConfig
    config = ServerConfig {
          serverInsecure = Just (InsecureConfig (Just "0.0.0.0") defaultInsecurePort)
        , serverSecure   = Nothing
        }
