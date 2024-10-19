module Server (main) where

import Control.Monad
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
  Individual handlers
-------------------------------------------------------------------------------}

getFeature :: DB -> Proto Point -> IO (Proto Feature)
getFeature db p =
    return $ fromMaybe (defMessage & #location .~ p) (featureAt db p)

listFeatures ::
     DB
  -> Proto Rectangle
  -> (NextElem (Proto Feature) -> IO ())
  -> IO ()
listFeatures db r send =
    NextElem.forM_ (featuresIn db r) send

recordRoute ::
     DB
  -> IO (NextElem (Proto Point))
  -> IO (Proto RouteSummary)
recordRoute db recv = do
    start <- getCurrentTime
    ps    <- NextElem.collect recv
    stop  <- getCurrentTime
    return $ summary db (stop `diffUTCTime` start) ps

routeChat ::
     IO (NextElem (Proto RouteNote))
  -> (NextElem (Proto RouteNote) -> IO ())
  -> IO ()
routeChat recv send = do
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

methods :: DB -> Methods IO (ProtobufMethodsOf RouteGuide)
methods db =
      Method (mkNonStreaming    $ getFeature   db)
    $ Method (mkServerStreaming $ listFeatures db)
    $ Method (mkClientStreaming $ recordRoute  db)
    $ Method (mkBiDiStreaming   $ routeChat      )
    $ NoMoreMethods

main :: IO ()
main = do
    db <- getDB
    runServerWithHandlers def config $ fromMethods (methods db)
  where
    config :: ServerConfig
    config = ServerConfig {
          serverInsecure = Just (InsecureConfig Nothing defaultInsecurePort)
        , serverSecure   = Nothing
        }
