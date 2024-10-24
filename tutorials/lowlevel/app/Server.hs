module Server (main) where

import Control.Monad
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Time

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType (Methods(..), fromMethods)

import Proto.API.RouteGuide

import RouteGuide

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

getFeature :: DB -> Call (Protobuf RouteGuide "getFeature") -> IO ()
getFeature db call = do
    p <- recvFinalInput call
    sendFinalOutput call (
        fromMaybe (defMessage & #location .~ p) (featureAt db p)
      , NoMetadata
      )

listFeatures :: DB -> Call (Protobuf RouteGuide "listFeatures") -> IO ()
listFeatures db call = do
    r <- recvFinalInput call
    StreamElem.forM_ (featuresIn db r) NoMetadata (sendOutput call)

recordRoute :: DB -> Call (Protobuf RouteGuide "recordRoute") -> IO ()
recordRoute db call = do
    start            <- getCurrentTime
    (ps, NoMetadata) <- StreamElem.collect (recvInput call)
    stop             <- getCurrentTime
    sendFinalOutput call (
        summary db (stop `diffUTCTime` start) ps
      , NoMetadata
      )

routeChat :: Call (Protobuf RouteGuide "routeChat") -> IO ()
routeChat call = do
    st :: IORef Chat <- newIORef emptyChat
    NoMetadata <- StreamElem.whileNext_ (recvInput call) $ \note -> do
      prev <- atomicModifyIORef st $ \chat -> (
          recordNote note chat
        , getNotes chat (note ^. #location)
        )
      -- Can't use StreamElem.forM_ here: we don't want to send 'FinalElem' yet
      forM_ prev $ sendNextOutput call
    sendTrailers call NoMetadata

{-------------------------------------------------------------------------------
  Server top-level
-------------------------------------------------------------------------------}

methods :: DB -> Methods IO (ProtobufMethodsOf RouteGuide)
methods db =
      RawMethod (mkRpcHandler $ getFeature   db)
    $ RawMethod (mkRpcHandler $ listFeatures db)
    $ RawMethod (mkRpcHandler $ recordRoute  db)
    $ RawMethod (mkRpcHandler $ routeChat      )
    $ NoMoreMethods


-- Alternative way to define the handlers, avoiding 'fromMethods'
_handlers :: DB -> [SomeRpcHandler IO]
_handlers db = [
      someRpcHandler . mkRpcHandler $ getFeature   db
    , someRpcHandler . mkRpcHandler $ listFeatures db
    , someRpcHandler . mkRpcHandler $ recordRoute  db
    , someRpcHandler . mkRpcHandler $ routeChat
    ]

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
