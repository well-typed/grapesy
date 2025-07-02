module Client (main) where

import Conduit
import Control.Concurrent
import Data.Conduit.List
import Data.Int
import Data.Text (Text)
import System.Random

import Data.ProtoLens.Labels ()

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.Conduit
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import RouteGuide

import Proto.API.RouteGuide

{-------------------------------------------------------------------------------
  Call each of the methods of the RouteGuide service
-------------------------------------------------------------------------------}

listFeatures :: Connection -> IO ()
listFeatures conn = do
    let lo  = defMessage
                & #latitude  .~  400000000
                & #longitude .~ -750000000
        hi  = defMessage
                & #latitude  .~  420000000
                & #longitude .~ -730000000
        req = defMessage
                & #lo .~ lo
                & #hi .~ hi

    let sink :: ConduitT (Proto Feature) Void IO ()
        sink = mapM_C print

    serverStreaming conn (rpc @(Protobuf RouteGuide "listFeatures")) req $ \source ->
      runConduit $ source .| sink

recordRoute :: Connection -> IO ()
recordRoute conn = do
    db <- getDB

    let source :: ConduitT () (Proto Point) IO ()
        source = replicateMC 10 $ do
            i <- randomRIO (0, length db - 1)
            let p = (db !! i) ^. #location
            threadDelay 500_000 -- 0.5 seconds
            return p

    resp <- clientStreaming_ conn (rpc @(Protobuf RouteGuide "recordRoute")) $ \sink ->
              runConduit $ source .| sink
    print resp

routeChat :: Connection -> IO ()
routeChat conn = do

    let clientSource :: ConduitT () (Proto RouteNote) IO ()
        clientSource = sourceList messages

        clientSink :: ConduitT (Proto RouteNote) Void IO ()
        clientSink = mapM_C print

    biDiStreaming conn (rpc @(Protobuf RouteGuide "routeChat")) $ \serverSink serverSource -> do
      runConduit $ clientSource .| serverSink
      runConduit $ serverSource .| clientSink
  where
    messages :: [Proto RouteNote]
    messages = [
          makeRouteNote "First message"  0 0
        , makeRouteNote "Second message" 0 1
        , makeRouteNote "Third message"  1 0
        , makeRouteNote "Fourth message" 0 0
        , makeRouteNote "Fifth message"  1 0
        ]

    makeRouteNote :: Text -> Int32 -> Int32 -> Proto RouteNote
    makeRouteNote message latitude longitude =
        let location =
              defMessage
                & #latitude  .~ latitude
                & #longitude .~ longitude
        in defMessage
             & #message  .~ message
             & #location .~ location

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main =
    withConnection def server $ \conn -> do
      putStrLn "-------------- ListFeatures --------------"
      listFeatures conn

      putStrLn "-------------- RecordRoute --------------"
      recordRoute conn

      putStrLn "-------------- RouteChat --------------"
      routeChat conn
  where
    server :: Server
    server = ServerInsecure $ Address "127.0.0.1" defaultInsecurePort Nothing
