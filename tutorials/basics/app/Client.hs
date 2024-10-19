module Client (main) where

import Control.Concurrent
import Control.Monad
import Data.Int
import Data.Text (Text)
import System.Random

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common
import Network.GRPC.Common.NextElem qualified as NextElem
import Network.GRPC.Common.Protobuf

import RouteGuide

import Proto.API.RouteGuide

{-------------------------------------------------------------------------------
  Call each of the methods of the RouteGuide service
-------------------------------------------------------------------------------}

getFeature :: Connection -> IO ()
getFeature conn = do
    let req = defMessage
                & #latitude  .~  409146138
                & #longitude .~ -746188906
    resp <- nonStreaming conn (rpc @(Protobuf RouteGuide "getFeature")) req
    print resp

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
    serverStreaming conn (rpc @(Protobuf RouteGuide "listFeatures")) req $ \recv ->
      NextElem.whileNext_ recv print

recordRoute :: Connection -> IO ()
recordRoute conn = do
    db   <- getDB
    resp <- clientStreaming_ conn (rpc @(Protobuf RouteGuide "recordRoute")) $ \send -> do
      replicateM_ 10 $ do
        i <- randomRIO (0, length db - 1)
        let p = (db !! i) ^. #location
        send $ NextElem p
        threadDelay 500_000 -- 0.5 seconds
      send NoNextElem
    print resp

routeChat :: Connection -> IO ()
routeChat conn = do
    biDiStreaming conn (rpc @(Protobuf RouteGuide "routeChat")) $ \send recv -> do
      NextElem.forM_ messages send
      NextElem.whileNext_ recv print
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
      putStrLn "-------------- GetFeature --------------"
      getFeature conn

      putStrLn "-------------- ListFeatures --------------"
      listFeatures conn

      putStrLn "-------------- RecordRoute --------------"
      recordRoute conn

      putStrLn "-------------- RouteChat --------------"
      routeChat conn
  where
    server :: Server
    server = ServerInsecure $ Address "127.0.0.1" defaultInsecurePort Nothing
