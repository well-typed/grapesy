module Client (main) where

import Control.Concurrent
import Control.Monad
import Data.Int
import Data.Text (Text)
import System.Random

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.StreamElem qualified as StreamElem

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
    withRPC conn def (Proxy @(Protobuf RouteGuide "getFeature")) $ \call -> do
      sendFinalInput call req
      resp <- recvFinalOutput call
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
    withRPC conn def (Proxy @(Protobuf RouteGuide "listFeatures")) $ \call -> do
      sendFinalInput call req
      void $ StreamElem.whileNext_ (recvOutput call) print

recordRoute :: Connection -> IO ()
recordRoute conn = do
    db <- getDB
    withRPC conn def (Proxy @(Protobuf RouteGuide "recordRoute")) $ \call -> do
      replicateM_ 10 $ do
        i <- randomRIO (0, length db - 1)
        let p = (db !! i) ^. #location
        sendNextInput call p
        threadDelay 500_000 -- 0.5 seconds
      sendEndOfInput call
      (resp, NoMetadata) <- recvFinalOutput call
      print resp

routeChat :: Connection -> IO ()
routeChat conn = do
    withRPC conn def (Proxy @(Protobuf RouteGuide "routeChat")) $ \call -> do
      StreamElem.forM_ messages NoMetadata $ sendInput call
      void $ StreamElem.whileNext_ (recvOutput call) print
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
