module Client (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Int
import Data.Text (Text)
import System.Random

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.CanCallRPC
import Network.GRPC.Common
import Network.GRPC.Common.NextElem qualified as NextElem
import Network.GRPC.Common.Protobuf

import RouteGuide

import Proto.API.RouteGuide

{-------------------------------------------------------------------------------
  Custom client monad

  The only requirements here as far as @grapesy@ is concerned is the monad stack
  satisfies 'MonadIO', 'MonadMask', and 'CanCallRPC'.
-------------------------------------------------------------------------------}

newtype Client a = WrapClient {
      unwrapClient :: ReaderT ClientEnv IO a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadMask
    )

data ClientEnv = ClientEnv {
      conn :: Connection
    }

runClient :: Connection -> Client a -> IO a
runClient conn = flip runReaderT ClientEnv{conn} . unwrapClient

instance CanCallRPC Client where
  getConnection = WrapClient $ conn <$> ask

{-------------------------------------------------------------------------------
  Call each of the methods of the RouteGuide service
-------------------------------------------------------------------------------}

getFeature :: Client ()
getFeature = do
    let req = defMessage
                & #latitude  .~  409146138
                & #longitude .~ -746188906
    resp <- nonStreaming (rpc @(Protobuf RouteGuide "getFeature")) req
    liftIO $ print resp

listFeatures :: Client ()
listFeatures = do
    let lo  = defMessage
                & #latitude  .~  400000000
                & #longitude .~ -750000000
        hi  = defMessage
                & #latitude  .~  420000000
                & #longitude .~ -730000000
        req = defMessage
                & #lo .~ lo
                & #hi .~ hi
    serverStreaming (rpc @(Protobuf RouteGuide "listFeatures")) req $ \recv -> liftIO $
      NextElem.whileNext_ recv print

recordRoute :: Client ()
recordRoute = do
    db   <- liftIO $ getDB
    resp <- clientStreaming_ (rpc @(Protobuf RouteGuide "recordRoute")) $ \send -> liftIO $ do
      replicateM_ 10 $ do
        i <- randomRIO (0, length db - 1)
        let p = (db !! i) ^. #location
        threadDelay 500_000 -- 0.5 seconds
        send $ NextElem p
      send NoNextElem
    liftIO $ print resp

routeChat :: Client ()
routeChat = do
    biDiStreaming (rpc @(Protobuf RouteGuide "routeChat")) $ \send recv -> liftIO $ do
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

client :: Client ()
client = do
    liftIO $ putStrLn "-------------- GetFeature --------------"
    getFeature

    liftIO $ putStrLn "-------------- ListFeatures --------------"
    listFeatures

    liftIO $ putStrLn "-------------- RecordRoute --------------"
    recordRoute

    liftIO $ putStrLn "-------------- RouteChat --------------"
    routeChat

main :: IO ()
main =
    withConnection def server $ \conn ->
      runClient conn client
  where
    server :: Server
    server = ServerInsecure $ Address "127.0.0.1" defaultInsecurePort Nothing
