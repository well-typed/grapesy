{-# LANGUAGE OverloadedStrings #-}
module Test.Stress.Server (server) where


import Control.Concurrent
import Control.Monad (forM_)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server
import Network.GRPC.Server.Binary qualified as Binary
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Test.Stress.Common
import Test.Stress.Server.API

import GHC.Conc

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

server :: Maybe Word -> IO ()
server disconnectInterval =
    case disconnectInterval of
      Just w -> do
        tid <- forkIO serve
        threadDelay $ fromIntegral w
        killThread tid
        let
          waitForDeath = do
            stat <- threadStatus tid
            case stat of
              ThreadFinished -> do
                putStrLn $ "dead: " ++ show tid
                return ()
              status -> do
                putStrLn $ "not yet dead: " ++ show tid ++ "; " ++ show status
                waitForDeath
        waitForDeath
        threadDelay 1000000
        server disconnectInterval
      Nothing ->
        serve
  where
    config :: ServerConfig
    config = ServerConfig {
          serverInsecure = Just $ InsecureConfig Nothing defaultInsecurePort
        , serverSecure   = Nothing
        }

    serve :: IO ()
    serve =
        runServerWithHandlers config def { serverCompression = Compr.none } [
            SomeRpcHandler (Proxy @ManyNonStreaming) $
              streamingRpcHandler $ Binary.mkNonStreaming serverNonStreaming
          , SomeRpcHandler (Proxy @ManyClientStreaming) $
              streamingRpcHandler $ Binary.mkClientStreaming serverClientStreaming
          , SomeRpcHandler (Proxy @ManyServerStreaming) $
              streamingRpcHandler $ Binary.mkServerStreaming serverServerStreaming
          , SomeRpcHandler (Proxy @ManyBiDiStreaming) $
              streamingRpcHandler $ Binary.mkBiDiStreaming serverBiDiStreaming
          ]

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

-- | Simple non-streaming handler
serverNonStreaming :: Word -> IO Word
serverNonStreaming = return . succ

-- | Client streaming handler
serverClientStreaming :: IO (StreamElem NoMetadata String) -> IO Word
serverClientStreaming recv = do
    loop 0
  where
    loop :: Word -> IO Word
    loop !n =
      StreamElem.value <$> recv >>=
        \case
          Just _ -> loop (n + 1)
          Nothing -> return n

-- | Server streaming handler
serverServerStreaming :: Word -> (Lazy.ByteString -> IO ()) -> IO ()
serverServerStreaming n send = do
    forM_ [0 .. n] $ \m ->
      send . BS.Lazy.pack . (:[]) . fromIntegral $
        if odd m then
          m
        else
          1

-- | Bidirectional streaming handler
serverBiDiStreaming
  :: IO (StreamElem NoMetadata Word)
  -> (Word -> IO ())
  -> IO ()
serverBiDiStreaming recv send =
    loop
  where
    loop :: IO ()
    loop = StreamElem.value <$> recv >>= go

    go :: Maybe Word -> IO ()
    go (Just n) = send (collatz n) >> loop
    go Nothing  = return ()
