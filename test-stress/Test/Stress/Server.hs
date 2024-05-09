{-# LANGUAGE OverloadedStrings #-}
module Test.Stress.Server (server) where

import Control.Monad
import Data.ByteString.Lazy qualified as Lazy (ByteString)
-- import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy.Char8 qualified as BS.Char8

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Test.Stress.Server.API

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

server :: IO ()
server =
    runServerWithHandlers config def { serverCompression = Compr.none } [
        SomeRpcHandler (Proxy @ManyServerStreaming) $
          streamingRpcHandler $ mkServerStreaming serverServerStreaming
      ]
  where
    config :: ServerConfig
    config = ServerConfig {
          serverInsecure = Just $ InsecureConfig Nothing defaultInsecurePort
        , serverSecure   = Nothing
        }

-- | Server streaming handler
serverServerStreaming :: Lazy.ByteString -> (Lazy.ByteString -> IO ()) -> IO ()
serverServerStreaming inp send = do
    let n = read $ BS.Char8.unpack inp
    replicateM_ n $ send "00000000"
