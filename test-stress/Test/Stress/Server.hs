module Test.Stress.Server (server) where

import Network.GRPC.Common
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType
import Network.GRPC.Server.StreamType.Binary qualified as Binary

import Test.Stress.Cmdline
import Test.Stress.Server.API

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

server :: Cmdline -> IO ()
server _cmdline =
    runServerWithHandlers config def [
        fromMethod $
          Binary.mkNonStreaming @ManyShortLived @Word $
            return . succ
      ]
  where
    config :: ServerConfig
    config = ServerConfig {
          serverInsecure = Just $ InsecureConfig Nothing defaultInsecurePort
        , serverSecure   = Nothing
        }
