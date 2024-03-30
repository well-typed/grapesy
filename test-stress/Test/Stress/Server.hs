module Test.Stress.Server (server) where

import Network.GRPC.Common
import Network.GRPC.Server
import Network.GRPC.Server.Binary qualified as Binary
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Test.Stress.Cmdline
import Test.Stress.Server.API

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

server :: Cmdline -> IO ()
server _cmdline =
    runServerWithHandlers config def [
        SomeRpcHandler (Proxy @ManyShortLived) $
          streamingRpcHandler $ Binary.mkNonStreaming serverManyShortLived
      ]
  where
    config :: ServerConfig
    config = ServerConfig {
          serverInsecure = Just $ InsecureConfig Nothing defaultInsecurePort
        , serverSecure   = Nothing
        }

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

serverManyShortLived :: Word -> IO Word
serverManyShortLived = return . succ
