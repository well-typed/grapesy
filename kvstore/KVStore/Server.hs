module KVStore.Server (withKeyValueServer) where

import Control.Concurrent (threadDelay)
import Control.Monad

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server
import Network.GRPC.Server.Run

import KVStore.API
import KVStore.API.JSON qualified as JSON
import KVStore.API.Protobuf qualified as Protobuf
import KVStore.Cmdline
import KVStore.Util.Store (Store)
import KVStore.Util.Store qualified as Store

import Paths_grapesy

{-------------------------------------------------------------------------------
  Server proper
-------------------------------------------------------------------------------}

withKeyValueServer :: Cmdline -> (RunningServer -> IO ()) -> IO ()
withKeyValueServer cmdline@Cmdline{
                       cmdJSON
                     , cmdSecure
                     , cmdDisableTcpNoDelay
                     , cmdPingRateLimit
                     } k = do
    store <- Store.new

    config :: ServerConfig <-
      if cmdSecure then do
        pub  <- getDataFileName "grpc-demo.pem"
        priv <- getDataFileName "grpc-demo.key"
        return ServerConfig {
            serverInsecure = Nothing
          , serverSecure   = Just $ SecureConfig {
                secureHost       = "0.0.0.0"
              , securePort       = defaultSecurePort
              , securePubCert    = pub
              , secureChainCerts = []
              , securePrivKey    = priv
              , secureSslKeyLog  = SslKeyLogFromEnv
              }
          }
      else
        return ServerConfig {
            serverInsecure = Just $ InsecureConfig Nothing defaultInsecurePort
          , serverSecure   = Nothing
          }

    let rpcHandlers :: [SomeRpcHandler IO]
        rpcHandlers
          | cmdJSON   = JSON.server     $ handlers cmdline store
          | otherwise = Protobuf.server $ handlers cmdline store

    server <- mkGrpcServer params rpcHandlers
    forkServer params config server k
  where
    params :: ServerParams
    params = def {
          serverHTTP2Settings = def {
              http2TcpNoDelay            = not cmdDisableTcpNoDelay
            , http2OverridePingRateLimit = cmdPingRateLimit
            }

          -- The Java benchmark does not use compression (unclear if the Java
          -- implementation supports compression at all; the compression Interop
          -- tests are also disabled for Java). For a fair comparison, we
          -- therefore disable compression here also.
        , serverCompression = Compr.none
        }

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

handlers :: Cmdline -> Store Key Value -> KVStore
handlers cmdline store = KVStore {
      create = \(key, value) -> do
        simulateWork cmdline writeDelayMillis
        inserted <- Store.putIfAbsent store key value
        unless inserted $ throwGrpcError GrpcAlreadyExists
    , update = \(key, value) -> do
        simulateWork cmdline writeDelayMillis
        replaced <- Store.replace store key value
        unless replaced $ throwGrpcError GrpcNotFound
    , retrieve = \key -> do
        simulateWork cmdline readDelayMillis
        mValue <- Store.get store key
        case mValue of
          Just value -> return value
          Nothing    -> throwGrpcError GrpcNotFound
    , delete = \key -> do
        simulateWork cmdline writeDelayMillis
        Store.remove store key
    }

{-------------------------------------------------------------------------------
  Delays
-------------------------------------------------------------------------------}

simulateWork :: Cmdline -> Int -> IO ()
simulateWork Cmdline{cmdSimulateWork} n
  | cmdSimulateWork = threadDelay (n * 1_000)
  | otherwise       = return ()

readDelayMillis, writeDelayMillis :: Int
readDelayMillis  = 10
writeDelayMillis = 50
