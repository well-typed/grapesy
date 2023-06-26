module Test.Util.ClientServer (
    ClientServerConfig(..)
  , testClientServer
    -- * Lower-level functionality
  , testClient
  , testServer
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Tracer
import Data.Default

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Run qualified as Server

import Paths_grapesy

data ClientServerConfig = ClientServerConfig {
      clientCompr :: Compression.Negotation
    , serverCompr :: Compression.Negotation
    , useTLS      :: Bool
    }

instance Default ClientServerConfig where
  def = ClientServerConfig {
      clientCompr = Compression.none
    , serverCompr = Compression.none
    , useTLS      = False
    }

testServer ::
     ClientServerConfig
  -> [Server.RpcHandler IO]
  -> IO ()
testServer cfg serverHandlers = do
    pubCert <- getDataFileName "grpc-demo.cert"
    privKey <- getDataFileName "grpc-demo.priv"

    let serverConfig :: Server.ServerConfig
        serverConfig
          | useTLS cfg
          = Server.ServerConfig {
                serverTracer   = nullTracer
              , serverInsecure = Nothing
              , serverSecure   = Just $ Server.SecureConfig {
                        secureHost       = Nothing
                      , securePort       = "50052"
                      , securePubCert    = pubCert
                  , secureChainCerts = []
                  , securePrivKey    = privKey
                  }
              }

          | otherwise = Server.ServerConfig {
                serverTracer   = nullTracer
              , serverInsecure = Just Server.InsecureConfig {
                    insecureHost = Nothing
                  , insecurePort = "50051"
                  }
              , serverSecure   = Nothing
              }

        serverParams :: Server.ServerParams
        serverParams = Server.ServerParams {
              serverDebugTracer = nullTracer
            , serverCompression = serverCompr cfg
            }

    Server.withServer serverParams serverHandlers $
      Server.runServer serverConfig

testClient :: forall a.
     ClientServerConfig
  -> (Client.Connection -> IO a)
  -> IO a
testClient cfg clientRun = do
    pubCert <- getDataFileName "grpc-demo.cert"

    let clientParams :: Client.ConnParams
        clientParams = Client.ConnParams {
              connDebugTracer    = nullTracer
            , connCompression    = clientCompr cfg
            , connDefaultTimeout = Nothing
            }

        clientServer :: Client.Server
        clientServer
          | useTLS cfg
          = Client.ServerSecure
              (Client.ValidateServer $ Client.certStoreFromPath pubCert)
              clientAuthority

          | otherwise
          = Client.ServerInsecure
              clientAuthority

        clientAuthority :: Client.Authority
        clientAuthority
          | useTLS cfg
          = Client.Authority {
                authorityHost = "localhost"
              , authorityPort = 50052
              }

          | otherwise
          = Client.Authority {
                authorityHost = "localhost"
              , authorityPort = 50051
              }

    Client.withConnection clientParams clientServer $ \conn ->
      clientRun conn

testClientServer :: forall a.
     ClientServerConfig
  -> (Client.Connection -> IO a)
  -> [Server.RpcHandler IO]
  -> IO a
testClientServer cfg clientRun serverHandlers = do
    withAsync (testServer cfg serverHandlers) $ \_serverThread -> do
      -- TODO: This threadDelay is obviously awful. When we fix proper
      -- wait-for-ready semantics, we can avoid it.
      threadDelay 100_000
      testClient cfg clientRun
