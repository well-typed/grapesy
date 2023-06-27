module Test.Util.ClientServer (
    -- * Configuraiton
    ClientServerConfig(..)
    -- ** TLS
  , TlsSetup(..)
  , TlsOk(..)
  , TlsFail(..)
    -- ** Evaluation
  , isTestFailure
    -- * Run
  , testClientServer
    -- ** Lower-level functionality
  , testClient
  , testServer
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Tracer
import Data.Default
import Data.Typeable
import Network.TLS

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Run qualified as Server

import Paths_grapesy

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data ClientServerConfig = ClientServerConfig {
      clientCompr :: Compression.Negotation
    , serverCompr :: Compression.Negotation
    , useTLS      :: Maybe TlsSetup
    }

instance Default ClientServerConfig where
  def = ClientServerConfig {
      clientCompr = Compression.none
    , serverCompr = Compression.none
    , useTLS      = Nothing
    }

{-------------------------------------------------------------------------------
  Configuration: TLS
-------------------------------------------------------------------------------}

-- | TLS setup
data TlsSetup = TlsOk TlsOk | TlsFail TlsFail

-- | TLS setup that we expect to work
data TlsOk =
    -- | Configure the client so that the server's cert is a known root
    --
    -- This means that client can validate the server's cert, even though it is
    -- self signed.
    TlsOkCertAsRoot

    -- | Configure the client to not validate the server's cert at all
  | TlsOkSkipValidation

-- | TLS setup that should result in an error
data TlsFail =
    -- | Don't take any special provisions in the client
    --
    -- This means that TLS validation will fail, since the server's cert is
    -- self signed.
    TlsFailValidation

    -- | Client uses the wrong address (127.0.0.1 instead of localhost)
    --
    -- This too will result in a TLS failure (hostname mismatch). To ensure we
    -- get the TLS failure we expect, we /do/ add the server's certificate to
    -- the client's trusted certs.
  | TlsFailHostname

    -- | The server is not configured for TLS
  | TlsFailUnsupported

{-------------------------------------------------------------------------------
  Config evaluation (do we expect an error?)
-------------------------------------------------------------------------------}

type TestFailure = String

isTestFailure :: ClientServerConfig -> SomeException -> Maybe TestFailure
isTestFailure cfg err
    | Just (tlsException :: TLSException) <- fromException err
    = case (useTLS cfg, tlsException) of
        (   Just (TlsFail TlsFailValidation)
          , HandshakeFailed (Error_Protocol (_msg, _bool, UnknownCa))
          ) ->
          Nothing
        (   Just (TlsFail TlsFailHostname)
          , HandshakeFailed (Error_Protocol (_msg, _bool, CertificateUnknown))
          ) ->
           Nothing
        (   Just (TlsFail TlsFailUnsupported)
          , HandshakeFailed (Error_Packet_Parsing _)
          ) ->
           Nothing
        _otherwise ->
          unexpectedException
    | otherwise
    = unexpectedException
  where
    unexpectedException :: Maybe TestFailure
    unexpectedException = Just $ concat [
          "Unexpected exception of type "
        , case err of
            SomeException e -> show (typeOf e)
        , ": "
        , show err
        ]

{-------------------------------------------------------------------------------
  Server
-------------------------------------------------------------------------------}

testServer ::
     ClientServerConfig
  -> [Server.RpcHandler IO]
  -> IO ()
testServer cfg serverHandlers = do
    pubCert <- getDataFileName "grpc-demo.cert"
    privKey <- getDataFileName "grpc-demo.priv"

    let serverConfig :: Server.ServerConfig
        serverConfig =
            case useTLS cfg of
              Nothing -> Server.ServerConfig {
                  serverTracer   = nullTracer
                , serverInsecure = Just Server.InsecureConfig {
                      insecureHost = Nothing
                    , insecurePort = "50051"
                    }
                , serverSecure   = Nothing
                }
              Just (TlsFail TlsFailUnsupported) -> Server.ServerConfig {
                  serverTracer   = nullTracer
                , serverInsecure = Just Server.InsecureConfig {
                      insecureHost = Nothing
                    , insecurePort = "50052"
                    }
                , serverSecure   = Nothing
                }
              Just _tlsSetup -> Server.ServerConfig {
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


        serverParams :: Server.ServerParams
        serverParams = Server.ServerParams {
              serverDebugTracer = nullTracer
            , serverCompression = serverCompr cfg
            }

    Server.withServer serverParams serverHandlers $
      Server.runServer serverConfig

{-------------------------------------------------------------------------------
  Client
-------------------------------------------------------------------------------}

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
        clientServer =
            case useTLS cfg of
              Just tlsSetup ->
                Client.ServerSecure
                  ( case tlsSetup of
                      TlsOk TlsOkCertAsRoot ->
                        correctClientSetup
                      TlsOk TlsOkSkipValidation ->
                        Client.NoServerValidation
                      TlsFail TlsFailValidation ->
                        Client.ValidateServer mempty
                      TlsFail TlsFailHostname ->
                        correctClientSetup
                      TlsFail TlsFailUnsupported ->
                        correctClientSetup
                  )
                  clientAuthority

              Nothing ->
                Client.ServerInsecure
                  clientAuthority
          where
            correctClientSetup :: Client.ServerValidation
            correctClientSetup =
                Client.ValidateServer $
                  Client.certStoreFromPath pubCert

        clientAuthority :: Client.Authority
        clientAuthority =
            case useTLS cfg of
              Just tlsSetup -> Client.Authority {
                  authorityHost = case tlsSetup of
                                    TlsFail TlsFailHostname -> "127.0.0.1"
                                    _otherwise              -> "localhost"
                , authorityPort = 50052
                }

              Nothing -> Client.Authority {
                  authorityHost = "localhost"
                , authorityPort = 50051
                }

    Client.withConnection clientParams clientServer $ \conn ->
      clientRun conn

{-------------------------------------------------------------------------------
  Main entry point: run server and client together
-------------------------------------------------------------------------------}

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
