{-# LANGUAGE OverloadedStrings #-}

module Test.Util.ClientServer (
    -- * Configuraiton
    ClientServerConfig(..)
    -- ** TLS
  , TlsSetup(..)
  , TlsOk(..)
  , TlsFail(..)
    -- ** Evaluation
  , isExpectedException
    -- * Run
  , runTestClientServer
    -- ** Lower-level functionality
  , runTestClient
  , runTestServer
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Tracer
import Data.Default
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Network.TLS

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Common.Exceptions
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

-- | Check if the given configuration gives rise to expected exceptions
isExpectedException :: ClientServerConfig -> SomeException -> Bool
isExpectedException cfg err
    | Just (tlsException :: TLSException) <- fromException err
    = case (useTLS cfg, tlsException) of
        (   Just (TlsFail TlsFailValidation)
          , HandshakeFailed (Error_Protocol (_msg, _bool, UnknownCa))
          ) ->
          True
        (   Just (TlsFail TlsFailHostname)
          , HandshakeFailed (Error_Protocol (_msg, _bool, CertificateUnknown))
          ) ->
           True
        (   Just (TlsFail TlsFailUnsupported)
          , HandshakeFailed (Error_Packet_Parsing _)
          ) ->
           True
        _otherwise ->
          False

    | Just (grpcException :: GrpcException) <- fromException err
    = if | Set.disjoint (Map.keysSet (Compr.supported (clientCompr cfg)))
                        (Map.keysSet (Compr.supported (serverCompr cfg)))
         , GrpcUnknown <- grpcError grpcException
         , Just msg <- grpcErrorMessage grpcException
         , "CompressionNegotationFailed" `Text.isInfixOf` msg
         -> True

         | otherwise
         -> False

    | Just (threadCancelled :: ThreadCancelled) <- fromException err
    = isExpectedException cfg (threadCancelledReason threadCancelled)

    | Just (channelClosed :: ChannelClosed) <- fromException err
    = isExpectedException cfg (channelClosedReason channelClosed)

    | otherwise
    = False

{-------------------------------------------------------------------------------
  Server
-------------------------------------------------------------------------------}

runTestServer ::
     ClientServerConfig
  -> [Server.RpcHandler IO]
  -> IO ()
runTestServer cfg serverHandlers = do
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

runTestClient :: forall a.
     ClientServerConfig
  -> (Client.Connection -> IO a)
  -> IO a
runTestClient cfg clientRun = do
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

runTestClientServer :: forall a.
     ClientServerConfig
  -> (Client.Connection -> IO a)
  -> [Server.RpcHandler IO]
  -> IO a
runTestClientServer cfg clientRun serverHandlers = do
    withAsync (runTestServer cfg serverHandlers) $ \_serverThread -> do
      -- TODO: This threadDelay is obviously awful. When we fix proper
      -- wait-for-ready semantics, we can avoid it.
      threadDelay 100_000
      runTestClient cfg clientRun
