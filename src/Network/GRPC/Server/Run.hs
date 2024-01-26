-- | Convenience functions for running a HTTP2 server
--
-- Intended for unqualified import.
module Network.GRPC.Server.Run (
    -- * Configuration
    ServerConfig(..)
  , InsecureConfig(..)
  , SecureConfig(..)
    -- * Run the server
  , runServer
  , runServerWithHandlers
  ) where

import Control.Exception
import Data.Default
import Network.ByteOrder (BufferSize)
import Network.HTTP2.Server qualified as HTTP2
import Network.HTTP2.TLS.Server qualified as HTTP2.TLS
import Network.Run.TCP (runTCPServer)
import Network.Socket (HostName, PortNumber)
import Network.TLS qualified as TLS

import Network.GRPC.Server (mkGrpcServer, ServerParams, RpcHandler)
import Network.GRPC.Util.TLS (SslKeyLog(..))
import Network.GRPC.Util.TLS qualified as Util.TLS

import Debug.Concurrent

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Server configuration
--
-- Describes the configuration of both an insecure server and a secure server.
-- See the documentation of 'runServer' for a description of what servers will
-- result from various configurations.
--
-- The default configuration enables the default insecure configuration only.
data ServerConfig = ServerConfig {
      -- | Configuration for insecure communication (without TLS)
      --
      -- Set to 'Nothing' to disable.
      serverInsecure :: Maybe InsecureConfig

      -- | Configuration for secure communication (over TLS)
      --
      -- Set to 'Nothing' to disable.
    , serverSecure :: Maybe SecureConfig
    }

instance Default ServerConfig where
  def = ServerConfig {
        serverInsecure = Just def
      , serverSecure   = Nothing
      }

-- | Offer insecure connection (no TLS)
--
-- 'ServerConfig' defaults to an insecure connection on @localhost:50051@.
data InsecureConfig = InsecureConfig {
      -- | Hostname
      insecureHost :: Maybe HostName

      -- | Port number
    , insecurePort :: PortNumber
    }
  deriving (Show)

instance Default InsecureConfig where
  def = InsecureConfig {
      insecureHost = Nothing
    , insecurePort = 50051
    }

-- | Offer secure connection (over TLS)
data SecureConfig = SecureConfig {
      -- | Hostname to bind to
      --
      -- This doesn't need to match the common name (CN) in the TLS certificate.
      -- For example, if the client connects to @localhost@, and the certificate
      -- CN is also @localhost@, the server can still bind to @0.0.0.0@.
      secureHost :: HostName

      -- | Port number
    , securePort :: PortNumber

      -- | TLS public certificate (X.509 format)
    , securePubCert :: FilePath

      -- | TLS chain certificates (X.509 format)
    , secureChainCerts :: [FilePath]

      -- | TLS private key
    , securePrivKey :: FilePath

      -- | SSL key log
    , secureSslKeyLog :: SslKeyLog
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Run server
-------------------------------------------------------------------------------}

-- | Run a 'HTTP2.Server' with the given 'ServerConfig'. Alternatively, you may
-- wish to use 'runServerWithHandlers', which handles the creation of the
-- 'HTTP2.Server' for you.
--
-- If both configurations are disabled, 'runServer' (and
-- 'runServerWithHandlers') will simply immediately evaluate to '()'. If both
-- configurations are enabled, then two servers will be run concurrently; one
-- with the insecure configuration and the other with the secure configuration.
--
-- Obviously, if only one of the configurations is enabled, then just that
-- server will be run.
runServer :: ServerConfig -> HTTP2.Server -> IO ()
runServer ServerConfig{serverInsecure, serverSecure} server =
    concurrently_
      (maybe (return ()) (runInsecure server) serverInsecure)
      (maybe (return ()) (runSecure   server) serverSecure)

-- | Convenience function that combines 'runServer' with 'mkGrpcServer'
runServerWithHandlers ::
     ServerConfig
  -> ServerParams
  -> [RpcHandler IO]
  -> IO ()
runServerWithHandlers config params handlers = do
    server <- mkGrpcServer params handlers
    runServer config server

{-------------------------------------------------------------------------------
  Insecure
-------------------------------------------------------------------------------}

runInsecure :: HTTP2.Server -> InsecureConfig -> IO ()
runInsecure server cfg =
    runTCPServer (insecureHost cfg) (show $ insecurePort cfg) $ \sock -> do
      bracket (HTTP2.allocSimpleConfig sock writeBufferSize)
              HTTP2.freeSimpleConfig $ \config ->
        HTTP2.run HTTP2.defaultServerConfig config server

{-------------------------------------------------------------------------------
  Secure (over TLS)
-------------------------------------------------------------------------------}

runSecure :: HTTP2.Server -> SecureConfig -> IO ()
runSecure server cfg = do
    cred :: TLS.Credential <-
          TLS.credentialLoadX509Chain
            (securePubCert    cfg)
            (secureChainCerts cfg)
            (securePrivKey    cfg)
      >>= \case
            Left  err -> throwIO $ CouldNotLoadCredentials err
            Right res -> return res

    keyLogger <- Util.TLS.keyLogger (secureSslKeyLog cfg)
    let settings :: HTTP2.TLS.Settings
        settings = HTTP2.TLS.defaultSettings {
              HTTP2.TLS.settingsKeyLogger = keyLogger
            }

    HTTP2.TLS.run
      settings
      (TLS.Credentials [cred])
      (secureHost cfg)
      (securePort cfg)
      server

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

writeBufferSize :: BufferSize
writeBufferSize = 4096

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data CouldNotLoadCredentials =
    -- | Failed to load server credentials
    CouldNotLoadCredentials String
  deriving stock (Show)
  deriving anyclass (Exception)

