-- | Convenience functions for running a HTTP2 server
--
-- Intended for unqualified import.
module Network.GRPC.Server.Run (
    -- * Configuration
    ServerConfig(..)
  , ServerSetup(..)
  , InsecureConfig(..)
  , SecureConfig(..)
    -- * Run the server
  , runServer
  ) where

import Control.Exception
import Data.Default
import Network.ByteOrder (BufferSize)
import Network.HTTP2.Server qualified as HTTP2
import Network.HTTP2.TLS.Server qualified as HTTP2.TLS
import Network.Run.TCP (runTCPServer)
import Network.Socket (HostName, ServiceName, PortNumber)
import Network.TLS qualified as TLS

import Network.GRPC.Util.TLS (SslKeyLog(..))
import Network.GRPC.Util.TLS qualified as Util.TLS

import Debug.Concurrent

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data ServerConfig = ServerConfig {
      -- | General settings
      serverSetup :: ServerSetup

      -- | Configuration for insecure communication (without TLS)
      --
      -- Set to 'Nothing' to disable.
    , serverInsecure :: Maybe InsecureConfig

      -- | Configuration for secure communication (over TLS)
      --
      -- Set to 'Nothing' to disable.
    , serverSecure :: Maybe SecureConfig
    }

data ServerSetup = ServerSetup {
      -- | Low-level handler hook
      --
      -- This hook will be wrapped around each handler invocation, and can in
      -- principle modify nearly every aspect of the handler execution.
      --
      -- Use with care.
      serverHandlerHook :: HTTP2.Server -> HTTP2.Server
    }

instance Default ServerSetup where
  def = ServerSetup {
        serverHandlerHook = id
      }

data InsecureConfig = InsecureConfig {
      -- | Hostname
      insecureHost :: Maybe HostName

      -- | Port number
    , insecurePort :: ServiceName
    }
  deriving (Show)

data SecureConfig = SecureConfig {
      -- | Hostname
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

runServer :: ServerConfig -> HTTP2.Server -> IO ()
runServer ServerConfig{serverSetup, serverInsecure, serverSecure} server =
    concurrently_
      (maybe (return ()) (runInsecure serverSetup server) serverInsecure)
      (maybe (return ()) (runSecure   serverSetup server) serverSecure)

{-------------------------------------------------------------------------------
  Insecure
-------------------------------------------------------------------------------}

runInsecure :: ServerSetup -> HTTP2.Server -> InsecureConfig -> IO ()
runInsecure ServerSetup{serverHandlerHook} server cfg =
    runTCPServer (insecureHost cfg) (insecurePort cfg) $ \sock -> do
      bracket (HTTP2.allocSimpleConfig sock writeBufferSize)
              HTTP2.freeSimpleConfig $ \config ->
        HTTP2.run HTTP2.defaultServerConfig config $ serverHandlerHook server

{-------------------------------------------------------------------------------
  Secure (over TLS)
-------------------------------------------------------------------------------}

runSecure :: ServerSetup -> HTTP2.Server -> SecureConfig -> IO ()
runSecure  ServerSetup{serverHandlerHook} server cfg = do
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
      (serverHandlerHook server)

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

