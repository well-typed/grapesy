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
    -- * Logging
  , LogMsg(..)
    -- * Low-level utilities
    --
    -- These are useful if the infrastructure provided in this module is not
    -- general enough.
  , allocTlsConfig
  , freeTlsConfig
  ) where

import Control.Concurrent.Async
import Control.Exception
import Control.Tracer
import Data.ByteString qualified as Strict (ByteString)
import Data.Default
import Network.ByteOrder (BufferSize)
import Network.HTTP2.Server qualified as HTTP2
import Network.Run.TCP (runTCPServer)
import Network.Socket (Socket, HostName, ServiceName)
import Network.TLS qualified as TLS

import Network.GRPC.Util.HTTP2.TLS
import Network.GRPC.Util.TLS qualified as Util.TLS

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data ServerConfig = ServerConfig {
      serverTracer   :: Tracer IO LogMsg

      -- | Configuration for insecure communication (without TLS)
      --
      -- Set to 'Nothing' to disable.
    , serverInsecure :: Maybe InsecureConfig

      -- | Configuration for secure communication (over TLS)
      --
      -- Set to 'Nothing' to disable.
    , serverSecure   :: Maybe SecureConfig
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
      secureHost :: Maybe HostName

      -- | Port number
    , securePort :: ServiceName

      -- | TLS public certificate (X.509 format)
    , securePubCert :: FilePath

      -- | TLS chain certificates (X.509 format)
    , secureChainCerts :: [FilePath]

      -- | TLS private key
    , securePrivKey :: FilePath
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Run server
-------------------------------------------------------------------------------}

runServer :: ServerConfig -> HTTP2.Server -> IO ()
runServer ServerConfig{serverTracer, serverInsecure, serverSecure} server =
    concurrently_
      (maybe (return ()) (runInsecure serverTracer server) serverInsecure)
      (maybe (return ()) (runSecure   serverTracer server) serverSecure)

{-------------------------------------------------------------------------------
  Insecure
-------------------------------------------------------------------------------}

runInsecure :: Tracer IO LogMsg -> HTTP2.Server -> InsecureConfig -> IO ()
runInsecure tracer server cfg =
    runTCPServer (insecureHost cfg) (insecurePort cfg) $ \sock -> do
      traceWith tracer $ InsecureClientConnected
      bracket (HTTP2.allocSimpleConfig sock writeBufferSize)
              HTTP2.freeSimpleConfig $ \config ->
        HTTP2.run config server

{-------------------------------------------------------------------------------
  Secure (over TLS)
-------------------------------------------------------------------------------}

runSecure :: Tracer IO LogMsg -> HTTP2.Server -> SecureConfig -> IO ()
runSecure tracer server cfg = do
    cred :: TLS.Credential <-
          TLS.credentialLoadX509Chain
            (securePubCert    cfg)
            (secureChainCerts cfg)
            (securePrivKey    cfg)
      >>= \case
            Left  err -> throwIO $ CouldNotLoadCredentials err
            Right res -> return res

    runTCPServer (secureHost cfg) (securePort cfg) $ \sock -> do
      -- New client connected
      traceWith tracer $ SecureClientConnected

      -- TLS handshake
      tlsContext :: TLS.Context <- newTlsContext sock $ TLS.Credentials [cred]
      TLS.handshake tlsContext
      protocol <- TLS.getNegotiatedProtocol tlsContext
      traceWith tracer $ TlsHandshakeComplete protocol

      -- Run http2 over TLS
      bracket (allocTlsConfig tlsContext writeBufferSize)
              (freeTlsConfig tlsContext) $ \config ->
        HTTP2.run config server

{-------------------------------------------------------------------------------
  Auxiliary: construct TLS context
-------------------------------------------------------------------------------}

newTlsContext :: Socket -> TLS.Credentials -> IO TLS.Context
newTlsContext s creds = do
    debugParams <- Util.TLS.debugParams
    TLS.contextNew s def {
        TLS.serverShared    = Util.TLS.serverShared creds
      , TLS.serverDebug     = debugParams
      , TLS.serverSupported = Util.TLS.supported
      , TLS.serverHooks     = Util.TLS.serverHooks
      }

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

writeBufferSize :: BufferSize
writeBufferSize = 4096

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data LogMsg =
    -- | New client connected without TLS
    InsecureClientConnected

    -- | New client connected over TLS
  | SecureClientConnected

    -- | TLS handshake complete
    --
    -- We record the negotiated protocol (ALPN); we expect this to be "h2".
   | TlsHandshakeComplete (Maybe Strict.ByteString)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data CouldNotLoadCredentials =
    -- | Failed to load server credentials
    CouldNotLoadCredentials String
  deriving stock (Show)
  deriving anyclass (Exception)

