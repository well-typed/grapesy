{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

#include "MachDeps.h"

-- | Convenience functions for running a HTTP2 server
--
-- Intended for unqualified import.
module Network.GRPC.Server.Run (
    -- * Configuration
    ServerConfig(..)
  , InsecureConfig(..)
  , SecureConfig(..)
    -- * Simple interface
  , runServer
  , runServerWithHandlers
    -- * Full interface
  , RunningServer -- opaque
  , forkServer
  , waitServer
  , waitServerSTM
  , getInsecureSocket
  , getSecureSocket
  , getServerSocket
  , getServerPort
    -- * Exceptions
  , ServerTerminated(..)
  , CouldNotLoadCredentials(..)
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Maybe (fromMaybe)
import Network.ByteOrder (BufferSize)
import Network.HTTP2.Server qualified as HTTP2
import Network.HTTP2.TLS.Server qualified as HTTP2.TLS
import Network.Run.TCP qualified as Run
import Network.Socket
import Network.TLS qualified as TLS

import Network.GRPC.Common.HTTP2Settings
import Network.GRPC.Server
import Network.GRPC.Util.HTTP2 (allocConfigWithTimeout)
import Network.GRPC.Util.TLS (SslKeyLog(..))
import Network.GRPC.Util.TLS qualified as Util.TLS

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Server configuration
--
-- Describes the configuration of both an insecure server and a secure server.
-- See the documentation of 'runServer' for a description of what servers will
-- result from various configurations.
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

-- | Offer insecure connection (no TLS)
data InsecureConfig = InsecureConfig {
      -- | Hostname
      insecureHost :: Maybe HostName

      -- | Port number
      --
      -- Can use @0@ to let the server pick its own port. This can be useful in
      -- testing scenarios; see 'getServerPort' or the more general
      -- 'getInsecureSocket' for a way to figure out what this port actually is.
    , insecurePort :: PortNumber
    }
  deriving (Show)

-- | Offer secure connection (over TLS)
data SecureConfig = SecureConfig {
      -- | Hostname to bind to
      --
      -- Unlike in 'InsecureConfig', the 'HostName' is required here, because it
      -- must match the certificate.
      --
      -- This doesn't need to match the common name (CN) in the TLS certificate.
      -- For example, if the client connects to @127.0.0.1@, and the certificate
      -- CN is also @127.0.0.1@, the server can still bind to @0.0.0.0@.
      secureHost :: HostName

      -- | Port number
      --
      -- See 'insecurePort' for additional discussion.
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
  Simple interface
-------------------------------------------------------------------------------}

-- | Run a 'HTTP2.Server' with the given 'ServerConfig'.
--
-- If both configurations are disabled, 'runServer' will simply immediately
-- return. If both configurations are enabled, then two servers will be run
-- concurrently; one with the insecure configuration and the other with the
-- secure configuration. Obviously, if only one of the configurations is
-- enabled, then just that server will be run.
--
-- See also 'runServerWithHandlers', which handles the creation of the
-- 'HTTP2.Server' for you.
runServer :: ServerParams -> ServerConfig -> HTTP2.Server -> IO ()
runServer params cfg server = forkServer params cfg server $ waitServer

-- | Convenience function that combines 'runServer' with 'mkGrpcServer'
runServerWithHandlers ::
     ServerParams
  -> ServerConfig
  -> [SomeRpcHandler IO]
  -> IO ()
runServerWithHandlers params config handlers = do
    server <- mkGrpcServer params handlers
    runServer params config server

{-------------------------------------------------------------------------------
  Full interface
-------------------------------------------------------------------------------}

data RunningServer = RunningServer {
      -- | Insecure server (no TLS)
      --
      -- If the insecure server is disabled, this will be a trivial "Async' that
      -- immediately completes.
      runningServerInsecure :: Async ()

      -- | Secure server (with TLS)
      --
      -- Similar remarks apply as for 'runningInsecure'.
    , runningServerSecure :: Async ()

      -- | Socket used by the insecure server
      --
      -- See 'getInsecureSocket'.
    , runningSocketInsecure :: TMVar Socket

      -- | Socket used by the secure server
      --
      -- See 'getSecureSocket'.
    , runningSocketSecure :: TMVar Socket
    }

data ServerTerminated = ServerTerminated
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Start the server
forkServer ::
     ServerParams
  -> ServerConfig
  -> HTTP2.Server
  -> (RunningServer -> IO a)
  -> IO a
forkServer params ServerConfig{serverInsecure, serverSecure} server k = do
    runningSocketInsecure <- newEmptyTMVarIO
    runningSocketSecure   <- newEmptyTMVarIO

    let secure, insecure :: IO ()
        insecure =
          case serverInsecure of
            Nothing  -> return ()
            Just cfg -> runInsecure params cfg runningSocketInsecure server
        secure =
          case serverSecure of
            Nothing  -> return ()
            Just cfg -> runSecure params cfg runningSocketSecure server

    withAsync insecure $ \runningServerInsecure ->
      withAsync secure $ \runningServerSecure ->
        k RunningServer{
              runningServerInsecure
            , runningServerSecure
            , runningSocketInsecure
            , runningSocketSecure
            }

-- | Wait for the server to terminate
--
-- Returns the results of the insecure and secure servers separately.
-- Note that under normal circumstances the server /never/ terminates.
waitServerSTM ::
     RunningServer
  -> STM ( Either SomeException ()
         , Either SomeException ()
         )
waitServerSTM server = do
    insecure <- waitCatchSTM (runningServerInsecure server)
    secure   <- waitCatchSTM (runningServerSecure   server)
    return (insecure, secure)

-- | IO version of 'waitServerSTM' that rethrows exceptions
waitServer :: RunningServer -> IO ()
waitServer server =
    atomically (waitServerSTM server) >>= \case
      (Right (), Right ()) -> return ()
      (Left  e , _       ) -> throwIO e
      (_       , Left  e ) -> throwIO e

-- | Get the socket used by the insecure server
--
-- The socket is created as the server initializes; this function will block
-- until that is complete. However:
--
-- * If the server throws an exception, that exception is rethrown here.
-- * If the server has already terminated, we throw 'ServerTerminated'
-- * If the insecure server was not enabled, it is considered to have terminated
--   immediately and the same 'ServerTerminated' exception is thrown.
getInsecureSocket :: RunningServer -> STM Socket
getInsecureSocket server = do
    getSocket (runningServerInsecure server)
              (runningSocketInsecure server)

-- | Get the socket used by the secure server
--
-- Similar remarks apply as for 'getInsecureSocket'.
getSecureSocket :: RunningServer -> STM Socket
getSecureSocket server = do
    getSocket (runningServerSecure server)
              (runningSocketSecure server)

-- | Get \"the\" socket associated with the server
--
-- Precondition: only one server must be enabled (secure or insecure).
getServerSocket :: RunningServer -> STM Socket
getServerSocket server = do
    insecure <- catchSTM (Right <$> getInsecureSocket server) (return . Left)
    secure   <- catchSTM (Right <$> getSecureSocket   server) (return . Left)
    case (insecure, secure) of
      (Right sock, Left ServerTerminated) ->
        return sock
      (Left ServerTerminated, Right sock) ->
        return sock
      (Left ServerTerminated, Left ServerTerminated) ->
        throwSTM ServerTerminated
      (Right _, Right _) ->
        error $ "getServerSocket: precondition violated"

-- | Get \"the\" port number used by the server
--
-- Precondition: only one server must be enabled (secure or insecure).
getServerPort :: RunningServer -> IO PortNumber
getServerPort server = do
    sock <- atomically $ getServerSocket server
    addr <- getSocketName sock
    case addr of
      SockAddrInet  port   _host   -> return port
      SockAddrInet6 port _ _host _ -> return port
      SockAddrUnix{} -> error "getServerPort: unexpected unix socket"

-- | Internal generalization of 'getInsecureSocket'/'getSecureSocket'
getSocket :: Async () -> TMVar Socket -> STM Socket
getSocket serverAsync socketTMVar = do
    status <-  (Left  <$> waitCatchSTM serverAsync)
      `orElse` (Right <$> readTMVar    socketTMVar)
    case status of
      Left (Left err) -> throwSTM err
      Left (Right ()) -> throwSTM $ ServerTerminated
      Right sock      -> return sock

{-------------------------------------------------------------------------------
  Insecure
-------------------------------------------------------------------------------}

runInsecure ::
     ServerParams
  -> InsecureConfig
  -> TMVar Socket
  -> HTTP2.Server
  -> IO ()
runInsecure params cfg socketTMVar server = do
    withServerSocket
        serverHTTP2Settings
        socketTMVar
        (insecureHost cfg)
        (insecurePort cfg) $ \listenSock -> do
      Run.runTCPServerWithSocket listenSock $ \clientSock -> do
        when (http2TcpNoDelay serverHTTP2Settings) $
          -- See description of 'withServerSocket'
          setSockOpt clientSock NoDelay True
        bracket ( allocConfigWithTimeout
                    clientSock
                    writeBufferSize
                    disableTimeout
                )
                HTTP2.freeSimpleConfig $ \config ->
          HTTP2.run serverConfig config server
  where
    ServerParams{
        serverOverrideNumberOfWorkers
      , serverHTTP2Settings
      } = params

    serverConfig :: HTTP2.ServerConfig
    serverConfig = HTTP2.defaultServerConfig {
          HTTP2.numberOfWorkers =
              fromMaybe
                (HTTP2.numberOfWorkers HTTP2.defaultServerConfig)
                (fromIntegral <$> serverOverrideNumberOfWorkers)
        , HTTP2.connectionWindowSize =
              fromIntegral $ http2ConnectionWindowSize serverHTTP2Settings
        , HTTP2.settings =
              HTTP2.defaultSettings {
                  HTTP2.initialWindowSize =
                      fromIntegral $
                        http2StreamWindowSize serverHTTP2Settings
                , HTTP2.maxConcurrentStreams =
                      Just . fromIntegral $
                        http2MaxConcurrentStreams serverHTTP2Settings
                }
        }

{-------------------------------------------------------------------------------
  Secure (over TLS)
-------------------------------------------------------------------------------}

runSecure ::
     ServerParams
  -> SecureConfig
  -> TMVar Socket
  -> HTTP2.Server
  -> IO ()
runSecure params cfg socketTMVar server = do
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
              HTTP2.TLS.settingsKeyLogger =
                keyLogger
            , HTTP2.TLS.settingsTimeout =
                disableTimeout
            , HTTP2.TLS.settingsNumberOfWorkers =
                fromMaybe
                  (HTTP2.TLS.settingsNumberOfWorkers HTTP2.TLS.defaultSettings)
                  (fromIntegral <$> serverOverrideNumberOfWorkers)
            , HTTP2.TLS.settingsConnectionWindowSize =
                fromIntegral $ http2ConnectionWindowSize serverHTTP2Settings
            , HTTP2.TLS.settingsStreamWindowSize =
                fromIntegral $ http2StreamWindowSize serverHTTP2Settings
            , HTTP2.TLS.settingsConcurrentStreams =
                fromIntegral $ http2MaxConcurrentStreams serverHTTP2Settings
            }

    withServerSocket
        serverHTTP2Settings
        socketTMVar
        (Just $ secureHost cfg)
        (securePort cfg) $ \listenSock ->
      -- TODO: We should really set NoDelay on the clientSock, but we have
      -- no access to it.
      HTTP2.TLS.runWithSocket
        settings
        (TLS.Credentials [cred])
        listenSock
        server
  where
    ServerParams{
        serverOverrideNumberOfWorkers
      , serverHTTP2Settings
      } = params

data CouldNotLoadCredentials =
    -- | Failed to load server credentials
    CouldNotLoadCredentials String
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Work around the fact that we cannot disable timeouts in http2/http2-tls
--
-- TODO: <https://github.com/well-typed/grapesy/issues/123>
-- We need a proper solution for this.
disableTimeout :: Int
disableTimeout =
#if (WORD_SIZE_IN_BITS == 64)
    -- Set a really high timeout to effectively disable timeouts
    --
    -- We cannot use `maxBound` here, because this is value in
    -- seconds which will be multiplied by 1_000_000 to get a value
    -- in microseconds; `maxBound` would result in overflow.
    1000000000 -- roughly 30 years
#else
#warning "Timeout for RPC messages is set to 30 minutes on 32-bit systems."
#warning "See https://github.com/kazu-yamamoto/http2/issues/112"
    -- Unfortunately, the same trick does not work on 32-bit
    -- systems, where we simply don't have enough range. The
    -- maximum timeout we can support here is 30 mins.
    30 * 60
#endif

-- | Create server listen socket
--
-- We set @TCP_NODELAY@ on the server listen socket, but there is no guarantee
-- that the option will be inherited by the sockets returned from @accept@ for
-- each client request. On Linux it seems to be, although I cannot find any
-- authoritative reference to say so; the best I could find is a section in Unix
-- Network Programming [1]. On FreeBSD on the other hand, the man page suggests
-- that @TCP_NODELAY@ is /not/ inherited [2].
--
-- Even the Linux man page for @accept@ is maddingly vague:
--
-- > Portable programs should not rely on inheritance or non‐inheritance of file
-- > status flags and always explicitly set all required flags on the socket
-- > returned from accept().
--
-- Whether that /file status/ flags is significant (as opposed to other kinds of
-- flags?) is unclear, especially in the second half of this sentence. The Linux
-- man page on @tcp@ is even worse; this is the only mention of inheritance in
-- entire page:
--
-- > [TCP_USER_TIMEOUT], like many others, will be inherited by the socket
-- > returned by accept(2), if it was set on the listening socket.
--
-- It is therefore best to explicitly set @TCP_NODELAY@ on the client request
-- socket.
--
-- [1] <https://notes.shichao.io/unp/ch7/#socket-states>
-- [2] <https://man.freebsd.org/cgi/man.cgi?query=tcp>
withServerSocket ::
     HTTP2Settings
  -> TMVar Socket
  -> Maybe HostName
  -> PortNumber
  -> (Socket -> IO a)
  -> IO a
withServerSocket http2Settings socketTMVar host port k = do
    addr <- Run.resolve Stream host (show port) [AI_PASSIVE]
    bracket (openServerSocket addr) close $ \sock -> do
      atomically $ putTMVar socketTMVar sock
      k sock
  where
    openServerSocket :: AddrInfo -> IO Socket
    openServerSocket = Run.openTCPServerSocketWithOptions $ concat [
          [ (NoDelay, 1)
          | http2TcpNoDelay http2Settings
          ]
        ]

writeBufferSize :: BufferSize
writeBufferSize = 4096
