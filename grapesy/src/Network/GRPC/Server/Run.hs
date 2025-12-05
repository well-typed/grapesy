{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Network.GRPC.Util.Imports

import Control.Concurrent.STM (STM, atomically, catchSTM, throwSTM, orElse)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, readTMVar)
import Network.HTTP2.Server qualified as HTTP2
import Network.HTTP2.TLS.Server qualified as HTTP2.TLS
import Network.Run.TCP qualified as Run
import Network.Socket (Socket, AddrInfo, HostName, PortNumber)
import Network.Socket qualified as Socket
import Network.TLS qualified as TLS

#if MIN_VERSION_network_run(0,4,4)
import Data.List.NonEmpty qualified as NE
#endif

import Network.GRPC.Common.HTTP2Settings
import Network.GRPC.Server
import Network.GRPC.Util.HTTP2
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
  deriving stock (Show, Generic)

-- | Offer insecure connection (no TLS)
data InsecureConfig =
    -- | Insecure TCP connection
    InsecureConfig {
      -- | Hostname
      insecureHost :: Maybe HostName

      -- | Port number
      --
      -- Can use @0@ to let the server pick its own port. This can be useful in
      -- testing scenarios; see 'getServerPort' or the more general
      -- 'getInsecureSocket' for a way to figure out what this port actually is.
    , insecurePort :: PortNumber
    }
    -- | Insecure (but local) Unix domain socket connection
  | InsecureUnix {
      -- | Path to the socket
      insecurePath :: FilePath
    }
  deriving stock (Show, Generic)

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
  deriving stock (Show, Generic)

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
runServer :: HTTP2Settings -> ServerConfig -> HTTP2.Server -> IO ()
runServer http2 cfg server = forkServer http2 cfg server $ waitServer

-- | Convenience function that combines 'runServer' with 'mkGrpcServer'
--
-- NOTE: If you want to override the 'HTTP2Settings', use 'runServer' instead.
runServerWithHandlers ::
     ServerParams
  -> ServerConfig
  -> [SomeRpcHandler IO]
  -> IO ()
runServerWithHandlers params config handlers = do
    server <- mkGrpcServer params handlers
    runServer http2 config server
  where
    http2 :: HTTP2Settings
    http2 = def

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
     HTTP2Settings
  -> ServerConfig
  -> HTTP2.Server
  -> (RunningServer -> IO a)
  -> IO a
forkServer http2 ServerConfig{serverInsecure, serverSecure} server k = do
    runningSocketInsecure <- newEmptyTMVarIO
    runningSocketSecure   <- newEmptyTMVarIO

    let secure, insecure :: IO ()
        insecure =
          case serverInsecure of
            Nothing  -> return ()
            Just cfg -> runInsecure http2 cfg runningSocketInsecure server
        secure =
          case serverSecure of
            Nothing  -> return ()
            Just cfg -> runSecure http2 cfg runningSocketSecure server

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
    addr <- Socket.getSocketName sock
    case addr of
      Socket.SockAddrInet  port   _host   -> return port
      Socket.SockAddrInet6 port _ _host _ -> return port
      Socket.SockAddrUnix{} -> error "getServerPort: unexpected unix socket"

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
     HTTP2Settings
  -> InsecureConfig
  -> TMVar Socket
  -> HTTP2.Server
  -> IO ()
runInsecure http2 cfg socketTMVar server = do
    openSock cfg $ \listenSock ->
      withTimeManager $ \mgr ->
        Run.runTCPServerWithSocket listenSock $ \clientSock -> do
          when (http2TcpNoDelay http2 && not isUnixSocket) $ do
            -- See description of 'withServerSocket'
            Socket.setSocketOption clientSock Socket.NoDelay 1
          when (http2TcpAbortiveClose http2) $ do
            Socket.setSockOpt clientSock Socket.Linger
              (Socket.StructLinger { Socket.sl_onoff = 1, Socket.sl_linger = 0 })
          withConfigForInsecure mgr clientSock $ \config ->
            HTTP2.run serverConfig config server
  where
    serverConfig :: HTTP2.ServerConfig
    serverConfig = mkServerConfig http2

    openSock :: InsecureConfig -> (Socket -> IO a) -> IO a
    openSock InsecureConfig{insecureHost, insecurePort} = do
      withServerSocket
        http2
        socketTMVar
        insecureHost
        insecurePort
    openSock InsecureUnix{insecurePath} = do
      withUnixSocket
        insecurePath
        socketTMVar

    isUnixSocket = case cfg of
      InsecureConfig{} -> False
      InsecureUnix{}   -> True

{-------------------------------------------------------------------------------
  Secure (over TLS)
-------------------------------------------------------------------------------}

runSecure ::
     HTTP2Settings
  -> SecureConfig
  -> TMVar Socket
  -> HTTP2.Server
  -> IO ()
runSecure http2 cfg socketTMVar server = do
    cred :: TLS.Credential <-
          TLS.credentialLoadX509Chain
            (securePubCert    cfg)
            (secureChainCerts cfg)
            (securePrivKey    cfg)
      >>= \case
            Left  err -> throwIO $ CouldNotLoadCredentials err
            Right res -> return res

    keyLogger <- Util.TLS.keyLogger (secureSslKeyLog cfg)
    let serverConfig :: HTTP2.ServerConfig
        serverConfig = mkServerConfig http2

        tlsSettings :: HTTP2.TLS.Settings
        tlsSettings = mkTlsSettings http2 keyLogger

    withServerSocket
        http2
        socketTMVar
        (Just $ secureHost cfg)
        (securePort cfg) $ \listenSock ->
      HTTP2.TLS.runTLSWithSocket
          tlsSettings
          (TLS.Credentials [cred])
          listenSock
          "h2" $ \mgr backend -> do
        when (http2TcpNoDelay http2) $
          -- See description of 'withServerSocket'
          Socket.setSocketOption (HTTP2.TLS.requestSock backend) Socket.NoDelay 1
        when (http2TcpAbortiveClose http2) $ do
          Socket.setSockOpt (HTTP2.TLS.requestSock backend) Socket.Linger
            (Socket.StructLinger { Socket.sl_onoff = 1, Socket.sl_linger = 0 })
        withConfigForSecure mgr backend $ \config ->
          HTTP2.run serverConfig config server

data CouldNotLoadCredentials =
    -- | Failed to load server credentials
    CouldNotLoadCredentials String
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

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
#if MIN_VERSION_network_run(0,4,4)
    addr <- Run.resolve Socket.Stream host (show port) [Socket.AI_PASSIVE] NE.head
#else
    addr <- Run.resolve Socket.Stream host (show port) [Socket.AI_PASSIVE]
#endif
    bracket (openServerSocket addr) Socket.close $ \sock -> do
      atomically $ putTMVar socketTMVar sock
      k sock
  where
    openServerSocket :: AddrInfo -> IO Socket
    openServerSocket = Run.openTCPServerSocketWithOptions $ concat [
          [ (Socket.NoDelay, 1)
          | http2TcpNoDelay http2Settings
          ]
        ]

-- | Create a Unix domain socket
--
-- Note that @TCP_NODELAY@ should not be set on unix domain sockets,
-- otherwise clients would get their connection closed immediately.
withUnixSocket :: FilePath -> TMVar Socket -> (Socket -> IO a) -> IO a
withUnixSocket path socketTMVar k = do
    bracket openServerSocket Socket.close $ \sock -> do
      atomically $ putTMVar socketTMVar sock
      k sock
  where
    openServerSocket :: IO Socket
    openServerSocket = do
      sock <- Socket.socket Socket.AF_UNIX Socket.Stream 0
      Socket.setSocketOption sock Socket.ReuseAddr 1
      Socket.withFdSocket sock Socket.setCloseOnExecIfNeeded
      Socket.bind sock $ Socket.SockAddrUnix path
      Socket.listen sock 1024
      return sock
