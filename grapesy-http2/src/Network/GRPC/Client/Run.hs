{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Establishing connection to a server
--
module Network.GRPC.Client.Run (
    -- * Definition
    Connection -- opaque
  , withConnection
  , openConnection
  , closeConnection
    -- * Configuration
  , Server(..)
  , ServerValidation(..)
  , SslKeyLog(..)
  , ConnParams(..)
  , ReconnectPolicy(..)
  , ReconnectDecision(..)
  , Reconnect(..)
  , OnConnection(..)
  , ReconnectTo(..)
  , exponentialBackoff
    -- * Using the connection
  , connParams
  , getConnectionToServer
  , getOutboundCompression
  , updateConnectionMeta
  ) where

import Network.GRPC.Client.Connection

import Network.GRPC.Util.Imports

import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, writeTVar, readTVar)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar)
import Network.HPACK qualified as HPACK
import Network.HTTP2.Client qualified as HTTP2.Client
import Network.HTTP2.TLS.Client qualified as HTTP2.TLS.Client
import Network.Run.TCP qualified as Run
import Network.Socket (Socket, AddrInfo, StructLinger (..), SocketOption (..), SockOptValue (..))
import Network.Socket qualified as Socket
import Network.TLS (TLSException)

import Network.GRPC.Client.Meta qualified as Meta
import Network.GRPC.Common.HTTP2Settings
import Network.GRPC.Util.GHC
import Network.GRPC.Util.Session.Client qualified as Session
import Network.GRPC.Util.TLS qualified as Util.TLS

{-------------------------------------------------------------------------------
  Open a new connection
-------------------------------------------------------------------------------}

-- | Open a connection to the server.
--
-- See 'Network.GRPC.Client.withRPC' for making individual RPCs on the new
-- connection.
--
-- The connection to the server is set up asynchronously; the first call to
-- 'withRPC' will block until the connection has been established.
--
-- If the server cannot be reached, the behaviour depends on
-- 'connReconnectPolicy': if the policy allows reconnection attempts, we will
-- wait the time specified by the policy and try again. This implements the gRPC
-- "Wait for ready" semantics.
--
-- If the connection to the server is lost /after/ it has been established, any
-- currently ongoing RPC calls will be closed; attempts at further communication
-- on any of these calls will result in a 'ServerDisconnected' exception being
-- thrown. If that exception is caught, and the 'ReconnectPolicy' allows, we
-- will automatically try to re-establish a connection to the server. This can
-- be especially important when there is a proxy between the client and the
-- server, which may drop an existing connection after a certain period.
--
-- NOTE: The /default/ 'ReconnectPolicy' is 'DontReconnect', as per the gRPC
-- specification of "Wait for ready" semantics. You may wish to override this
-- default.
--
-- Clients should prefer sending many calls on a single connection, rather than
-- sending few calls on many connections, as minimizing the number of
-- connections used via this interface results in better memory behavior. See
-- [well-typed/grapesy#134](https://github.com/well-typed/grapesy/issues/133)
-- for discussion.
withConnection ::
     ConnParams
  -> Server
  -> (Connection -> IO a)
  -> IO a
withConnection connParams server k = do
    bracket (openConnection connParams server) closeConnection k

-- | Open a connection to the server.
--
-- See 'withConnection' for details.
--
-- __Warning:__
-- Connections hold open resources and must be closed using 'closeConnection'.
-- To prevent resource and memory leaks due to asynchronous exceptions, it is
-- recommended to use the bracketed function 'withConnection' whenever
-- possible, and otherwise run functions that allocate and release a resource
-- with asynchronous exceptions masked, and ensure that every use allocate
-- operation is followed by the corresponding release operation even in the
-- presence of asynchronous exceptions, e.g., using 'bracket'.
openConnection :: ConnParams -> Server -> IO Connection
openConnection connParams server = do
    connMetaVar  <- newMVar $ Meta.init (connInitCompression connParams)
    connStateVar <- newTVarIO ConnectionNotReady

    connOutOfScope <- newEmptyMVar
    let stayConnectedThread :: IO ()
        stayConnectedThread =
            stayConnected connParams server connStateVar connOutOfScope

    -- We don't use withAsync because we want the thread to terminate cleanly
    -- when we no longer need the connection (which we indicate by writing to
    -- connOutOfScope).
    void $ forkLabelled "grapesy:stayConnected" $ stayConnectedThread
    pure Connection {connParams, connMetaVar, connStateVar, connOutOfScope}

-- | Close a connection to the server.
closeConnection :: Connection -> IO ()
closeConnection conn = putMVar (connOutOfScope conn) ()

{-------------------------------------------------------------------------------
  Fatal exceptions (no point reconnecting)
-------------------------------------------------------------------------------}

isFatalException :: SomeException -> Bool
isFatalException err
  | Just (_tlsException :: TLSException) <- fromException err
  = True

  | otherwise
  = False



{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Connection attempt
--
-- This is an internal data structure used only in 'stayConnected' and helpers.
data Attempt = ConnectionAttempt {
      attemptParams       :: ConnParams
    , attemptOnConnection :: OnConnection
    , attemptState        :: TVar ConnectionState
    , attemptOutOfScope   :: MVar ()
    , attemptClosed       :: TMVar (Maybe SomeException)
    }

newConnectionAttempt ::
     ConnParams
  -> OnConnection
  -> TVar ConnectionState
  -> MVar ()
  -> IO Attempt
newConnectionAttempt attemptParams
                     attemptOnConnection
                     attemptState
                     attemptOutOfScope = do
    attemptClosed <- newEmptyTMVarIO
    return ConnectionAttempt{
        attemptParams
      , attemptOnConnection
      , attemptState
      , attemptOutOfScope
      , attemptClosed
      }

-- | Stay connected to the server
stayConnected ::
     ConnParams
  -> Server
  -> TVar ConnectionState
  -> MVar ()
  -> IO ()
stayConnected connParams initialServer connStateVar connOutOfScope = do
    loop
      initialServer
      (connOnConnection connParams)
      (connReconnectPolicy connParams)
  where
    loop :: Server -> OnConnection -> ReconnectPolicy -> IO ()
    loop server onConnection remainingReconnectPolicy = do
        -- Start new attempt (this just allocates some internal state)
        attempt <- newConnectionAttempt connParams onConnection connStateVar connOutOfScope

        -- Just like in 'runHandler' on the server side, it is important that
        -- 'stayConnected' runs in a separate thread. If it does not, then the
        -- moment we disconnect @http2[-tls]@ will throw an exception and we
        -- will not get the chance to process any other messages. This is
        -- especially important when we fail to setup a call: the server will
        -- respond with an informative gRPC error message (which we will raise
        -- as a 'GrpcException' in the client), and then disconnect. If we do
        -- not call @run@ in a separate thread, the only exception we will see
        -- is the low-level exception reported by @http2@ (something about
        -- stream errors), rather than the informative gRPC exception we want.

        mRes <- try $
          case server of
            ServerInsecure addr ->
              connectInsecure connParams attempt addr
            ServerSecure validation sslKeyLog addr ->
              connectSecure connParams attempt validation sslKeyLog addr
            ServerUnix path ->
              connectUnix connParams attempt path

        thisReconnectPolicy <- atomically $ do
          putTMVar (attemptClosed attempt) $ either Just (\() -> Nothing) mRes
          connState <- readTVar connStateVar
          return $ case connState of
            ConnectionReady{}->
              -- Suppose we have a maximum of 5x to try and connect to a server.
              -- Then if we manage to connect, and /then/ lose the connection,
              -- we should have those same 5x tries again.
              connReconnectPolicy connParams
            _otherwise ->
              remainingReconnectPolicy

        case mRes of
          Right () -> do
            atomically $ writeTVar connStateVar $ ConnectionOutOfScope
          Left err
            | isFatalException err ->
                atomically $ writeTVar connStateVar $ ConnectionAbandoned err
            | otherwise -> do
                -- Mark the connection as not ready /before/ running the reconnt
                -- policy. This prevents any attempts to use the connection
                -- while the policy is running.
                atomically $ writeTVar connStateVar $ ConnectionNotReady
                runReconnectPolicy thisReconnectPolicy >>= \case
                  DontReconnect -> do
                    atomically $ writeTVar connStateVar $ ConnectionAbandoned err
                  DoReconnect reconnect -> do
                    let
                      nextServer =
                        case reconnectTo reconnect of
                          ReconnectToPrevious -> server
                          ReconnectToOriginal -> initialServer
                          ReconnectToNew new  -> new

                      onReconnect' =
                        case onReconnect reconnect of
                          Just act -> act
                          Nothing  -> connOnConnection connParams

                    loop nextServer onReconnect' $ nextPolicy reconnect

-- | Unix domain socket connection
connectUnix :: ConnParams -> Attempt -> FilePath -> IO ()
connectUnix connParams attempt path = do
  client <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  Socket.connect client $ Socket.SockAddrUnix path
  connectSocket connParams attempt "localhost" client

-- | Insecure connection (no TLS)
connectInsecure :: ConnParams -> Attempt -> Address -> IO ()
connectInsecure connParams attempt addr = do
    Run.runTCPClientWithSettings
        runSettings
        (addressHost addr)
        (show $ addressPort addr)
        $ connectSocket connParams attempt (authority addr)
  where
    ConnParams{connHTTP2Settings} = connParams

    runSettings :: Run.Settings
    runSettings = Run.defaultSettings {
          Run.settingsOpenClientSocket = openClientSocket connHTTP2Settings
        }

-- | Insecure connection over the given socket
connectSocket :: ConnParams -> Attempt -> String -> Socket -> IO ()
connectSocket connParams attempt connAuthority sock = do
    bracket (HTTP2.Client.allocSimpleConfig sock writeBufferSize)
            HTTP2.Client.freeSimpleConfig $ \conf ->
      HTTP2.Client.run clientConfig conf $ \sendRequest _aux -> do
        let conn = Session.ConnectionToServer sendRequest
        atomically $
          writeTVar (attemptState attempt) $
            ConnectionReady (attemptClosed attempt) conn
        runOnConnection $ attemptOnConnection attempt
        takeMVar $ attemptOutOfScope attempt
  where
    ConnParams{connHTTP2Settings} = connParams

    settings :: HTTP2.Client.Settings
    settings = HTTP2.Client.defaultSettings {
          HTTP2.Client.maxConcurrentStreams =
              Just . fromIntegral $
                http2MaxConcurrentStreams connHTTP2Settings
        , HTTP2.Client.initialWindowSize =
              fromIntegral $
                http2StreamWindowSize connHTTP2Settings
        }

    clientConfig :: HTTP2.Client.ClientConfig
    clientConfig = overrideRateLimits connParams $
        HTTP2.Client.defaultClientConfig {
            HTTP2.Client.authority = connAuthority
          , HTTP2.Client.settings = settings
          , HTTP2.Client.connectionWindowSize =
                fromIntegral $
                  http2ConnectionWindowSize connHTTP2Settings
          }

-- | Secure connection (using TLS)
connectSecure ::
     ConnParams
  -> Attempt
  -> ServerValidation
  -> SslKeyLog
  -> Address
  -> IO ()
connectSecure connParams attempt validation sslKeyLog addr = do
    keyLogger <- Util.TLS.keyLogger sslKeyLog
    caStore   <- Util.TLS.validationCAStore validation

    let settings :: HTTP2.TLS.Client.Settings
        settings = HTTP2.TLS.Client.defaultSettings {
              HTTP2.TLS.Client.settingsKeyLogger     = keyLogger
            , HTTP2.TLS.Client.settingsCAStore       = caStore
            , HTTP2.TLS.Client.settingsAddrInfoFlags = []

            , HTTP2.TLS.Client.settingsValidateCert =
                case validation of
                  ValidateServer _   -> True
                  NoServerValidation -> False
            , HTTP2.TLS.Client.settingsOpenClientSocket =
                openClientSocket connHTTP2Settings
            , HTTP2.TLS.Client.settingsConcurrentStreams = fromIntegral $
                http2MaxConcurrentStreams connHTTP2Settings
            , HTTP2.TLS.Client.settingsStreamWindowSize = fromIntegral $
                http2StreamWindowSize connHTTP2Settings
            , HTTP2.TLS.Client.settingsConnectionWindowSize = fromIntegral $
                http2ConnectionWindowSize connHTTP2Settings
            }

        clientConfig :: HTTP2.Client.ClientConfig
        clientConfig = overrideRateLimits connParams $
            HTTP2.TLS.Client.defaultClientConfig
              settings
              (authority addr)

    HTTP2.TLS.Client.runWithConfig
          clientConfig
          settings
          (addressHost addr)
          (addressPort addr)
        $ \sendRequest _aux -> do
      let conn = Session.ConnectionToServer sendRequest
      atomically $
        writeTVar (attemptState attempt) $
          ConnectionReady (attemptClosed attempt) conn
      runOnConnection $ attemptOnConnection attempt
      takeMVar $ attemptOutOfScope attempt
  where
    ConnParams{connHTTP2Settings} = connParams

-- | Authority
--
-- We omit the port number in the authority, for compatibility with TLS
-- SNI as well as the gRPC spec (the HTTP2 spec says the port number is
-- optional in the authority).
authority :: Address -> String
authority addr =
    case addressAuthority addr of
      Nothing   -> addressHost addr
      Just auth -> auth

-- | Override rate limits imposed by @http2@
overrideRateLimits ::
     ConnParams
  -> HTTP2.Client.ClientConfig -> HTTP2.Client.ClientConfig
overrideRateLimits connParams clientConfig = clientConfig {
      HTTP2.Client.settings = settings {
          HTTP2.Client.pingRateLimit =
            case http2OverridePingRateLimit (connHTTP2Settings connParams) of
              Nothing    -> HTTP2.Client.pingRateLimit settings
              Just limit -> limit
        , HTTP2.Client.emptyFrameRateLimit =
            case http2OverrideEmptyFrameRateLimit (connHTTP2Settings connParams) of
              Nothing    -> HTTP2.Client.emptyFrameRateLimit settings
              Just limit -> limit
        , HTTP2.Client.settingsRateLimit =
            case http2OverrideSettingsRateLimit (connHTTP2Settings connParams) of
              Nothing    -> HTTP2.Client.settingsRateLimit settings
              Just limit -> limit
        , HTTP2.Client.rstRateLimit =
            case http2OverrideRstRateLimit (connHTTP2Settings connParams) of
              Nothing    -> HTTP2.Client.rstRateLimit settings
              Just limit -> limit
        }
    }
  where
    settings :: HTTP2.Client.Settings
    settings = HTTP2.Client.settings clientConfig

{-------------------------------------------------------------------------------
  Auxiliary http2
-------------------------------------------------------------------------------}

openClientSocket :: HTTP2Settings -> AddrInfo -> IO Socket
openClientSocket http2Settings =
    Run.openClientSocketWithOpts socketOptions
  where
    socketOptions :: [(SocketOption, SockOptValue)]
    socketOptions = concat [
          [ ( NoDelay
            , SockOptValue @Int 1
            )
          | http2TcpNoDelay http2Settings
          ]
        , [ ( Linger
            , SockOptValue $ StructLinger { sl_onoff = 1, sl_linger = 0 }
            )
          | http2TcpAbortiveClose http2Settings
          ]
        ]

-- | Write-buffer size
--
-- See docs of 'confBufferSize', but importantly: "this value is announced
-- via SETTINGS_MAX_FRAME_SIZE to the peer."
--
-- Value of 4KB is taken from the example code.
writeBufferSize :: HPACK.BufferSize
writeBufferSize = 4096
