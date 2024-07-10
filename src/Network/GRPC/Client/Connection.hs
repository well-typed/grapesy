{-# LANGUAGE OverloadedStrings #-}

-- | Connection to a server
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Client.Connection (Connection, withConnection)
-- > import Network.GRPC.Client.Connection qualified as Connection
module Network.GRPC.Client.Connection (
    -- * Definition
    Connection -- opaque
  , Call(..)
  , connParams
  , startRPC
  , withConnection
    -- * Configuration
  , Server(..)
  , ServerValidation(..)
  , SslKeyLog(..)
  , ConnParams(..)
  , ReconnectPolicy(..)
  , exponentialBackoff
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.Default
import Data.Foldable (asum)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Version
import GHC.Stack
import Network.HPACK qualified as HPACK
import Network.HTTP2.Client qualified as HTTP2.Client
import Network.HTTP2.TLS.Client qualified as HTTP2.TLS.Client
import Network.Run.TCP qualified as Run
import Network.Socket
import Network.TLS (TLSException)
import System.Random

import Network.GRPC.Client.Meta (Meta)
import Network.GRPC.Client.Meta qualified as Meta
import Network.GRPC.Client.Session
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Common.HTTP2Settings
import Network.GRPC.Spec
import Network.GRPC.Util.GHC
import Network.GRPC.Util.HTTP2.Stream (ServerDisconnected(..))
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Util.Thread
import Network.GRPC.Util.TLS (ServerValidation(..), SslKeyLog(..))
import Network.GRPC.Util.TLS qualified as Util.TLS

import Paths_grapesy qualified as Grapesy

{---------------------------------------------------2----------------------------
  Connection API

  'Connection' is kept abstract (opaque) in the user facing API.

  The closest concept on the server side concept is
  'Network.GRPC.Server.Context': this does not identify a connection from a
  particular client (@http2@ gives us each request separately, without
  identifying which requests come from the same client), but keeps track of the
  overall server state.
-------------------------------------------------------------------------------}

-- | Open connection to server
--
-- Before we can send RPC requests, we have to connect to a specific server
-- first. Once we have opened a connection to that server, we can send as many
-- RPC requests over that one connection as we wish. 'Connection' abstracts over
-- this connection, and also maintains some information about the server.
--
-- We can make many RPC calls over the same connection.
data Connection = Connection {
      -- | Configuration
      connParams :: ConnParams

      -- | Information about the open connection
    , connMetaVar :: MVar Meta

      -- | Connection state
    , connStateVar :: TVar ConnectionState
    }

-- | State of the call
--
-- This type is kept abstract (opaque) in the public facing API.
data Call rpc = SupportsClientRpc rpc => Call {
      callSession :: ClientSession rpc
    , callChannel :: Session.Channel (ClientSession rpc)
    }

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

-- | Connection configuration
--
-- You may wish to override 'connReconnectPolicy'.
data ConnParams = ConnParams {
      -- | Compression negotation
      connCompression :: Compr.Negotation

      -- | Default timeout
      --
      -- Individual RPC calls can override this through 'CallParams'.
    , connDefaultTimeout :: Maybe Timeout

      -- | Reconnection policy
      --
      -- NOTE: The default 'ReconnectPolicy' is 'DontReconnect', as per the
      -- spec (see 'ReconnectPolicy'). You may wish to override this in order
      -- to enable Wait for Ready semantics (retry connecting to a server
      -- when it is not yet ready) as well as automatic reconnects (reconnecting
      -- after a server disappears). The latter can be especially important
      -- when there are proxies, which tend to drop connections after a certain
      -- amount of time.
    , connReconnectPolicy :: ReconnectPolicy

      -- | Optionally override the content type
      --
      -- If 'Nothing', the @Content-Type@ header will be omitted entirely
      -- (this is not conform gRPC spec).
    , connContentType :: Maybe ContentType

      -- | Optionally set the initial compression algorithm
      --
      -- Under normal circumstances, the @grapesy@ client will only start using
      -- compression once the server has informed it what compression algorithms
      -- it supports. This means the first message will necessarily be
      -- uncompressed. 'connCompression' can be used to override this behaviour,
      -- but should be used with care: if the server does not support the
      -- selected compression algorithm, it will not be able to decompress any
      -- messages sent by the client to the server.
    , connInitCompression :: Maybe Compression

      -- | HTTP2 settings
    , connHTTP2Settings :: HTTP2Settings
    }

instance Default ConnParams where
  def = ConnParams {
        connCompression           = def
      , connDefaultTimeout        = Nothing
      , connReconnectPolicy       = def
      , connContentType           = Just ContentTypeDefault
      , connInitCompression       = Nothing
      , connHTTP2Settings         = def
      }

{-------------------------------------------------------------------------------
  Reconnection policy
-------------------------------------------------------------------------------}

-- | Reconnect policy
--
-- See 'exponentialBackoff' for a convenient function to construct a policy.
data ReconnectPolicy =
    -- | Do not attempt to reconnect
    --
    -- When we get disconnected from the server (or fail to establish a
    -- connection), do not attempt to connect again.
    DontReconnect

    -- | Reconnect after random delay after the IO action returns
    --
    -- This is a very general API: typically the IO action will call
    -- 'threadDelay' after some amount of time (which will typically involve
    -- some randomness), but it can be used to do things such as display a
    -- message to the user somewhere that the client is reconnecting.
  | ReconnectAfter (IO ReconnectPolicy)

-- | The default policy is 'DontReconnect'
--
-- The default follows the gRPC specification of Wait for Ready semantics
-- <https://github.com/grpc/grpc/blob/master/doc/wait-for-ready.md>.
instance Default ReconnectPolicy where
  def = DontReconnect

-- | Exponential backoff
--
-- If the exponent is @1@, the delay interval will be the same every step;
-- for an exponent of greater than @1@, we will wait longer each step.
exponentialBackoff ::
     (Int -> IO ())
     -- ^ Execute the delay (in microseconds)
     --
     -- The default choice here can simply be 'threadDelay', but it is also
     -- possible to use this to add some logging. Simple example:
     --
     -- > waitFor :: Int -> IO ()
     -- > waitFor delay = do
     -- >   putStrLn $ "Disconnected. Reconnecting after " ++ show delay ++ "μs"
     -- >   threadDelay delay
     -- >   putStrLn "Reconnecting now."
  -> Double
     -- ^ Exponent
  -> (Double, Double)
     -- ^ Initial delay
  -> Word
     -- ^ Maximum number of attempts
  -> ReconnectPolicy
exponentialBackoff waitFor e = go
  where
    go :: (Double, Double) -> Word -> ReconnectPolicy
    go _        0 = DontReconnect
    go (lo, hi) n = ReconnectAfter $ do
        delay <- randomRIO (lo, hi)
        waitFor $ round $ delay * 1_000_000
        return $ go (lo * e, hi * e) (pred n)

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
  Server address
-------------------------------------------------------------------------------}

data Server =
    -- | Make insecure connection (without TLS) to the given server
    ServerInsecure Address

    -- | Make secure connection (with TLS) to the given server
  | ServerSecure ServerValidation SslKeyLog Address
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Open a new connection
-------------------------------------------------------------------------------}

-- | Open connection to the server
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
-- on any of these calls will result in an exception being thrown. However, if
-- the 'ReconnectPolicy' allows, we will automatically try to re-establish a
-- connection to the server. This can be especially important when there is a
-- proxy between the client and the server, which may drop an existing
-- connection after a certain period.
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
    k Connection {connParams, connMetaVar, connStateVar}
      `finally` putMVar connOutOfScope ()

-- | Open new channel to the server
--
-- This is a non-blocking call; the connection will be set up in a
-- background thread; if this takes time, then the first call to
-- 'sendInput' or 'recvOutput' will block, but the call to 'startRPC'
-- itself will not block. This non-blocking nature makes this safe to use
-- in 'bracket' patterns.
startRPC :: forall rpc.
     (SupportsClientRpc rpc, HasCallStack)
  => Connection
  -> Proxy rpc
  -> CallParams rpc
  -> IO (Call rpc)
startRPC Connection{connMetaVar, connParams, connStateVar} _ callParams = do
    (connClosed, conn) <-
      atomically $ do
        connState <- readTVar connStateVar
        case connState of
          ConnectionNotReady              -> retry
          ConnectionReady connClosed conn -> return (connClosed, conn)
          ConnectionAbandoned err         -> throwSTM err
          ConnectionOutOfScope            -> error "impossible"

    cOut     <- Meta.outboundCompression <$> currentMeta
    metadata <- buildMetadataIO $ callRequestMetadata callParams
    let flowStart :: Session.FlowStart (ClientOutbound rpc)
        flowStart = Session.FlowStartRegular $ OutboundHeaders {
            outHeaders     = requestHeaders cOut metadata
          , outCompression = fromMaybe noCompression cOut
          }

    let serverClosedConnection ::
             Either TrailersOnly' ProperTrailers'
          -> SomeException
        serverClosedConnection =
              either toException toException
            . grpcClassifyTermination
            . either (fst . trailersOnlyToProperTrailers) id

    channel <-
      Session.setupRequestChannel
        callSession
        conn
        serverClosedConnection
        flowStart

    -- Spawn a thread to monitor the connection, and close the new channel when
    -- the connection is closed. To prevent a memory leak by hanging on to the
    -- channel for the lifetime of the connection, the thread also terminates in
    -- the (normal) case that the channel is closed before the connection is.
    _ <- forkLabelled "grapesy:monitorConnection" $ do
      status <- atomically $ do
          (Left <$> waitForNormalOrAbnormalThreadTermination
                      (Session.channelOutbound channel))
        `orElse`
          (Right <$> readTMVar connClosed)
      case status of
        Left _ -> return () -- Channel closed before the connection
        Right mErr -> do
          let exitReason :: ExitCase ()
              exitReason =
                case mErr of
                  Nothing -> ExitCaseSuccess ()
                  Just exitWithException ->
                    ExitCaseException . toException $
                      ServerDisconnected exitWithException callStack
          _mAlreadyClosed <- Session.close channel exitReason
          return ()

    return $ Call callSession channel
  where
    currentMeta :: IO Meta
    currentMeta = readMVar connMetaVar

    updateMeta :: ResponseHeaders' -> IO ()
    updateMeta hdrs =
        modifyMVar_ connMetaVar $ Meta.update (connCompression connParams) hdrs

    requestHeaders :: Maybe Compression -> [CustomMetadata] -> RequestHeaders
    requestHeaders cOut metadata = RequestHeaders{
          requestTimeout =
            asum [
                callTimeout callParams
              , connDefaultTimeout connParams
              ]
        , requestMetadata =
            customMetadataMapFromList metadata
        , requestCompression =
            compressionId <$> cOut
        , requestAcceptCompression = Just $
            Compression.offer $ connCompression connParams
        , requestContentType =
            connContentType connParams
        , requestMessageType =
            Just MessageTypeDefault
        , requestUserAgent = Just $
            mconcat [
                "grpc-haskell-grapesy/"
              , mconcat . intersperse "." $
                  map (BS.Strict.C8.pack . show) $
                    versionBranch Grapesy.version
              ]
        , requestIncludeTE =
            True
        , requestTraceContext =
            Nothing
        , requestPreviousRpcAttempts =
            Nothing
        , requestUnrecognized =
            ()
        }

    callSession :: ClientSession rpc
    callSession = ClientSession {
          clientCompression = connCompression connParams
        , clientUpdateMeta  = updateMeta
        }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

data ConnectionState =
    -- | We haven't set up the connection yet
    ConnectionNotReady

    -- | The connection is ready
    --
    -- The nested @TMVar@ is written to when the connection is closed.
  | ConnectionReady (TMVar (Maybe SomeException)) Session.ConnectionToServer

    -- | We gave up trying to (re)establish the connection
  | ConnectionAbandoned SomeException

    -- | The connection was closed because it is no longer needed.
  | ConnectionOutOfScope

-- | Connection attempt
--
-- This is an internal data structure used only in 'stayConnected' and helpers.
data Attempt = ConnectionAttempt {
      attemptParams     :: ConnParams
    , attemptState      :: TVar ConnectionState
    , attemptOutOfScope :: MVar ()
    , attemptClosed     :: TMVar (Maybe SomeException)
    }

newConnectionAttempt ::
     ConnParams
  -> TVar ConnectionState
  -> MVar ()
  -> IO Attempt
newConnectionAttempt attemptParams attemptState attemptOutOfScope = do
    attemptClosed <- newEmptyTMVarIO
    return ConnectionAttempt{
        attemptParams
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
stayConnected connParams server connStateVar connOutOfScope =
    loop (connReconnectPolicy connParams)
  where
    loop :: ReconnectPolicy -> IO ()
    loop remainingReconnectPolicy = do
        -- Start new attempt (this just allocates some internal state)
        attempt <- newConnectionAttempt connParams connStateVar connOutOfScope

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
          Left err -> do
            case (isFatalException err, thisReconnectPolicy) of
              (True, _) -> do
                atomically $ writeTVar connStateVar $ ConnectionAbandoned err
              (False, DontReconnect) -> do
                atomically $ writeTVar connStateVar $ ConnectionAbandoned err
              (False, ReconnectAfter f) -> do
                atomically $ writeTVar connStateVar $ ConnectionNotReady
                loop =<< f

-- | Insecure connection (no TLS)
connectInsecure :: ConnParams -> Attempt -> Address -> IO ()
connectInsecure connParams attempt addr = do
    Run.runTCPClientWithSettings
        runSettings
        (addressHost addr)
        (show $ addressPort addr) $ \sock ->
      bracket (HTTP2.Client.allocSimpleConfig sock writeBufferSize)
              HTTP2.Client.freeSimpleConfig $ \conf ->
        HTTP2.Client.run clientConfig conf $ \sendRequest _aux -> do
          let conn = Session.ConnectionToServer sendRequest
          atomically $
            writeTVar (attemptState attempt) $
              ConnectionReady (attemptClosed attempt) conn
          takeMVar $ attemptOutOfScope attempt
  where
    ConnParams{connHTTP2Settings} = connParams

    runSettings :: Run.Settings
    runSettings = Run.defaultSettings {
          Run.settingsOpenClientSocket = openClientSocket connHTTP2Settings
        }

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
    clientConfig = overridePingRateLimit connParams $
        HTTP2.Client.defaultClientConfig {
            HTTP2.Client.authority = authority addr
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
              HTTP2.TLS.Client.settingsValidateCert =
                case validation of
                  ValidateServer _   -> True
                  NoServerValidation -> False

            , HTTP2.TLS.Client.settingsCAStore          = caStore
            , HTTP2.TLS.Client.settingsKeyLogger        = keyLogger
            , HTTP2.TLS.Client.settingsAddrInfoFlags    = []

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
        clientConfig = overridePingRateLimit connParams $
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

-- | Override ping rate limit
overridePingRateLimit ::
     ConnParams
  -> HTTP2.Client.ClientConfig -> HTTP2.Client.ClientConfig
overridePingRateLimit connParams clientConfig = clientConfig {
      HTTP2.Client.settings = settings {
          HTTP2.Client.pingRateLimit =
            case http2OverridePingRateLimit (connHTTP2Settings connParams) of
              Nothing    -> HTTP2.Client.pingRateLimit settings
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
    Run.openClientSocketWithOptions socketOptions
  where
    socketOptions :: [(SocketOption, Int)]
    socketOptions = concat [
          [ (NoDelay, 1)
          | http2TcpNoDelay http2Settings
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
