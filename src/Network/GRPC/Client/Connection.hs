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
  , ClientDebugMsg(..)
  , exponentialBackoff
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Tracer
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8
import Data.Default
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Proxy
import GHC.Stack
import Network.HPACK qualified as HPACK
import Network.HTTP2.Client qualified as HTTP2.Client
import Network.HTTP2.TLS.Client qualified as HTTP2.TLS.Client
import Network.Run.TCP qualified as Run
import Network.Socket
import System.Random
import Text.Show.Pretty

import Network.GRPC.Client.Meta (Meta)
import Network.GRPC.Client.Meta qualified as Meta
import Network.GRPC.Client.Session
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Spec
import Network.GRPC.Util.Concurrency
import Network.GRPC.Util.HTTP2.Stream (ServerDisconnected(..))
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Util.TLS (ServerValidation(..), SslKeyLog(..))
import Network.GRPC.Util.TLS qualified as Util.TLS

{-------------------------------------------------------------------------------
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
    , metaVar :: MVar Meta

      -- | Open new channel to the server
      --
      -- This is a non-blocking call; the connection will be set up in a
      -- background thread; if this takes time, then the first call to
      -- 'sendInput' or 'recvOutput' will block, but the call to 'startRPC'
      -- itself will not block. This non-blocking nature makes this safe to use
      -- in 'bracket' patterns.
    , startRPC :: forall rpc.
           IsRPC rpc
        => Proxy rpc
        -> CallParams
        -> IO (Call rpc)
    }

-- | State of the call
--
-- This type is kept abstract (opaque) in the public facing API.
data Call rpc = IsRPC rpc => Call {
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
      -- | Logging
      --
      -- Most applications will probably just set this to 'nullTracer' to
      -- disable logging.
      connDebugTracer :: Tracer IO ClientDebugMsg

      -- | Compression negotation
    , connCompression :: Compr.Negotation

      -- | Default timeout
      --
      -- Individual RPC calls can override this through 'CallParams'.
    , connDefaultTimeout :: Maybe Timeout

      -- | Reconnection policy
      --
      -- NOTE: The default 'ReconnectPolicy' is 'DontReconnect', as per the
      -- spec (see 'ReconnectPolicy'). You may wish to override this in order
      -- to enable Wait for Ready semantics (retry connecting to a server
      -- when it is not yet ready) as well as transparent retries (reconnecting
      -- after a server disappears). The latter can be especially important
      -- when there are proxies, which tend to drop connections after a certain
      -- amount of time.
    , connReconnectPolicy :: ReconnectPolicy
    }

instance Default ConnParams where
  def = ConnParams {
        connDebugTracer     = nullTracer
      , connCompression     = def
      , connDefaultTimeout  = Nothing
      , connReconnectPolicy = def
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

    -- | Reconnect after random delay within specified interval (in seconds)
  | ReconnectAfter (Double, Double) ReconnectPolicy
  deriving (Show)

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
     Double           -- ^ Exponent
  -> (Double, Double) -- ^ Initial delay
  -> Word             -- ^ Maximum number of attempts
  -> ReconnectPolicy
exponentialBackoff e = go
  where
    go :: (Double, Double) -> Word -> ReconnectPolicy
    go _        0 = DontReconnect
    go (lo, hi) n = ReconnectAfter (lo, hi) $ go (lo * e, hi * e) (pred n)

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data ClientDebugMsg =
    -- | Connecting to the server
    ClientDebugConnecting

    -- | Connected to the server (without TLS)
  | ClientDebugConnectedInsecure

    -- | Connected to the server (with TLS)
  | ClientDebugConnectedSecure

    -- | Message on an established connection
  | forall rpc.
         IsRPC rpc
      => ClientDebugMsg (Session.DebugMsg (ClientSession rpc))

    -- | Connection was closed normally
  | ClientDebugConnectionClosed

    -- | We got disconnected and are waiting to reconnect after specified delay
  | ClientDebugReconnectingAfter SomeException Double

    -- | We got disconnected and will not reconnect again
  | ClientDebugDisconnected SomeException

deriving instance Show ClientDebugMsg

instance PrettyVal ClientDebugMsg where
  prettyVal = String . show

{-------------------------------------------------------------------------------
  Server address
-------------------------------------------------------------------------------}

data Server =
    -- | Make insecure connection (without TLS) to the given server, with an
    -- optional http2 authority pseudo-header.
    ServerInsecure Authority (Maybe String)
    -- | Make secure connection (with TLS) to the given server
  | ServerSecure ServerValidation SslKeyLog Authority
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
-- wait the time specified by the policy and try again. This implements the
-- gRPC "Wait for ready" semantics.
--
-- If the connection to the server is lost /after/ it has een established, any
-- currently ongoing RPC calls will be closed; attempts at further communication
-- on any of these calls will result in an exception being thrown. However, if
-- the 'ReconnectPolicy' allows, we will automatically try to re-establish a
-- connection to the server (this is sometimes known as transparent retries).
-- This can be especially important when there is a proxy between the client and
-- the server, which may drop an existing connection after a certain period.
--
-- NOTE: The /default/ 'ReconnectPolicy' is 'DontReconnect', as per the gRPC
-- specification of "Wait for ready" semantics. You may wish to override this
-- default.
withConnection ::
     HasCallStack
  => ConnParams
  -> Server
  -> (Connection -> IO a)
  -> IO a
withConnection connParams server k = do
    metaVar <- newMVar $ Meta.init
    connVar <- newTVarIO ConnectionNotReady

    let currentMeta :: IO Meta
        currentMeta = readMVar metaVar

        updateMeta :: ResponseHeaders -> IO ()
        updateMeta hdrs =
            modifyMVar_ metaVar $ Meta.update (connCompression connParams) hdrs

    let startRPC :: forall rpc.
             (IsRPC rpc, HasCallStack)
          => Proxy rpc
          -> CallParams
          -> IO (Call rpc)
        startRPC _proxy callParams = do
            (connClosed, conn) <-
              atomically $ do
                connState <- readTVar connVar
                case connState of
                  ConnectionNotReady               -> retry
                  ConnectionReady connClosed conn -> return (connClosed, conn)
                  ConnectionAbandoned err          -> throwSTM err
                  ConnectionClosed                 -> error "impossible"

            cOut <- Meta.outboundCompression <$> currentMeta
            let flowStart :: Session.FlowStart (ClientOutbound rpc)
                flowStart = Session.FlowStartRegular $ OutboundHeaders {
                    outHeaders     = requestHeaders cOut
                  , outCompression = fromMaybe noCompression cOut
                  }

            channel <-
              Session.setupRequestChannel
                callSession
                tracer
                conn
                flowStart

            _ <- forkIO $ do
              mErr <- atomically $ readTMVar connClosed
              let exitReason :: ExitCase ()
                  exitReason =
                    case mErr of
                      Nothing -> ExitCaseSuccess ()
                      Just exitWithException ->
                        ExitCaseException . toException $
                          ServerDisconnected exitWithException
              _mAlreadyClosed <- Session.close channel exitReason
              return ()

            return $ Call callSession channel
          where
            requestHeaders :: Maybe Compression -> RequestHeaders
            requestHeaders cOut = RequestHeaders{
                  requestTimeout =
                    asum [
                        callTimeout callParams
                      , connDefaultTimeout connParams
                      ]
                , requestMetadata =
                    callRequestMetadata callParams
                , requestCompression =
                    compressionId <$> cOut
                , requestAcceptCompression = Just $
                    Compression.offer $ connCompression connParams
                }

            callSession :: ClientSession rpc
            callSession = ClientSession {
                  clientCompression = connCompression connParams
                , clientUpdateMeta  = updateMeta
                }

            tracer :: Tracer IO (Session.DebugMsg (ClientSession rpc))
            tracer = contramap (ClientDebugMsg @rpc) $
                       connDebugTracer connParams

    connCanClose <- newEmptyMVar
    let stayConnectedThread :: IO ()
        stayConnectedThread =
            stayConnected connParams server connVar connCanClose

    -- We don't use withAsync because we want the thread to terminate cleanly
    -- when we no longer need the connection (which we indicate by writing to
    -- connCanClose).
    void $ forkIO $ stayConnectedThread
    k Connection {connParams, metaVar, startRPC}
      `finally` putMVar connCanClose ()

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

data ConnectionState =
    -- | We haven't set up the connection yet
    ConnectionNotReady

    -- | The connection is ready
    --
    -- The nested @TVar@ is written to when the connection is closed.
  | ConnectionReady (TMVar (Maybe SomeException)) Session.ConnectionToServer

    -- | We gave up trying to (re)establish the connection
  | ConnectionAbandoned SomeException

    -- | The connection was closed because it is no longer needed.
  | ConnectionClosed

-- | Stay connected to the server
stayConnected ::
     ConnParams
  -> Server
  -> TVar ConnectionState
  -> MVar ()
  -> IO ()
stayConnected connParams server connVar connCanClose =
    loop (connReconnectPolicy connParams)
  where
    loop :: ReconnectPolicy -> IO ()
    loop remainingReconnectPolicy = do
        traceWith tracer $ ClientDebugConnecting
        connClosed <- newEmptyTMVarIO

        -- Just like in 'runHandler' on the server side, it is important that we
        -- call @run@ (from @http2@ or @http2-tls@) in a separate thread. If we
        -- do not, then the moment we disconnect @http2[-tls]@ will throw and we
        -- will not get the chance to process any other messages. This is
        -- especially important when we fail to setup a call: the server will
        -- respond with an informative gRPC error message (which we will raise
        -- as a 'GrpcException' in the client), and then disconnect. If we do
        -- not call @run@ in a separate thread, the only exception we will see
        -- is the low-level exception reported by @http2@ (something about
        -- stream errors), rather than the informative gRPC exception we want.

        mRes <- try $
          case server of
            ServerInsecure dest mAuth -> runTCPClient dest $ \sock ->
              bracket (HTTP2.Client.allocSimpleConfig sock writeBufferSize)
                      HTTP2.Client.freeSimpleConfig $ \conf ->
                HTTP2.Client.run
                      (clientConfig dest mAuth Http)
                      conf
                    $ \sendRequest -> do
                  traceWith tracer $ ClientDebugConnectedInsecure
                  let conn = Session.ConnectionToServer sendRequest
                  atomically $ writeTVar connVar $ ConnectionReady connClosed conn
                  takeMVar connCanClose
            ServerSecure validation sslKeyLog auth -> do
              keyLogger <- Util.TLS.keyLogger sslKeyLog
              caStore   <- Util.TLS.validationCAStore validation
              let settings :: HTTP2.TLS.Client.Settings
                  settings = HTTP2.TLS.Client.defaultSettings {
                        HTTP2.TLS.Client.settingsValidateCert =
                          case validation of
                            ValidateServer _   -> True
                            NoServerValidation -> False
                      , HTTP2.TLS.Client.settingsCAStore       = caStore
                      , HTTP2.TLS.Client.settingsKeyLogger     = keyLogger
                      , HTTP2.TLS.Client.settingsAddrInfoFlags = []
                      }

              HTTP2.TLS.Client.run
                    settings
                    (authorityHost auth)
                    (fromIntegral $ authorityPort auth)
                  $ \sendRequest -> do
                traceWith tracer $ ClientDebugConnectedSecure
                let conn = Session.ConnectionToServer sendRequest
                atomically $ writeTVar connVar $ ConnectionReady connClosed conn
                takeMVar connCanClose

        thisReconnectPolicy <- atomically $ do
          putTMVar connClosed $ either Just (\() -> Nothing) mRes
          connState <- readTVar connVar
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
            traceWith tracer $ ClientDebugConnectionClosed
            atomically $ writeTVar connVar $ ConnectionClosed
          Left err -> do
            case thisReconnectPolicy of
              DontReconnect -> do
                traceWith tracer $ ClientDebugDisconnected err
                atomically $ writeTVar connVar $ ConnectionAbandoned err
              ReconnectAfter (lo, hi) reconnectPolicy' -> do
                delay <- randomRIO (lo, hi)
                traceWith tracer $ ClientDebugReconnectingAfter err delay
                atomically $ writeTVar connVar $ ConnectionNotReady
                threadDelay $ round $ delay * 1_000_000
                loop reconnectPolicy'

    runTCPClient :: Authority -> (Socket -> IO a) -> IO a
    runTCPClient auth =
        Run.runTCPClient (authorityHost auth) (show $ authorityPort auth)

    -- See docs of 'confBufferSize', but importantly: "this value is announced
    -- via SETTINGS_MAX_FRAME_SIZE to the peer."
    --
    -- Value of 4kB is taken from the example code.
    writeBufferSize :: HPACK.BufferSize
    writeBufferSize = 4096

    -- TODO: This is currently only used for the HTTP case, not HTTPS
    -- | dest is only used if mAuth is Nothing
    clientConfig :: Authority -> Maybe String -> Scheme -> HTTP2.Client.ClientConfig
    clientConfig dest mAuth scheme = HTTP2.Client.ClientConfig {
          scheme    = rawScheme serverPseudoHeaders
        , authority = maybe (rawAuthority serverPseudoHeaders) BS.Strict.UTF8.fromString mAuth

          -- Docs describe this as "How many pushed responses are contained in
          -- the cache". Since gRPC does not make use of HTTP2 server push, we
          -- set it to 0.
        , cacheLimit = 0
        }
      where
        serverPseudoHeaders :: RawServerHeaders
        serverPseudoHeaders = buildServerHeaders $ ServerHeaders scheme dest

    tracer :: Tracer IO ClientDebugMsg
    tracer = connDebugTracer connParams
