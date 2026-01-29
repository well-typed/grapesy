{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Connection to a server
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Client.Connection (Connection, withConnection)
-- > import Network.GRPC.Client.Connection qualified as Connection
module Network.GRPC.Client.Connection (
    -- * Definition
    Connection(..)
  , ConnectionState(..)
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
  , getConnectionToServer
  , getOutboundCompression
  , updateConnectionMeta
  ) where

import Network.GRPC.Util.Imports

import Control.Concurrent.MVar (MVar, readMVar, modifyMVar_)
import Control.Concurrent.STM (atomically, retry, throwSTM)
import Control.Concurrent.STM.TVar (TVar, readTVar)
import Control.Concurrent.STM.TMVar (TMVar)
import System.Random (randomRIO)

import Network.GRPC.Client.Meta (Meta)
import Network.GRPC.Client.Meta qualified as Meta
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.HTTP2Settings
import Network.GRPC.Util.Session.Client qualified as Session
import Network.GRPC.Util.TLS (ServerValidation(..), SslKeyLog(..))

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
-- See 'withConnection'.
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

      -- | Flag for garbage collection.
    , connOutOfScope :: MVar ()
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

      -- | Action to run upon successful connection
    , connOnConnection :: OnConnection

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

      -- | Should we verify all request headers?
      --
      -- This is the client analogue of
      -- 'Network.GRPC.Server.Context.serverVerifyHeaders'; see detailed
      -- discussion there.
      --
      -- Arguably, it is less essential to verify headers on the client: a
      -- server must deal with all kinds of different clients, and might want to
      -- know if any of those clients has expectations that it cannot fulfill. A
      -- client however connects to a known server, and knows what information
      -- it wants from the server.
    , connVerifyHeaders :: Bool

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
        connCompression     = def
      , connDefaultTimeout  = Nothing
      , connOnConnection    = def
      , connReconnectPolicy = def
      , connContentType     = Just ContentTypeDefault
      , connVerifyHeaders   = False
      , connInitCompression = Nothing
      , connHTTP2Settings   = def
      }

{-------------------------------------------------------------------------------
  Reconnection policy
-------------------------------------------------------------------------------}

-- | Reconnect policy
--
-- See 'exponentialBackoff' for a convenient function to construct a policy.
--
-- When we get disconnected from the server, we will 'runReconnectPolicy'
-- to decide what to do next. This can run arbitrary IO actions; two example
-- use cases are
--
-- * wait until we reconnect (run 'threadDelay')
-- * update some application-specific state to indicate that we are not
--   currently connected to the server (see also 'onReconnect')
newtype ReconnectPolicy = ReconnectPolicy{
      runReconnectPolicy :: IO ReconnectDecision
    }

-- | Decision on whether to reconnect or not
--
-- See 'ReconnectPolicy'.
data ReconnectDecision =
    -- | Do not attempt to reconnect
    --
    -- When we get disconnected from the server (or fail to establish a
    -- connection), do not attempt to connect again.
    DontReconnect

    -- | Reconnect (optionally to a different server)
  | DoReconnect Reconnect

-- | Decision made by a 'ReconnectPolicy'
data Reconnect = Reconnect {
      -- | Server to reconnect to
      --
      -- This can be used to implement a rudimentary redundancy scheme. For
      -- example, you could decide to reconnect to a known fallback server after
      -- connection to a main server fails a certain number of times.
      reconnectTo :: ReconnectTo

      -- | Action to run on successful reconnect
      --
      -- If it is 'Nothing', the 'connOnReconnect' given in the initial
      -- 'ConnParams' will be used instead.
    , onReconnect :: Maybe OnConnection

      -- | New policy
      --
      -- If the /next/ connection attempt fails at any point, use this as the
      -- next 'ReconnectPolicy'
    , nextPolicy :: ReconnectPolicy
    }

-- | An action to run upon successful (re)connection to a server
--
-- This can be used to, for example, display a message to the user that the
-- connection has been (re)established .
newtype OnConnection = OnConnection { runOnConnection :: IO () }

-- | What server should we attempt to reconnect to?
--
-- * 'ReconnectToPrevious' will attempt to reconnect to the last server we
--   attempted to connect to, whether or not that attempt was successful.
-- * 'ReconnectToOriginal' will attempt to reconnect to the original server that
--   'withConnection' was given.
-- * 'ReconnectToNew' will attempt to connect to the newly specified server.
data ReconnectTo =
      ReconnectToPrevious
    | ReconnectToOriginal
    | ReconnectToNew Server

-- | The default policy is 'DontReconnect'
--
-- The default follows the gRPC specification of Wait for Ready semantics
-- <https://github.com/grpc/grpc/blob/master/doc/wait-for-ready.md>.
instance Default ReconnectPolicy where
  def = ReconnectPolicy $ pure DontReconnect

-- | The default 'OnConnection' is to do nothing
instance Default OnConnection where
  def = OnConnection $ pure ()

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
    go _        0 = ReconnectPolicy $ pure DontReconnect
    go (lo, hi) n = ReconnectPolicy $ do
        delay <- randomRIO (lo, hi)
        waitFor $ round $ delay * 1_000_000
        return $ DoReconnect Reconnect {
              reconnectTo = ReconnectToOriginal
            , onReconnect = Nothing
            , nextPolicy  = go (lo * e, hi * e) (pred n)
            }

{-------------------------------------------------------------------------------
  Server address
-------------------------------------------------------------------------------}

data Server =
    -- | Make insecure connection (without TLS) to the given server
    ServerInsecure Address

    -- | Make secure connection (with TLS) to the given server
  | ServerSecure ServerValidation SslKeyLog Address

    -- | Make a local connection over a Unix domain socket
  | ServerUnix FilePath
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Making use of the connection
-------------------------------------------------------------------------------}

-- | Get connection to the server
--
-- Returns two things: the connection to the server, as well as a @TMVar@ that
-- should be monitored to see if that connection is still live.
getConnectionToServer :: forall.
     HasCallStack
  => Connection
  -> IO (TMVar (Maybe SomeException), Session.ConnectionToServer)
getConnectionToServer Connection{connStateVar} = atomically $ do
    connState <- readTVar connStateVar
    case connState of
      ConnectionNotReady              -> retry
      ConnectionReady connClosed conn -> return (connClosed, conn)
      ConnectionAbandoned err         -> throwSTM err
      ConnectionOutOfScope            -> error "impossible"

-- | Get outbound compression algorithm
--
-- This is stateful, because it depends on whether or not compression negotation
-- has happened yet: before the remote peer has told us which compression
-- algorithms it can support, we must use no compression.
getOutboundCompression :: Connection -> IO (Maybe Compression)
getOutboundCompression Connection{connMetaVar} =
    Meta.outboundCompression <$> readMVar connMetaVar

-- | Update connection metadata
--
-- Amongst other things, this updates the compression algorithm to be used
-- (see also 'getOutboundCompression').
updateConnectionMeta ::
     Connection
  -> ResponseHeaders' HandledSynthesized
  -> IO ()
updateConnectionMeta Connection{connMetaVar, connParams} hdrs =
    modifyMVar_ connMetaVar $ Meta.update (connCompression connParams) hdrs

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
