-- | Connection to a server
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Client.Connection (Connection, withConnection)
-- > import Network.GRPC.Client.Connection qualified as Connection
module Network.GRPC.Client.Connection (
    -- * Definition
    Connection -- opaque
  , Server(..)
  , ServerValidation(..)
  , SslKeyLog(..)
  , withConnection
  , connectionToServer
  , params
    -- * Configuration
  , ConnParams(..)
  , ClientDebugMsg(..)
    -- * Meta-information
  , currentMeta
  , updateMeta
  ) where

import Control.Concurrent
import Control.Monad.Catch
import Control.Tracer
import Data.Default
import Network.HPACK qualified as HPACK
import Network.HTTP2.Client qualified as HTTP2.Client
import Network.HTTP2.TLS.Client qualified as HTTP2.TLS.Client
import Network.Run.TCP qualified as Run
import Network.Socket
import Text.Show.Pretty

import Network.GRPC.Client.Meta (Meta)
import Network.GRPC.Client.Meta qualified as Meta
import Network.GRPC.Client.Session
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Spec
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
--
-- TODO: Discuss and implement auto reconnect.
-- TODO: (Related:) wait-for-ready semantics
--       See <https://github.com/grpc/grpc/blob/master/doc/wait-for-ready.md>
--       as well as <https://github.com/grpc/grpc/blob/master/examples/python/wait_for_ready/README.md>
data Connection = Connection {
      -- | Configuration
      params :: ConnParams

      -- | Information about the open connection
    , metaVar :: MVar Meta

      -- | The actual connection to the server (from @http2@)
    , connectionToServer :: Session.ConnectionToServer
    }

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

-- | Connection configuration
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
    }

instance Default ConnParams where
  def = ConnParams {
        connDebugTracer    = nullTracer
      , connCompression    = def
      , connDefaultTimeout = Nothing
      }

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data ClientDebugMsg =
    forall rpc.
         IsRPC rpc
      => PeerDebugMsg (Session.DebugMsg (ClientSession rpc))

deriving instance Show ClientDebugMsg

instance PrettyVal ClientDebugMsg where
  prettyVal = String . show

{-------------------------------------------------------------------------------
  Server address
-------------------------------------------------------------------------------}

data Server =
    -- | Make insecure connection (without TLS) to the given server
    ServerInsecure Authority

    -- | Make secure connection (with TLS) to the given server
  | ServerSecure ServerValidation SslKeyLog Authority
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Open a new connection
-------------------------------------------------------------------------------}

withConnection :: ConnParams -> Server -> (Connection -> IO a) -> IO a
withConnection params server k = do
    metaVar <- newMVar $ Meta.init
    case server of
      ServerInsecure auth ->
        runTCPClient auth $ \sock ->
          bracket (HTTP2.Client.allocSimpleConfig sock writeBufferSize)
                  HTTP2.Client.freeSimpleConfig $ \conf ->
            HTTP2.Client.run (clientConfig auth Http) conf $ \sendRequest ->
              k Connection{
                  params
                , metaVar
                , connectionToServer = Session.ConnectionToServer sendRequest
                }
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
          (\sendRequest -> k Connection{
                params
              , metaVar
              , connectionToServer = Session.ConnectionToServer sendRequest
              }
          )
  where
    -- See docs of 'confBufferSize', but importantly: "this value is announced
    -- via SETTINGS_MAX_FRAME_SIZE to the peer."
    --
    -- Value of 4kB is taken from the example code.
    writeBufferSize :: HPACK.BufferSize
    writeBufferSize = 4096

    -- TODO: This is currently only used for the HTTP case, not HTTPS
    clientConfig :: Authority -> Scheme -> HTTP2.Client.ClientConfig
    clientConfig auth scheme = HTTP2.Client.ClientConfig {
          scheme    = rawScheme serverPseudoHeaders
        , authority = rawAuthority serverPseudoHeaders

          -- Docs describe this as "How many pushed responses are contained in
          -- the cache". Since gRPC does not make use of HTTP2 server push, we
          -- set it to 0.
        , cacheLimit = 0
        }
      where
        serverPseudoHeaders :: RawServerHeaders
        serverPseudoHeaders = buildServerHeaders $ ServerHeaders scheme auth

    runTCPClient :: Authority -> (Socket -> IO a) -> IO a
    runTCPClient auth =
        Run.runTCPClient (authorityHost auth) (show $ authorityPort auth)

{-------------------------------------------------------------------------------
  Access and update meta information
-------------------------------------------------------------------------------}

currentMeta :: Connection -> IO Meta
currentMeta Connection{metaVar} = readMVar metaVar

updateMeta :: Connection -> ResponseHeaders -> IO ()
updateMeta Connection{params, metaVar} hdrs =
    modifyMVar_ metaVar $ Meta.update (connCompression params) hdrs
