-- | Connection to a server
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Client.Connection (Connection, withConnection)
-- > import Network.GRPC.Client.Connection qualified as Connection
module Network.GRPC.Client.Connection (
    -- * Definition
    Connection -- opaque
  , withConnection
  , connectionToServer
  , params
    -- * Configuration
  , ConnParams(..)
  , PeerDebugMsg(..)
    -- * Meta-information
  , currentMeta
  , updateMeta
  ) where

import Control.Concurrent
import Control.Monad.Catch
import Control.Tracer
import Data.Default
import Network.HPACK qualified as HPACK
import Network.HTTP2.Client qualified as Client
import Network.Run.TCP (runTCPClient)

import Network.GRPC.Client.Meta (Meta)
import Network.GRPC.Client.Meta qualified as Meta
import Network.GRPC.Client.Session
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Spec
import Network.GRPC.Spec.PseudoHeaders
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Session qualified as Session

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
      connTracer :: Tracer IO (SomeRPC PeerDebugMsg)

      -- | Compression negotation
    , connCompression :: Compr.Negotation

      -- | Default timeout
      --
      -- Individual RPC calls can override this through 'CallParams'.
    , connDefaultTimeout :: Maybe Timeout
    }

instance Default ConnParams where
  def = ConnParams {
        connTracer         = nullTracer
      , connCompression    = def
      , connDefaultTimeout = Nothing
      }

newtype PeerDebugMsg rpc = PeerDebugMsg (Session.DebugMsg (ClientSession rpc))

deriving instance IsRPC rpc => Show (PeerDebugMsg rpc)

{-------------------------------------------------------------------------------
  Open a new connection
-------------------------------------------------------------------------------}

withConnection ::
     ConnParams
  -> Scheme
  -> Authority
  -> (Connection -> IO a)
  -> IO a
withConnection params scheme auth k = do
    metaVar <- newMVar $ Meta.init
    runTCPClient (authorityHost auth) (show $ authorityPort auth) $ \sock ->
      bracket
          (Client.allocSimpleConfig sock bufferSize)
          Client.freeSimpleConfig $ \conf ->
        Client.run clientConfig conf $ \sendRequest ->
          k Connection{
              params
            , metaVar
            , connectionToServer = Session.ConnectionToServer sendRequest
            }
  where
    -- See docs of 'confBufferSize', but importantly: "this value is announced
    -- via SETTINGS_MAX_FRAME_SIZE to the peer."
    --
    -- Value of 4kB is taken from the example code.
    bufferSize :: HPACK.BufferSize
    bufferSize = 4096

    serverPseudoHeaders :: RawServerHeaders
    serverPseudoHeaders = buildServerHeaders $ ServerHeaders scheme auth

    clientConfig :: Client.ClientConfig
    clientConfig = Client.ClientConfig {
          scheme    = rawScheme serverPseudoHeaders
        , authority = rawAuthority serverPseudoHeaders

          -- Docs describe this as "How many pushed responses are contained in
          -- the cache". I don't think I really know what this means. Value of
          -- 20 is from the example code.
        , cacheLimit = 20
        }

{-------------------------------------------------------------------------------
  Access and update meta information
-------------------------------------------------------------------------------}

currentMeta :: Connection -> IO Meta
currentMeta Connection{metaVar} = readMVar metaVar

updateMeta :: Connection -> ResponseHeaders -> IO ()
updateMeta Connection{params, metaVar} hdrs =
    modifyMVar_ metaVar $ Meta.update (connCompression params) hdrs
