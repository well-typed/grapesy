-- | Server context
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Server.Context (ServerContext, ServerParams)
-- > import Network.GRPC.Server.Context as Context
module Network.GRPC.Server.Context (
    -- * Context
    ServerContext -- opaque
  , withContext
  , params
    -- * Configuration
  , ServerParams(..)
  , PeerDebugMsg(..)
  ) where

import Control.Tracer
import Data.Default

import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Common.Peer qualified as Peer
import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  Context

  TODO:

  - Compression
  - Server stats
-------------------------------------------------------------------------------}

data ServerContext = ServerContext {
      params :: ServerParams
    }

withContext :: ServerParams -> (ServerContext -> IO a) -> IO a
withContext params k = k $ ServerContext{params}

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data ServerParams = ServerParams {
      serverTracer      :: Tracer IO (SomeRPC PeerDebugMsg)
    , serverCompression :: Compression.Negotation
    }

instance Default ServerParams where
  def = ServerParams {
        serverTracer      = nullTracer
      , serverCompression = def
      }

newtype PeerDebugMsg rpc = PeerDebugMsg (Peer.DebugMsg (Output rpc) (Input rpc))

deriving instance IsRPC rpc => Show (PeerDebugMsg rpc)

