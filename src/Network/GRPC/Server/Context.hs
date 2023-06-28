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
  , ServerDebugMsg(..)
  ) where

import Control.Exception
import Control.Tracer
import Data.Default

import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server.Session (ServerSession)
import Network.GRPC.Spec.PseudoHeaders
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Session qualified as Session

{-------------------------------------------------------------------------------
  Context

  TODO: Stats
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
      serverDebugTracer :: Tracer IO ServerDebugMsg
    , serverCompression :: Compr.Negotation
    }

instance Default ServerParams where
  def = ServerParams {
        serverDebugTracer = nullTracer
      , serverCompression = def
      }

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data ServerDebugMsg =
    NewRequest Path

  | forall rpc.
          IsRPC rpc
       => PeerDebugMsg (Session.DebugMsg (ServerSession rpc))

   | AcceptCallFailed SomeException

deriving instance Show ServerDebugMsg

