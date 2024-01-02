-- | Server context
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Server.Context (ServerContext, ServerParams)
-- > import Network.GRPC.Server.Context as Context
module Network.GRPC.Server.Context (
    -- * Context
    ServerContext -- opaque
  , new
  , params
    -- * Configuration
  , ServerParams(..)
  , ServerDebugMsg(..)
  ) where

import Control.Exception
import Control.Tracer
import Data.Default
import Text.Show.Pretty

import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server.Session (ServerSession)
import Network.GRPC.Spec
import Network.GRPC.Util.Session qualified as Session

{-------------------------------------------------------------------------------
  Context

  TODO: The server context is more of a placeholder at the moment. The plan is
  to use it to keep things like server usage statistics etc.
-------------------------------------------------------------------------------}

data ServerContext = ServerContext {
      params :: ServerParams
    }

new :: ServerParams -> IO ServerContext
new params = return ServerContext{params}

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data ServerParams = ServerParams {
      -- | Server compression preferences
      serverCompression :: Compr.Negotation

      -- | Tracer for exceptions thrown by handlers
      --
      -- The default uses 'stdoutTracer'.
    , serverExceptionTracer :: Tracer IO SomeException

      -- | Tracer for debug messages
      --
      -- This is prmarily for debugging @grapesy@ itself; most client code will
      -- probably want to use 'nullTracer' here.
    , serverDebugTracer :: Tracer IO ServerDebugMsg
    }

instance Default ServerParams where
  def = ServerParams {
        serverCompression     = def
      , serverExceptionTracer = contramap show stdoutTracer
      , serverDebugTracer     = nullTracer
      }

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data ServerDebugMsg =
    -- | New request
    ServerDebugRequest Path

    -- | Message on existing connection
  | forall rpc.
          IsRPC rpc
       => ServerDebugMsg (Session.DebugMsg (ServerSession rpc))

     -- | Could not connect to the client
  | ServerDebugAcceptFailed SomeException

deriving instance Show ServerDebugMsg

instance PrettyVal ServerDebugMsg where
  prettyVal = String . show
