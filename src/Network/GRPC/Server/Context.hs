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
import Text.Show.Pretty

import Network.GRPC.Server.Session (ServerSession)
import Network.GRPC.Spec
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
      -- | Tracer for exceptions thrown by handlers
      --
      -- The default uses 'stdoutTracer'.
      serverExceptionTracer :: Tracer IO SomeException

      -- | Tracer for debug messages
      --
      -- This is prmarily for debugging @grapesy@ itself; most client code will
      -- probably want to use 'nullTracer' here.
    , serverDebugTracer :: Tracer IO ServerDebugMsg
    }

instance Default ServerParams where
  def = ServerParams {
        serverExceptionTracer = contramap show stdoutTracer
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
