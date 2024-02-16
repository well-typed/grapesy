-- | Server context
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Server.Context (ServerContext, ServerParams)
-- > import Network.GRPC.Server.Context as Context
module Network.GRPC.Server.Context (
    -- * Context
    ServerContext(..)
  , new
    -- * Configuration
  , ServerParams(..)
    -- * Logging and failures
  , ServerDebugMsg(..)
  , CallSetupFailure(..)
  ) where

import Control.Exception
import Control.Monad.XIO (NeverThrows)
import Control.Monad.XIO qualified as XIO
import Control.Tracer
import Data.ByteString qualified as Strict (ByteString)
import Data.Default
import System.IO
import Text.Show.Pretty

import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server.RequestHandler.API
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

      -- | Tracer for debug messages
      --
      -- This is prmarily for debugging @grapesy@ itself; most client code will
      -- probably want to use 'nullTracer' here.
    , serverDebugTracer :: Tracer IO ServerDebugMsg

      -- | Top-level hook for request handlers
      --
      -- The most important responsibility of this function is to deal with
      -- any exceptions that the handler might throw, but in principle it has
      -- full control over how requests are handled.
      --
      -- The default merely logs any exceptions to 'stderr'.
    , serverTopLevel ::
           RequestHandler SomeException ()
        -> RequestHandler NeverThrows   ()

      -- | Override content-type for response to client.
      --
      -- If not defined, the default @application/grpc+format@ is used.
    , serverContentType :: Maybe Strict.ByteString
    }

instance Default ServerParams where
  def = ServerParams {
        serverCompression = def
      , serverDebugTracer = nullTracer
      , serverTopLevel    = defaultServerTopLevel
      , serverContentType = Nothing
      }

defaultServerTopLevel ::
     RequestHandler SomeException ()
  -> RequestHandler NeverThrows   ()
defaultServerTopLevel h req resp =
    h req resp `XIO.catchError` (XIO.swallowIO . hPrint stderr)

{-------------------------------------------------------------------------------
  Logging and failures
-------------------------------------------------------------------------------}

data ServerDebugMsg =
    -- | New request
    ServerDebugRequest RawResourceHeaders

    -- | Message on existing connection
  | forall rpc.
          IsRPC rpc
       => ServerDebugMsg (Session.DebugMsg (ServerSession rpc))

     -- | Could not setup the call to the client
  | ServerDebugCallSetupFailed CallSetupFailure

deriving instance Show ServerDebugMsg

instance PrettyVal ServerDebugMsg where
  prettyVal = String . show

-- | We failed to setup the call from the client
data CallSetupFailure =
    -- | Client sent resource headers that were not conform the gRPC spec
    CallSetupInvalidResourceHeaders InvalidResourceHeaders

    -- | No registered handler for the specified path
    --
    -- Note on terminology: HTTP has \"methods\" such as POST, GET, etc; gRPC
    -- supports only POST, and when another HTTP method is chosen, this will
    -- result in 'CallSetupInvalidResourceHeaders'. However, gRPC itself also
    -- has the concept of a "method" (a method, or gRPC call, supported by a
    -- particular service); it's these methods that
    -- 'CallSetupUnimplementedMethod' is referring to.
  | CallSetupUnimplementedMethod Path

    -- | Some other exception happened
  | CallSetupFailed SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

