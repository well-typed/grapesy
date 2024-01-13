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
  , ServerDebugMsg(..)
  ) where

import Control.Exception
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
      -- The default merely logs any exceptions to stderr.
    , serverTopLevel ::
           RequestHandler (Either SomeException ())
        -> RequestHandler ()

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
    where
      defaultServerTopLevel ::
           RequestHandler (Either SomeException ())
        -> RequestHandler ()
      defaultServerTopLevel h unmask req respond = do
          res <- h unmask req respond
          case res of
            Left err -> hPrint stderr err
            Right () -> return ()

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data ServerDebugMsg =
    -- | New request
    ServerDebugRequest RawResourceHeaders

    -- | Message on existing connection
  | forall rpc.
          IsRPC rpc
       => ServerDebugMsg (Session.DebugMsg (ServerSession rpc))

     -- | Could not setup the call to the client
  | ServerDebugCallSetupFailed SomeException

deriving instance Show ServerDebugMsg

instance PrettyVal ServerDebugMsg where
  prettyVal = String . show
