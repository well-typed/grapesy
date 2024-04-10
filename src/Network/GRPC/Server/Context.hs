-- | Server context
--
-- Intended for unqualified import.
module Network.GRPC.Server.Context (
    -- * Context
    ServerContext(..)
  , newServerContext
    -- * Configuration
  , ServerParams(..)
  ) where

import Control.Exception
import Control.Monad.XIO (NeverThrows)
import Control.Monad.XIO qualified as XIO
import Data.Default
import System.IO

import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server.RequestHandler.API
import Network.GRPC.Spec
import Data.Text (Text)
import Data.Text qualified as Text

{-------------------------------------------------------------------------------
  Context

  TODO: The server context is more of a placeholder at the moment. The plan is
  to use it to keep things like server usage statistics etc.
-------------------------------------------------------------------------------}

data ServerContext = ServerContext {
      serverParams :: ServerParams
    }

newServerContext :: ServerParams -> IO ServerContext
newServerContext serverParams = do
    return ServerContext{
        serverParams
      }

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data ServerParams = ServerParams {
      -- | Server compression preferences
      serverCompression :: Compr.Negotation

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

      -- | Render handler-side exceptions for the client
      --
      -- When a handler throws an exception other than a 'GrpcException', we use
      -- this function to render that exception for the client (server-side
      -- logging is taken care of by 'serverTopLevel'). The default
      -- implementation simply calls 'displayException' on the exception, which
      -- means the full context is visible on the client, which is most useful
      -- for debugging. However, it is a potential security concern: if the
      -- exception happens to contain sensitive information, this information
      -- will also be visible on the client. You may therefore wish to override
      -- the default behaviour.
    , serverExceptionToClient :: SomeException -> IO (Maybe Text)

      -- | Override content-type for response to client.
      --
      -- Set to 'Nothing' to omit the content-type header completely
      -- (this is not conform the gRPC spec).
    , serverContentType :: Maybe ContentType
    }

instance Default ServerParams where
  def = ServerParams {
        serverCompression       = def
      , serverTopLevel          = defaultServerTopLevel
      , serverExceptionToClient = return . Just . Text.pack . displayException
      , serverContentType       = Just ContentTypeDefault
      }

defaultServerTopLevel ::
     RequestHandler SomeException ()
  -> RequestHandler NeverThrows   ()
defaultServerTopLevel h req resp =
    h req resp `XIO.catchError` (XIO.swallowIO . hPrint stderr)
