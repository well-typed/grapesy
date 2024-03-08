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
  ) where

import Control.Exception
import Control.Monad.XIO (NeverThrows)
import Control.Monad.XIO qualified as XIO
import Data.Default
import System.IO

import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server.RequestHandler.API
import Network.GRPC.Spec

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
      -- Set to 'Nothing' to omit the content-type header completely
      -- (this is not conform the gRPC spec).
    , serverContentType :: Maybe ContentType
    }

instance Default ServerParams where
  def = ServerParams {
        serverCompression = def
      , serverTopLevel    = defaultServerTopLevel
      , serverContentType = Just ContentTypeDefault
      }

defaultServerTopLevel ::
     RequestHandler SomeException ()
  -> RequestHandler NeverThrows   ()
defaultServerTopLevel h req resp =
    h req resp `XIO.catchError` (XIO.swallowIO . hPrint stderr)
