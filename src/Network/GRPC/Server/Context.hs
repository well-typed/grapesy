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

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server.RequestHandler.API
import Network.GRPC.Spec
import Data.Text (Text)
import Data.Text qualified as Text

{-------------------------------------------------------------------------------
  Context

  TODO: <https://github.com/well-typed/grapesy/issues/130>
  The server context is more of a placeholder at the moment. The plan is to use
  it to keep things like server usage statistics etc.
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

      -- | Verify that all request headers can be parsed
      --
      -- When enabled, we verify at the start of each request that all request
      -- headers are valid. By default we do /not/ do this, throwing an error
      -- only in scenarios where we really cannot continue.
    , serverVerifyHeaders :: Bool

      -- | Number of threads that will be spawned to process incoming frames
      -- on the currently active HTTP\/2 streams
      --
      -- This setting is specific to the
      -- [http2](https://hackage.haskell.org/package/http2) package's
      -- implementation of the HTTP\/2 specification for servers. Set to
      -- 'Nothing' if you don't want to override the default.
      --
      -- __Note__: If a lower 'http2ConnectionWindowSize' is desired, the
      -- number of workers should be increased to avoid a potential HTTP\/2
      -- control flow deadlock.
    , serverOverrideNumberOfWorkers :: Maybe Word

      -- | HTTP\/2 settings
    , serverHTTP2Settings :: HTTP2Settings
    }

instance Default ServerParams where
  def = ServerParams {
        serverCompression             = def
      , serverTopLevel                = defaultServerTopLevel
      , serverExceptionToClient       = defaultServerExceptionToClient
      , serverContentType             = Just ContentTypeDefault
      , serverVerifyHeaders           = False
      , serverOverrideNumberOfWorkers = Nothing
      , serverHTTP2Settings           = def
      }

defaultServerTopLevel ::
     RequestHandler SomeException ()
  -> RequestHandler NeverThrows   ()
defaultServerTopLevel h req resp =
    h req resp `XIO.catchError` (XIO.swallowIO . hPrint stderr)

-- | Default implementation for 'serverExceptionToClient'
--
-- We unwrap the 'SomeException' wrapper so that we do not include the exception
-- context in the output to the client (relevant for @ghc >= 9.10@ only).
--
-- See <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0330-exception-backtraces.rst>.
defaultServerExceptionToClient :: SomeException -> IO (Maybe Text)
defaultServerExceptionToClient (SomeException e) =
    return $ Just (Text.pack $ displayException e)