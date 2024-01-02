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
  , RequestHandler
  ) where

import Control.Exception
import Control.Tracer
import Data.Default
import Network.HTTP2.Server qualified as HTTP2
import System.IO
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

-- | HTTP2 request handler
--
-- The request handler should not throw any exceptions. Most of the time we will
-- therefore work with
--
-- > RequestHandler (Either SomeException ())
--
-- It is passed a function to unmask asynchronous exceptions when it is ready.
--
-- Technical note: handlers should never throw exceptions, as doing so will
-- cause @http2@ to reset the stream, which is not always the right thing to do
-- (see detailed comments in 'acceptCall').
type RequestHandler a =
     (forall x. IO x -> IO x)
  -> HTTP2.Request
  -> (HTTP2.Response -> IO ())
  -> IO a

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
    }

instance Default ServerParams where
  def = ServerParams {
        serverCompression = def
      , serverDebugTracer = nullTracer
      , serverTopLevel    = defaultServerTopLevel
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
