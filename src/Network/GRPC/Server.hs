{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Server (
    -- * Server proper
    ServerParams(..)
  , withServer

    -- * Handlers
  , Call       -- opaque
  , RpcHandler -- opaque
  , Handler.mkRpcHandler

    -- * Open (ongoing) call
  , recvInput
  , sendOutput
  , getRequestMetadata
  , setResponseMetadata

    -- ** Protocol specific wrappers
  , recvFinalInput
  , recvNextInput
  , sendFinalOutput
  , sendNextOutput
  , sendTrailers

    -- ** Low-level API
  , ProperTrailers(..)
  , TrailersOnly(..)
  , recvInputSTM
  , sendOutputSTM
  , initiateResponse
  , sendTrailersOnly

    -- * Common serialization formats
  , Protobuf
  ) where

import Control.Exception
import Control.Tracer
import Data.Text qualified as Text
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Common.Exceptions
import Network.GRPC.Server.Call
import Network.GRPC.Server.Connection (Connection, withConnection)
import Network.GRPC.Server.Connection qualified as Connection
import Network.GRPC.Server.Context (ServerParams)
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Server.Handler (RpcHandler(..))
import Network.GRPC.Server.Handler qualified as Handler
import Network.GRPC.Spec
import Network.GRPC.Spec.PseudoHeaders
import Network.GRPC.Spec.RPC.Protobuf (Protobuf)

{-------------------------------------------------------------------------------
  Server proper
-------------------------------------------------------------------------------}

-- | Construct server
--
-- The server can be run using the standard infrastructure offered by the
-- @http2@ package, but "Network.GRPC.Server.Run" provides some convenience
-- functions.
withServer :: ServerParams -> [RpcHandler IO] -> (HTTP2.Server -> IO a) -> IO a
withServer params handlers k = do
    Context.withContext params $ \ctxt ->
      k $ withConnection ctxt $
        handleRequest (Handler.constructMap handlers)

handleRequest :: Handler.Map IO -> Connection -> IO ()
handleRequest handlers conn = do
    -- TODO: Proper "Apache style" logging (in addition to the debug logging)
    traceWith tracer $ Context.NewRequest path

    RpcHandler handler <- getHandler handlers path
    call <- acceptCall conn

    -- TODO: Timeouts
    --
    -- Wait-for-ready semantics makes this more complicated, maybe.
    -- See example in the grpc-repo (python/wait_for_ready).

    handle (forwardException call) $ handler call
  where
    path :: Path
    path = Connection.path conn

    tracer :: Tracer IO Context.ServerDebugMsg
    tracer =
          Context.serverDebugTracer
        $ Context.params
        $ Connection.context conn

-- | Forward exception to the client
--
-- If the handler throws an exception, attempt to forward it to the client so
-- that it is notified something went wrong. This is a best-effort only:
--
-- * The nature of the exception might mean that we we cannot send anything to
--   the client at all.
-- * It is possible the exception was thrown /after/ the handler already send
--   the trailers to the client.
--
-- We therefore catch and suppress all exceptions here.
forwardException :: Call rpc -> SomeException -> IO ()
forwardException call err =
    handle ignoreExceptions $
      sendTrailers call trailers
  where
    trailers :: ProperTrailers
    trailers
      | Just (err' :: GrpcException) <- fromException err
      = grpcExceptionToTrailers err'

      -- TODO: There might be a security concern here (server-side exceptions
      -- could potentially leak some sensitive data).
      | otherwise
      = ProperTrailers {
            trailerGrpcStatus  = GrpcError GrpcUnknown
          , trailerGrpcMessage = Just $ Text.pack $ show err
          , trailerMetadata    = []
          }

    -- See discussion above.
    ignoreExceptions :: SomeException -> IO ()
    ignoreExceptions _ = return ()

{-------------------------------------------------------------------------------
  Get handler for the request
-------------------------------------------------------------------------------}

getHandler :: Handler.Map m -> Path -> IO (RpcHandler m)
getHandler handlers path = do
    case Handler.lookup path handlers of
      Nothing -> throwIO $ grpcUnimplemented path
      Just h  -> return h

grpcUnimplemented :: Path -> GrpcException
grpcUnimplemented path = GrpcException {
      grpcError         = GrpcUnimplemented
    , grpcErrorMessage  = Just $ mconcat [
                                "Method "
                              , pathService path
                              , "."
                              , pathMethod path
                              , " not implemented"
                              ]
    , grpcErrorMetadata = []
    }
