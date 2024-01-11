{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Server (
    -- * Server proper
    mkGrpcServer

    -- ** Configuration
  , ServerParams(..)
  , Context.RequestHandler

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
  , sendFinalOutput
  , sendNextOutput
  , sendTrailers

    -- ** Low-level API
  , initiateResponse
  , sendTrailersOnly
  , isCallHealthy

    -- * Common serialization formats
  , Protobuf

    -- * Exceptions
  , ClientDisconnected(..)

    -- * Debugging
  , Context.ServerDebugMsg(..)
  ) where

import Control.Exception
import Control.Tracer
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Common
import Network.GRPC.Server.Call
import Network.GRPC.Server.Connection (Connection(..))
import Network.GRPC.Server.Connection qualified as Connection
import Network.GRPC.Server.Context (ServerParams)
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Server.Handler (RpcHandler(..))
import Network.GRPC.Server.Handler qualified as Handler
import Network.GRPC.Spec
import Network.GRPC.Util.HTTP2.Stream (ClientDisconnected(..))
import Network.GRPC.Util.Session.Server qualified as Session.Server

{-------------------------------------------------------------------------------
  Server proper
-------------------------------------------------------------------------------}

-- | Construct server
--
-- The server can be run using the standard infrastructure offered by the
-- @http2@ package, but "Network.GRPC.Server.Run" provides some convenience
-- functions.
mkGrpcServer :: ServerParams -> [RpcHandler IO] -> IO HTTP2.Server
mkGrpcServer params handlers = do
    ctxt <- Context.new params
    return $ server params (Handler.constructMap handlers) ctxt

server ::
     ServerParams
  -> Handler.Map IO
  -> Context.ServerContext
  -> HTTP2.Server
server Context.ServerParams{serverTopLevel} handlers ctxt req _aux respond =
    -- We start by masking asynchronous exceptions. It is possible that
    -- http2 kills us /before/ this call to @mask@, but if it does, no harm
    -- is done: nothing has happened yet, so nothing is interrupted.
    mask $ \unmask -> requestHandler unmask req (\resp -> respond resp [])
  where
    requestHandler :: Context.RequestHandler ()
    requestHandler =
          serverTopLevel
        $ Connection.mkRequestHandler ctxt
        $ handleRequest handlers

handleRequest ::
     Handler.Map IO
  -> (forall x. IO x -> IO x)
  -> Connection
  -> IO (Either SomeException ())
handleRequest handlers unmask conn = do
    -- TODO: Proper "Apache style" logging (in addition to the debug logging)
    traceWith tracer $ Context.ServerDebugRequest path
    withHandler handlers path conn $ \(RpcHandler h) -> do
      -- TODO: Timeouts
      --
      -- Wait-for-ready semantics makes this more complicated, maybe.
      -- See example in the grpc-repo (python/wait_for_ready).
      acceptCall unmask conn h
  where
    path :: Path
    path = Connection.path conn

    tracer :: Tracer IO Context.ServerDebugMsg
    tracer =
          Context.serverDebugTracer
        $ Context.params
        $ connectionContext conn

{-------------------------------------------------------------------------------
  Get handler for the request
-------------------------------------------------------------------------------}

withHandler ::
     Handler.Map m
  -> Path
  -> Connection
  -> (RpcHandler m -> IO (Either SomeException ()))
  -> IO (Either SomeException ())
withHandler handlers path conn k =
    case Handler.lookup path handlers of
      Just h  -> k h
      Nothing -> do
        Session.Server.respond conn' $
          HTTP2.responseNoBody
            HTTP.ok200
            (buildTrailersOnly trailers)
        return $ Left (toException err)
  where
    conn' :: Session.Server.ConnectionToClient
    conn' = connectionClient conn

    err :: GrpcException
    err = grpcUnimplemented path

    trailers :: TrailersOnly
    trailers = TrailersOnly $ grpcExceptionToTrailers err

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
