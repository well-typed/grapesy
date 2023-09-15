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
  , sendFinalOutput
  , sendNextOutput
  , sendTrailers

    -- ** Low-level API
  , recvInputSTM
  , sendOutputSTM
  , initiateResponse
  , sendTrailersOnly

    -- * Common serialization formats
  , Protobuf

    -- * Debugging
  , Context.ServerDebugMsg(..)
  ) where

import Control.Exception
import Control.Tracer
import GHC.Stack
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Common
import Network.GRPC.Server.Call
import Network.GRPC.Server.Connection (Connection(..), withConnection)
import Network.GRPC.Server.Connection qualified as Connection
import Network.GRPC.Server.Context (ServerParams)
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Server.Handler (RpcHandler(..))
import Network.GRPC.Server.Handler qualified as Handler
import Network.GRPC.Spec
import Network.GRPC.Util.Session.Server qualified as Session.Server

{-------------------------------------------------------------------------------
  Server proper
-------------------------------------------------------------------------------}

-- | Construct server
--
-- The server can be run using the standard infrastructure offered by the
-- @http2@ package, but "Network.GRPC.Server.Run" provides some convenience
-- functions.
withServer :: ServerParams -> [RpcHandler IO] -> (HTTP2.Server -> IO a) -> IO a
withServer params handlers k =
    Context.withContext params $
      k . server params (Handler.constructMap handlers)

server ::
     ServerParams
  -> Handler.Map IO
  -> Context.ServerContext
  -> HTTP2.Server
server params handlers ctxt =
    withConnection ctxt $
      handleRequest params handlers

handleRequest ::
     HasCallStack
  => ServerParams
  -> Handler.Map IO
  -> Connection -> IO ()
handleRequest params handlers conn = do
    -- TODO: Proper "Apache style" logging (in addition to the debug logging)
    traceWith tracer $ Context.NewRequest path
    withHandler params handlers path conn $ \(RpcHandler h) -> do
      -- TODO: Timeouts
      --
      -- Wait-for-ready semantics makes this more complicated, maybe.
      -- See example in the grpc-repo (python/wait_for_ready).
      acceptCall params conn h
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
     ServerParams
  -> Handler.Map m
  -> Path
  -> Connection
  -> (RpcHandler m -> IO ()) -> IO ()
withHandler params handlers path conn k =
    case Handler.lookup path handlers of
      Just h  -> k h
      Nothing -> do
        traceWith (Context.serverExceptionTracer params) (toException err)
        Session.Server.respond conn' $
          HTTP2.responseNoBody
            HTTP.ok200
            (buildTrailersOnly trailers)
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
