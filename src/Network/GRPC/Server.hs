{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Server (
    -- * Server proper
    mkGrpcServer

    -- ** Configuration
  , ServerParams(..)
  , RequestHandler

    -- * Handlers
  , Call       -- opaque
  , RpcHandler -- opaque
  , Handler.mkRpcHandler

    -- * Open (ongoing) call
  , recvInput
  , sendOutput
  , sendGrpcException
  , getRequestMetadata
  , setResponseMetadata

    -- ** Protocol specific wrappers
  , sendNextOutput
  , sendFinalOutput
  , sendTrailers
  , recvNextInput
  , recvFinalInput
  , recvEndOfInput

    -- ** Low-level\/specialized API
  , initiateResponse
  , sendTrailersOnly
  , recvInputWithEnvelope
  , sendOutputWithEnvelope
  , getRequestTraceContext

    -- * Exceptions
  , CallSetupFailure(..)
  , ClientDisconnected(..)
  , HandlerTerminated(..)
  , ResponseAlreadyInitiated(..)
  ) where

import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Server.Call
import Network.GRPC.Server.Context (ServerParams(..))
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Server.Handler (RpcHandler(..))
import Network.GRPC.Server.Handler qualified as Handler
import Network.GRPC.Server.RequestHandler
import Network.GRPC.Server.Session (CallSetupFailure(..))
import Network.GRPC.Util.HTTP2.Stream (ClientDisconnected(..))

{-------------------------------------------------------------------------------
  Server proper
-------------------------------------------------------------------------------}

-- | Construct server
--
-- The server can be run using the standard infrastructure offered by the
-- @http2@ package, but "Network.GRPC.Server.Run" provides some convenience
-- functions.
mkGrpcServer :: ServerParams -> [RpcHandler IO] -> IO HTTP2.Server
mkGrpcServer params@ServerParams{serverTopLevel} handlers = do
    ctxt <- Context.new params
    return $
        requestHandlerToServer
      $ serverTopLevel
      $ requestHandler handlerMap ctxt
  where
    handlerMap :: Handler.Map IO
    handlerMap = Handler.constructMap handlers

