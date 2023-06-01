{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Server (
    -- * Server proper
    ServerParams(..)
  , withServer

    -- * Handlers
  , RpcHandler(..)
  , Call -- opaque
  , Handler.defRpcHandler

    -- * Ongoing calls
  , StreamElem(..)
  , CustomMetadata(..)
  , recvInput
  , sendOutput

    -- ** Protocol specific wrappers
  , recvOnlyInput
  , recvNextInput
  , sendOnlyOutput
  , sendNextOutput
  , sendTrailers
  ) where

import Control.Exception
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
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.PseudoHeaders
import Network.GRPC.Util.StreamElem

{-------------------------------------------------------------------------------
  Server proper

  TODO: Deal with https.
-------------------------------------------------------------------------------}

withServer :: ServerParams -> [RpcHandler IO] -> (HTTP2.Server -> IO a) -> IO a
withServer params handlers k = do
    Context.withContext params $ \ctxt ->
      k $ withConnection ctxt $
        handleRequest (Handler.constructMap handlers)

handleRequest :: Handler.Map IO -> Connection -> IO ()
handleRequest handlers conn = do
    -- TODO: Proper "Apache style" logging (in addition to the debug logging)
    print path

    RpcHandler{
        handlerRPC
      , handlerResponseMetadata
      , handlerRun
      } <- getHandler handlers path
    call <- acceptCall conn handlerRPC handlerResponseMetadata
    handlerRun call
  where
    path :: Path
    path = Connection.path conn

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
