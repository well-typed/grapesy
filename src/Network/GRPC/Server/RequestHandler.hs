{-# LANGUAGE OverloadedStrings #-}

-- | Request handler
--
-- This is not part of the library's public API.
--
-- Intended for unqualified import.
module Network.GRPC.Server.RequestHandler (
    -- * Definition
    RequestHandler
  , requestHandlerToServer
    -- * Construction
  , requestHandler
  ) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.XIO (XIO', XIO)
import Control.Monad.XIO qualified as XIO
import Control.Tracer
import Data.Bifunctor
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.UTF8 qualified as BS.UTF8
import Data.Maybe (fromMaybe)
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Server.Call
import Network.GRPC.Server.Context (ServerContext)
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Server.Handler (RpcHandler(..))
import Network.GRPC.Server.Handler qualified as Handler
import Network.GRPC.Server.RequestHandler.API
import Network.GRPC.Server.Session (CallSetupFailure(..))
import Network.GRPC.Spec
import Network.GRPC.Util.Session.Server

{-------------------------------------------------------------------------------
  Construct request handler
-------------------------------------------------------------------------------}

-- | Construct request handler
requestHandler ::
     Handler.Map IO
  -> ServerContext
  -> RequestHandler SomeException ()
requestHandler handlers ctxt request respond = do
    RpcHandler (handler :: Call rpc -> IO ()) <-
      findHandler tracer handlers request `XIO.catchError`
        setupFailure tracer respond
    call :: Call rpc <- do
      setupCall connectionToClient ctxt `XIO.catchError`
        setupFailure tracer respond

    -- TODO: Timeouts
    runHandler call handler
  where
    connectionToClient :: ConnectionToClient
    connectionToClient = ConnectionToClient{request, respond}

    tracer :: Tracer IO Context.ServerDebugMsg
    tracer = Context.serverDebugTracer $ Context.params ctxt

findHandler ::
     Tracer IO Context.ServerDebugMsg
  -> Handler.Map IO
  -> HTTP2.Request
  -> XIO' CallSetupFailure (RpcHandler IO)
findHandler tracer handlers req = do
    -- TODO: Proper "Apache style" logging (in addition to the debug logging)
    XIO.swallowIO $ traceWith tracer $ Context.ServerDebugRequest rawHeaders

    resourceHeaders <- liftEither . first CallSetupInvalidResourceHeaders $
      parseResourceHeaders rawHeaders
    let path = resourcePath resourceHeaders
    handler <- do
      case Handler.lookup path handlers of
        Just h  -> return h
        Nothing -> throwError $ CallSetupUnimplementedMethod path

    return handler
  where
    rawHeaders :: RawResourceHeaders
    rawHeaders = RawResourceHeaders {
          rawPath   = fromMaybe "" $ HTTP2.requestPath   req
        , rawMethod = fromMaybe "" $ HTTP2.requestMethod req
        }

-- | Call setup failure
--
-- Something went wrong during call setup. No response has been sent to the
-- client at all yet. We try to tell the client what happened, but ignore any
-- exceptions that might arise from doing so.
setupFailure ::
     Tracer IO Context.ServerDebugMsg
  -> (HTTP2.Response -> IO ())
  -> CallSetupFailure
  -> XIO a
setupFailure tracer respond failure = do
    XIO.swallowIO $ do
      traceWith tracer $ Context.ServerDebugCallSetupFailed failure
      respond $ failureResponse failure
    throwM failure

{-------------------------------------------------------------------------------
  Failures
-------------------------------------------------------------------------------}

-- | Turn setup failure into response to the client
--
-- The gRPC spec mandates that we /always/ return HTTP 200 OK, but it does not
-- explicitly say what to do when the request is malformed (not conform the
-- gRPC specification). We choose to return HTTP errors in this case.
--
-- See <https://datatracker.ietf.org/doc/html/rfc7231#section-6.5> for
-- a discussion of the HTTP error codes, specifically
--
-- * 400 Bad Request
--   <https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.1>
-- * 405 Method Not Allowed
--   <https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.5>
--   <https://datatracker.ietf.org/doc/html/rfc7231#section-7.4.1>
--
-- Testing out-of-spec errors can be bit awkward. One option is @curl@:
--
-- > curl --verbose --http2 --http2-prior-knowledge http://localhost:50051/
failureResponse :: CallSetupFailure -> HTTP2.Response
failureResponse (CallSetupInvalidResourceHeaders (InvalidMethod method)) =
    HTTP2.responseBuilder
      HTTP.methodNotAllowed405
      [("Allow", "POST")]
      (Builder.byteString . mconcat $ [
          "Unexpected :method " <> method <> ".\n"
        , "The only method supported by gRPC is POST.\n"
        ])
failureResponse (CallSetupInvalidResourceHeaders (InvalidPath path)) =
    HTTP2.responseBuilder HTTP.badRequest400 [] . Builder.byteString $
      "Invalid path " <> path
failureResponse (CallSetupInvalidRequestHeaders err) =
    HTTP2.responseBuilder HTTP.badRequest400 [] . Builder.byteString $
      "Invalid request headers: " <> BS.UTF8.fromString err
failureResponse (CallSetupUnsupportedCompression cid) =
    HTTP2.responseBuilder HTTP.badRequest400 [] . Builder.byteString $
      "Unsupported compression: " <> BS.UTF8.fromString (show cid)
failureResponse (CallSetupUnimplementedMethod path) =
    HTTP2.responseNoBody HTTP.ok200 $
      buildTrailersOnly . TrailersOnly . grpcExceptionToTrailers $
        grpcUnimplemented path

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
