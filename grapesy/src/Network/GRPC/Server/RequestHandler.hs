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

import Control.Concurrent
import Control.Concurrent.Thread.Delay qualified as UnboundedDelays
import Control.Monad.Catch
import Data.Bifunctor
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS.Char8
import Data.ByteString.UTF8 qualified as BS.UTF8
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text qualified as Text
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Server.Call
import Network.GRPC.Server.Context (ServerContext (..))
import Network.GRPC.Server.Handler
import Network.GRPC.Server.HandlerMap (HandlerMap)
import Network.GRPC.Server.HandlerMap qualified as HandlerMap
import Network.GRPC.Server.RequestHandler.API
import Network.GRPC.Server.Session (CallSetupFailure(..))
import Network.GRPC.Spec
import Network.GRPC.Spec.Serialization
import Network.GRPC.Util.GHC
import Network.GRPC.Util.Session.Server

{-------------------------------------------------------------------------------
  Construct request handler
-------------------------------------------------------------------------------}

-- | Construct request handler
requestHandler :: HandlerMap IO -> ServerContext -> RequestHandler ()
requestHandler handlers ctxt unmask request respond = do
    labelThisThread "grapesy:requestHandler"

    SomeRpcHandler (_ :: Proxy rpc) handler <-
      findHandler handlers request      `catch` setupFailure respond
    (call :: Call rpc, mTimeout :: Maybe Timeout) <-
      setupCall connectionToClient ctxt `catch` setupFailure respond

    imposeTimeout mTimeout $
      runHandler unmask call handler
  where
    connectionToClient :: ConnectionToClient
    connectionToClient = ConnectionToClient{request, respond}

    timeoutException :: GrpcException
    timeoutException = GrpcException {
          grpcError         = GrpcDeadlineExceeded
        , grpcErrorMessage  = Nothing
        , grpcErrorMetadata = []
        }

    imposeTimeout :: Maybe Timeout -> IO () -> IO ()
    imposeTimeout Nothing  = id
    imposeTimeout (Just t) = timeoutWith timeoutException (timeoutToMicro t)

-- | Find handler (based on the path)
--
-- Throws 'CallSetupFailure' if no handler could be found.
findHandler ::
     HandlerMap IO
  -> HTTP2.Request
  -> IO (SomeRpcHandler IO)
findHandler handlers req = do
    -- TODO: <https://github.com/well-typed/grapesy/issues/131>
    -- We should do some request logging.

    resourceHeaders <-
      either throwM return . first CallSetupInvalidResourceHeaders $
        parseResourceHeaders rawHeaders
    let path = resourcePath resourceHeaders
    handler <- do
      case HandlerMap.lookup path handlers of
        Just h  -> return h
        Nothing -> throwM $ CallSetupUnimplementedMethod path

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
     (HTTP2.Response -> IO ())
  -> CallSetupFailure
  -> IO a
setupFailure respond failure = do
    _ :: Either SomeException () <- try $ respond $ failureResponse failure
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
-- > curl --verbose --http2 --http2-prior-knowledge http://127.0.0.1:50051/
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
failureResponse (CallSetupInvalidRequestHeaders invalid) =
    HTTP2.responseBuilder (statusInvalidHeaders invalid) [] $
      prettyInvalidHeaders invalid
failureResponse (CallSetupUnsupportedCompression cid) =
    HTTP2.responseBuilder HTTP.badRequest400 [] . Builder.byteString $
      "Unsupported compression: " <> BS.UTF8.fromString (show cid)
failureResponse (CallSetupUnimplementedMethod path) =
    HTTP2.responseNoBody HTTP.ok200 . buildTrailersOnly chooseContentType' $
      properTrailersToTrailersOnly (
          grpcExceptionToTrailers $ grpcUnimplemented path
        , Just ContentTypeDefault
        )
  where
    -- We cannot use the regular 'chooseContentType' here because we don't know
    -- which @rpc@ this is (given that it's an unimplemented method).
    chooseContentType' :: ContentType -> Maybe BS.UTF8.ByteString
    chooseContentType' ContentTypeDefault       = Nothing
    chooseContentType' (ContentTypeOverride ct) = Just ct

grpcUnimplemented :: Path -> GrpcException
grpcUnimplemented path = GrpcException {
      grpcError         = GrpcUnimplemented
    , grpcErrorMessage  = Just . Text.pack . BS.Char8.unpack $ mconcat [
                                "Method "
                              , pathService path
                              , "."
                              , pathMethod path
                              , " not implemented"
                              ]
    , grpcErrorMetadata = []
    }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Timeout with a specific exception
timeoutWith :: Exception e => e -> Integer -> IO a -> IO a
timeoutWith e t io = do
    me <- myThreadId

    let timer :: IO ()
        timer = do
            UnboundedDelays.delay t
            throwTo me e

    bracket (forkIO timer) killThread $ \_ -> io
