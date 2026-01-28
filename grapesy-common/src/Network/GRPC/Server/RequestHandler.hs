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

import Network.GRPC.Util.Imports

import Control.Concurrent (forkIO, throwTo, killThread, myThreadId)
import Control.Concurrent.Thread.Delay qualified as UnboundedDelays

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS.Char8
import Data.ByteString.UTF8 qualified as BS.UTF8
import Data.Text qualified as Text
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Semantics.Server qualified as Server

import Network.GRPC.Server.Call
import Network.GRPC.Server.Context (ServerContext (..), ServerParams(..))
import Network.GRPC.Server.Handler
import Network.GRPC.Server.HandlerMap (HandlerMap)
import Network.GRPC.Server.HandlerMap qualified as HandlerMap
import Network.GRPC.Server.RequestHandler.API
import Network.GRPC.Server.Session (CallSetupFailure(..))

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
      findHandler handlers request      `catch` setupFailure params respond
    (call :: Call rpc, mTimeout :: Maybe Timeout) <-
      setupCall connectionToClient ctxt `catch` setupFailure params respond

    imposeTimeout mTimeout $
      runHandler unmask call handler
  where
    ServerContext{serverParams = params} = ctxt

    connectionToClient :: ConnectionToClient
    connectionToClient = ConnectionToClient{request, respond}

    timeoutException :: GrpcException
    timeoutException = GrpcException {
          grpcError         = GrpcDeadlineExceeded
        , grpcErrorMessage  = Nothing
        , grpcErrorDetails  = Nothing
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
  -> Server.Request
  -> IO (SomeRpcHandler IO)
findHandler handlers req = do
    -- TODO: <https://github.com/well-typed/grapesy/issues/131>
    -- We should do some request logging.

    resourceHeaders <-
      either throwIO return . first CallSetupInvalidResourceHeaders $
        parseResourceHeaders rawHeaders
    let path = resourcePath resourceHeaders

    -- We have to be careful looking up the handler; there might be pure
    -- exceptions in the list of handlers (most commonly @undefined@).
    mHandler <- try $ evaluate $ HandlerMap.lookup path handlers
    case mHandler of
      Right (Just h) -> return h
      Right Nothing  -> throwIO $ CallSetupUnimplementedMethod path
      Left err       -> throwIO $ CallSetupHandlerLookupException err
  where
    rawHeaders :: RawResourceHeaders
    rawHeaders = RawResourceHeaders {
          rawPath   = fromMaybe "" $ Server.requestPath   req
        , rawMethod = fromMaybe "" $ Server.requestMethod req
        }

-- | Call setup failure
--
-- Something went wrong during call setup. No response has been sent to the
-- client at all yet. We try to tell the client what happened, but ignore any
-- exceptions that might arise from doing so.
setupFailure ::
     ServerParams
  -> (Server.Response -> IO ())
  -> CallSetupFailure
  -> IO a
setupFailure params sendResponse failure = do
    response <- mkFailureResponse params failure
    _ :: Either SomeException () <- try $ sendResponse response
    throwIO failure

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
mkFailureResponse :: ServerParams -> CallSetupFailure -> IO Server.Response
mkFailureResponse params = \case
    CallSetupInvalidResourceHeaders (InvalidMethod method) ->
      return $
        Server.responseBuilder
          HTTP.methodNotAllowed405
          [("Allow", "POST")]
          (Builder.byteString . mconcat $ [
              "Unexpected :method " <> method <> ".\n"
            , "The only method supported by gRPC is POST.\n"
            ])
    CallSetupInvalidResourceHeaders (InvalidPath path) ->
      return $
        Server.responseBuilder HTTP.badRequest400 [] . Builder.byteString $
          "Invalid path " <> path
    CallSetupInvalidRequestHeaders invalid ->
      return $
        Server.responseBuilder (statusInvalidHeaders invalid) [] $
          prettyInvalidHeaders invalid
    CallSetupUnsupportedCompression cid ->
      return $
        Server.responseBuilder HTTP.badRequest400 [] . Builder.byteString $
          "Unsupported compression: " <> BS.UTF8.fromString (show cid)
    CallSetupUnimplementedMethod path -> do
      let trailersOnly :: TrailersOnly
          trailersOnly = properTrailersToTrailersOnly (
              grpcExceptionToTrailers $ grpcUnimplemented path
            , serverContentType
            )
      return $
        Server.responseNoBody HTTP.ok200 $
          buildTrailersOnly contentTypeForUnknown trailersOnly
    CallSetupHandlerLookupException err -> do
      msg <- serverExceptionToClient err
      let trailersOnly :: TrailersOnly
          trailersOnly = properTrailersToTrailersOnly (
              grpcExceptionToTrailers $ GrpcException {
                  grpcError         = GrpcUnknown
                , grpcErrorMessage  = msg
                , grpcErrorDetails  = Nothing
                , grpcErrorMetadata = []
                }
            , serverContentType
            )
      return $
        Server.responseNoBody HTTP.ok200 $
          buildTrailersOnly contentTypeForUnknown trailersOnly
  where
    ServerParams{
        serverExceptionToClient
      , serverContentType
      } = params

-- | Variation on 'chooseContentType' that can be used when the RPC is unknown
contentTypeForUnknown :: ContentType -> Maybe BS.UTF8.ByteString
contentTypeForUnknown ContentTypeDefault       = Nothing
contentTypeForUnknown (ContentTypeOverride ct) = Just ct

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
    , grpcErrorDetails  = Nothing
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
