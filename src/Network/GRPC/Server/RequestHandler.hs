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

import Control.Exception
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Tracer
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.Maybe (fromMaybe)
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Server.Call
import Network.GRPC.Server.Context (ServerContext)
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Server.Handler (RpcHandler(..))
import Network.GRPC.Server.Handler qualified as Handler
import Network.GRPC.Server.RequestHandler.API
import Network.GRPC.Spec
import Network.GRPC.Util.Session.Server

{-------------------------------------------------------------------------------
  Construct request handler
-------------------------------------------------------------------------------}

-- | Construct request handler
requestHandler ::
     Handler.Map IO
  -> ServerContext
  -> RequestHandler (Either SomeException ())
requestHandler handlers ctxt unmask request respond = do
    -- TODO: Proper "Apache style" logging (in addition to the debug logging)
    liftIO $ traceWith tracer $ Context.ServerDebugRequest rawHeaders

    -- Set up the call and run the handler
    --
    -- Returns @Left@ of some error if call setup fails, or @Right@ the handler
    -- result otherwise.
    mErr <- runExceptT $ do
      resourceHeaders :: ResourceHeaders <-
        case parseRawHeaders rawHeaders of
          Right hdrs -> return hdrs
          Left  err  -> throwError (
                            toException err
                          , outOfSpecResponse err
                          )

      RpcHandler (handler :: Call rpc -> IO ()) <- do
        let path :: Path
            path = resourcePath resourceHeaders

            err :: GrpcException
            err = grpcUnimplemented path

        case Handler.lookup path handlers of
          Just h  -> return h
          Nothing -> throwError (
                         toException err
                       , inSpecResponse $ grpcExceptionToTrailers err
                       )

      call :: Call rpc <- do
        mCall <- liftIO $ setupCall connectionToClient ctxt
        case mCall of
          Right call -> return call
          Left  err  -> throwError (
                            err
                          , inSpecResponse $ serverExceptionToClientError err
                          )

      liftIO $ do
        -- TODO: Timeouts
        runHandler unmask call handler

    case mErr of
      Right res -> return res
      Left (err, resp) -> do
        -- Something went wrong during call setup. No response has been sent
        -- to the client at all yet. We try to tell the client what happened,
        -- but ignore any exceptions that might arise from doing so.
        liftIO $ traceWith tracer $ Context.ServerDebugCallSetupFailed err
        handle ignoreExceptions $ respond resp
        return $ Left err
  where
    connectionToClient :: ConnectionToClient
    connectionToClient = ConnectionToClient{request, respond}

    tracer :: Tracer IO Context.ServerDebugMsg
    tracer = Context.serverDebugTracer $ Context.params ctxt

    ignoreExceptions :: SomeException -> IO ()
    ignoreExceptions _ = return ()

    rawHeaders :: RawResourceHeaders
    rawHeaders = rawResourceHeaders request

    -- We failed to set up the call, but gRPC spec was not violated
    --
    -- Note that gRPC uses HTTP 200 even when there are gRPC errors
    -- (it's the @grpc-status@ value that matters).
    inSpecResponse :: ProperTrailers -> HTTP2.Response
    inSpecResponse =
          HTTP2.responseNoBody HTTP.ok200
        . buildTrailersOnly
        . TrailersOnly



{-------------------------------------------------------------------------------
  Pseudo headers
-------------------------------------------------------------------------------}

parseRawHeaders :: RawResourceHeaders -> Either OutOfSpecError ResourceHeaders
parseRawHeaders raw =
    case parseResourceHeaders raw of
      Left (InvalidPath   x) -> Left $ bad "invalid path"   x
      Left (InvalidMethod x) -> Left $ httpMethodNotAllowed x
      Right hdrs             -> return hdrs
  where
    bad :: Strict.ByteString -> Strict.ByteString -> OutOfSpecError
    bad msg arg = httpBadRequest $ mconcat [
          msg
        , ": "
        , arg
        ]

rawResourceHeaders :: HTTP2.Request -> RawResourceHeaders
rawResourceHeaders req = RawResourceHeaders {
      rawPath   = fromMaybe "" $ HTTP2.requestPath   req
    , rawMethod = fromMaybe "" $ HTTP2.requestMethod req
    }

{-------------------------------------------------------------------------------
  Out-of-spec errors
-------------------------------------------------------------------------------}

-- | The request did not conform to the gRPC specification
--
-- The gRPC spec mandates that we /always/ return HTTP 200 OK, but it does
-- not explicitly say what to do when the request is mal-formed (not
-- conform the gRPC specification). We choose to return HTTP errors in
-- this case.
--
-- See <https://datatracker.ietf.org/doc/html/rfc7231#section-6.5> for
-- a discussion of the HTTP error codes.
--
-- Testing out-of-spec errors can be bit awkward. One option is @curl@:
--
-- > curl --verbose --http2 --http2-prior-knowledge http://localhost:50051/
data OutOfSpecError = OutOfSpecError {
      outOfSpecStatus  :: HTTP.Status
    , outOfSpecHeaders :: [HTTP.Header]
    , outOfSpecBody    :: Strict.ByteString
    }
  deriving stock (Show)
  deriving anyclass (Exception)

outOfSpecResponse :: OutOfSpecError -> HTTP2.Response
outOfSpecResponse err =
    HTTP2.responseBuilder
      (outOfSpecStatus  err)
      (outOfSpecHeaders err)
      (Builder.byteString $ outOfSpecBody err)

-- | 405 Method Not Allowed
--
-- <https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.5>
-- <https://datatracker.ietf.org/doc/html/rfc7231#section-7.4.1>
httpMethodNotAllowed :: Strict.ByteString -> OutOfSpecError
httpMethodNotAllowed method = OutOfSpecError {
      outOfSpecStatus  = HTTP.methodNotAllowed405
    , outOfSpecHeaders = [("Allow", "POST")]
    , outOfSpecBody    = mconcat [
          "Unexpected :method " <> method <> ".\n"
        , "The only method supported by gRPC is POST.\n"
        ]
    }

-- | 400 Bad Request
--
-- <https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.1>
httpBadRequest :: Strict.ByteString -> OutOfSpecError
httpBadRequest body = OutOfSpecError {
      outOfSpecStatus  = HTTP.badRequest400
    , outOfSpecHeaders = []
    , outOfSpecBody    = body
    }

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
