{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Server (
    -- * Handlers
    RpcHandler(..)
  , Call -- opaque

    -- * Ongoing calls
  , IsFinal(..)
  , Trailers(..)
  , recvInput
  , sendOutput
  , sendTrailers

    -- * Server proper
  , server
  ) where

import Control.Concurrent.STM
import Control.Monad.Except
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Builder qualified as BS (Builder)
import Data.ByteString.Builder qualified as BS.Builder
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Server.Call
import Network.GRPC.Spec
import Network.GRPC.Spec.PseudoHeaders
import Network.GRPC.Spec.Response qualified as Response
import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  Handlers

  This is essentially an untyped interface; for a typed layer, see
  "Network.GRPC.Server.Protobuf".
-------------------------------------------------------------------------------}

data RpcHandler = forall rpc. IsRPC rpc => RpcHandler {
      rpcHandlerFor :: rpc
    , rpcHandlerRun :: Call rpc -> IO ()
    }

instance Show RpcHandler where
  show (RpcHandler rpc _) = "<RpcHandler " ++ show rpc ++ ">"

handlerPath :: RpcHandler -> Path
handlerPath (RpcHandler rpc _) = rpcPath rpc

{-------------------------------------------------------------------------------
  Ongoing calls
-------------------------------------------------------------------------------}

recvInput :: Call rpc -> STM (IsFinal, Maybe (Input rpc))
recvInput = undefined

sendOutput :: Call rpc -> Output rpc -> STM ()
sendOutput = undefined

sendTrailers :: Call rpc -> Trailers -> STM ()
sendTrailers = undefined

{-------------------------------------------------------------------------------
  Server proper
-------------------------------------------------------------------------------}

server :: [RpcHandler] -> HTTP2.Server
server handlers req aux respond = do
    print req
    mTrailersOnly <- runExceptT $ handleGrpcRequest handlerMap req aux respond
    case mTrailersOnly of
      Right () ->
        return ()
      Left (GrpcOutOfSpecErr err hdrs msg) -> do
        let body = HTTP2.responseBuilder err hdrs msg
        respond body []
      Left (GrpcInSpecErr err) -> do
        let body = HTTP2.responseNoBody HTTP.ok200 $ Response.buildTrailers err
        respond body []
  where
    handlerMap :: HashMap Path RpcHandler
    handlerMap = HashMap.fromList $ map (\h -> (handlerPath h, h)) handlers

-- TODO: Deal with https.
handleGrpcRequest ::
     HashMap Path RpcHandler
  -> HTTP2.Request
  -> HTTP2.Aux
  -> (HTTP2.Response -> [HTTP2.PushPromise] -> IO ())
  -> ExceptT ServerError IO ()
handleGrpcRequest handlerMap req _aux _respond = do
    -- > Call-Definition →
    -- >   Method
    -- >   Scheme
    -- >   Path
    -- >   TE
    -- >   [Authority]
    -- >   [Timeout]
    -- >   Content-Type
    -- >   [Message-Type]
    -- >   [Message-Encoding]
    -- >   [Message-Accept-Encoding]
    -- >   [User-Agent]

    pseudoHeaders@PseudoHeaders{
        resourceHeaders = ResourceHeaders{resourcePath}
      } <- getPseudoHeaders req

    -- TODO: For content types that aren't application/grpc (with or without a
    -- subtype), probably an HTTP 415 Unsupported Media Type response

    -- TODO (Maybe): For protocols that aren't HTTP/2, probably an HTTP 426
    -- Upgrade Required response. (Not even sure that we /can/ even do this
    -- with http2).

    liftIO $ print pseudoHeaders
    liftIO $ print handlerMap

    handler <- case HashMap.lookup resourcePath handlerMap of
                 Nothing -> throwError $ grpcUnimplemented resourcePath
                 Just s  -> return s

    case handler of
      RpcHandler _ h -> liftIO $ h Call

{-------------------------------------------------------------------------------
  Pseudo headers
-------------------------------------------------------------------------------}

getPseudoHeaders :: forall m.
     MonadError ServerError m
  => HTTP2.Request -> m PseudoHeaders
getPseudoHeaders req =
    case parsePseudoHeaders (rawPseudoHeaders req) of
      Left (InvalidScheme    x) -> bad "invalid scheme"    x
      Left (InvalidAuthority x) -> bad "invalid authority" x
      Left (InvalidPath      x) -> bad "invalid path"      x
      Left (InvalidMethod    x) -> throwError $ httpMethodNotAllowed x
      Right hdrs                -> return hdrs
  where
    bad :: BS.Builder.Builder -> BS.Strict.ByteString -> m a
    bad msg arg = throwError $ httpBadRequest $ mconcat [
          msg
        , ": "
        , BS.Builder.byteString arg
        ]

rawPseudoHeaders :: HTTP2.Request -> RawPseudoHeaders
rawPseudoHeaders req = RawPseudoHeaders {
      rawServerHeaders = RawServerHeaders {
          rawScheme    = fromMaybe "" $ HTTP2.requestScheme    req
        , rawAuthority = fromMaybe "" $ HTTP2.requestAuthority req
        }
    , rawResourceHeaders = RawResourceHeaders {
          rawPath   = fromMaybe "" $ HTTP2.requestPath   req
        , rawMethod = fromMaybe "" $ HTTP2.requestMethod req
        }
    }

{-------------------------------------------------------------------------------
  Errors

  Testing the out-of-spec errors can be bit awkward. One way is to use curl:

  > curl --verbose --http2 --http2-prior-knowledge http://localhost:50051/
-------------------------------------------------------------------------------}

-- | Server error
data ServerError =
    -- | HTTP error
    --
    -- The gRPC spec mandates that we /always/ return HTTP 200 OK, but it does
    -- not explicitly say what to do when the request is mal-formed (not
    -- conform the gRPC specification). We choose to return HTTP errors in
    -- this case.
    --
    -- See <https://datatracker.ietf.org/doc/html/rfc7231#section-6.5> for
    -- a discussion of the HTTP error codes.
    GrpcOutOfSpecErr HTTP.Status [HTTP.Header] BS.Builder

    -- | gRPC error
  | GrpcInSpecErr Trailers

-- | 405 Method Not Allowed
--
-- <https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.5>
-- <https://datatracker.ietf.org/doc/html/rfc7231#section-7.4.1>
httpMethodNotAllowed :: Strict.ByteString -> ServerError
httpMethodNotAllowed method =
    GrpcOutOfSpecErr
      HTTP.methodNotAllowed405
      [("Allow", "POST")]
      (mconcat [
          "Unexpected :method " <> BS.Builder.byteString method <> ".\n"
        , "The only method supported by gRPC is POST.\n"
        ])

-- | 400 Bad Request
--
-- <https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.1>
httpBadRequest :: BS.Builder -> ServerError
httpBadRequest = GrpcOutOfSpecErr HTTP.badRequest400 []

grpcUnimplemented :: Path -> ServerError
grpcUnimplemented path = GrpcInSpecErr $ Trailers {
      trailerGrpcStatus  = GrpcError GrpcUnimplemented
    , trailerGrpcMessage = Just $ mconcat [
                               "Method "
                             , pathService path
                             , "."
                             , pathMethod path
                             , " not implemented"
                             ]
    , trailerCustom      = []
    }
