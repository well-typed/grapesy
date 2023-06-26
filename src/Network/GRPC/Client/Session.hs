module Network.GRPC.Client.Session (
    ClientSession(..)
  , InboundHeaders(..)
  , OutboundHeaders(..)
  ) where

import Control.Exception
import Control.Monad
import Data.Proxy
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Exceptions
import Network.GRPC.Spec
import Network.GRPC.Spec.Compression (Compression)
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.LengthPrefixed qualified as LP
import Network.GRPC.Spec.PseudoHeaders
import Network.GRPC.Spec.Request qualified as Req
import Network.GRPC.Spec.Response qualified as Resp
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Session

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ClientSession rpc = ClientSession {
      clientCompression :: Compr.Negotation
    , clientUpdateMeta  :: ResponseHeaders -> IO ()
    }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance IsRPC rpc => IsSession (ClientSession rpc) where
  data InboundHeaders (ClientSession rpc) = InboundHeaders {
        inbHeaders     :: ResponseHeaders
      , inbCompression :: Compression
      }
    deriving (Show)
  data OutboundHeaders (ClientSession rpc) = OutboundHeaders {
        outHeaders     :: RequestHeaders
      , outCompression :: Compression
      }
    deriving (Show)

  type InboundTrailers  (ClientSession rpc) = [CustomMetadata]
  type OutboundTrailers (ClientSession rpc) = ()

  type InboundMsg  (ClientSession rpc) = Output rpc
  type OutboundMsg (ClientSession rpc) = Input  rpc

  -- Request trailers are not supported by gRPC
  buildTrailers _client = \() -> return []

  -- Response trailers
  --
  -- The gRPC spec states that
  --
  -- > Trailers-Only is permitted for calls that produce an immediate error
  --
  -- However, in practice gRPC servers can also respond with @Trailers-Only@ in
  -- non-error cases, simply indicating that the server considers the
  -- conversation over. To distinguish, we look at 'trailerGrpcStatus': in case
  -- of 'GrpcOk' we return the 'Trailers', and in the case of 'GrpcError' we
  -- throw 'RpcImmediateError'.
  parseTrailers _client raw =
      case Resp.parseTrailers raw of
        Left  err      -> throwIO $ ResponseInvalidTrailers err
        Right trailers -> case grpcExceptionFromTrailers trailers of
                            Nothing  -> return $ trailerMetadata trailers
                            Just err -> throwIO err

  parseMsg _ = LP.parseOutput (Proxy @rpc) . inbCompression
  buildMsg _ = LP.buildInput  (Proxy @rpc) . outCompression

instance IsRPC rpc => InitiateSession (ClientSession rpc) where
  parseResponseInfo client info = do
      unless (HTTP.statusCode (responseStatus info) == 200) $
        throwIO $ ResponseInvalidStatus (responseStatus info)

      responseHeaders :: ResponseHeaders <-
        case Resp.parseHeaders (Proxy @rpc) (responseHeaders info) of
          Left  err    -> throwIO $ ResponseInvalidHeaders err
          Right parsed -> return parsed

      cIn :: Compression <-
        Compr.getSupported (clientCompression client) $
          responseCompression responseHeaders

      clientUpdateMeta client responseHeaders

      return $ InboundHeaders {
          inbHeaders     = responseHeaders
        , inbCompression = cIn
        }

  buildRequestInfo _ outbound = do
      return RequestInfo {
          requestMethod  = rawMethod resourceHeaders
        , requestPath    = rawPath resourceHeaders
        , requestHeaders = Req.buildHeaders (Proxy @rpc) $ outHeaders outbound
        }
    where
      resourceHeaders :: RawResourceHeaders
      resourceHeaders = buildResourceHeaders $ ResourceHeaders {
            resourceMethod = Post
          , resourcePath   = rpcPath (Proxy @rpc)
          }

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data InvalidResponse =
    -- | Server sent a HTTP status other than 200 OK
    --
    -- The spec explicitly disallows this; if a particular method is not
    -- supported, then the server will respond with HTTP 200 and then a
    -- grpc-status of 'GrpcUnimplemented'.
    ResponseInvalidStatus HTTP.Status

    -- | We failed to parse the response headers
  | ResponseInvalidHeaders String

    -- | We failed to parse the response trailers
  | ResponseInvalidTrailers String
  deriving stock (Show)
  deriving anyclass (Exception)
