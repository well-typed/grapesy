module Network.GRPC.Client.Session (
    ClientSession(..)
  , ClientInbound
  , ClientOutbound
  , Headers(..)
  ) where

import Control.Exception
import Control.Monad
import Data.Proxy
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Exceptions
import Network.GRPC.Spec qualified as GRPC
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
    , clientUpdateMeta  :: GRPC.ResponseHeaders -> IO ()
    }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

data ClientInbound rpc
data ClientOutbound rpc

instance IsRPC rpc => DataFlow (ClientInbound rpc) where
  data Headers (ClientInbound rpc) = InboundHeaders {
        inbHeaders     :: GRPC.ResponseHeaders
      , inbCompression :: Compression
      }
    deriving (Show)

  type Message        (ClientInbound rpc) = Output rpc
  type ProperTrailers (ClientInbound rpc) = [CustomMetadata]
  type TrailersOnly   (ClientInbound rpc) = [CustomMetadata]

instance IsRPC rpc => DataFlow (ClientOutbound rpc) where
  data Headers (ClientOutbound rpc) = OutboundHeaders {
        outHeaders     :: GRPC.RequestHeaders
      , outCompression :: Compression
      }
    deriving (Show)

  type Message        (ClientOutbound rpc) = Input rpc
  type ProperTrailers (ClientOutbound rpc) = ()

  -- gRPC does not support request trailers, but does not require that a request
  -- has any messages at all. When it does not, the request can be terminated
  -- immediately after the initial set of headers; this makes it essentially a
  -- 'Trailers-Only' case, but the headers are just the normal headers.
  type TrailersOnly (ClientOutbound rpc) = GRPC.RequestHeaders

instance IsRPC rpc => IsSession (ClientSession rpc) where
  type Inbound  (ClientSession rpc) = ClientInbound  rpc
  type Outbound (ClientSession rpc) = ClientOutbound rpc

  buildProperTrailers _client = \() ->
      [] -- Request trailers are not supported by gRPC
  parseProperTrailers _client =
      processResponseTrailers $ Resp.parseProperTrailers (Proxy @rpc)

  parseMsg _ = LP.parseOutput (Proxy @rpc) . inbCompression
  buildMsg _ = LP.buildInput  (Proxy @rpc) . outCompression

instance IsRPC rpc => InitiateSession (ClientSession rpc) where
  parseResponseRegular client info = do
      unless (HTTP.statusCode (responseStatus info) == 200) $
        throwIO $ ResponseInvalidStatus (responseStatus info)

      responseHeaders :: GRPC.ResponseHeaders <-
        case Resp.parseHeaders (Proxy @rpc) (responseHeaders info) of
          Left  err    -> throwIO $ ResponseInvalidHeaders err
          Right parsed -> return parsed

      cIn :: Compression <-
        Compr.getSupported (clientCompression client) $
          GRPC.responseCompression responseHeaders

      clientUpdateMeta client responseHeaders

      return $ InboundHeaders {
          inbHeaders     = responseHeaders
        , inbCompression = cIn
        }

  parseResponseTrailersOnly _ info = do
      unless (HTTP.statusCode (responseStatus info) == 200) $
        throwIO $ ResponseInvalidStatus (responseStatus info)
      processResponseTrailers
        (fmap GRPC.getTrailersOnly .  Resp.parseTrailersOnly (Proxy @rpc))
        (responseHeaders info)

  buildRequestInfo _ start = RequestInfo {
        requestMethod  = rawMethod resourceHeaders
      , requestPath    = rawPath resourceHeaders
      , requestHeaders = Req.buildHeaders (Proxy @rpc) $
            case start of
              FlowStartRegular      headers -> outHeaders headers
              FlowStartTrailersOnly headers ->            headers
      }
    where
      resourceHeaders :: RawResourceHeaders
      resourceHeaders = buildResourceHeaders $ ResourceHeaders {
            resourceMethod = Post
          , resourcePath   = rpcPath (Proxy @rpc)
          }

-- | Process response trailers
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
processResponseTrailers ::
     ([HTTP.Header] -> Either String GRPC.ProperTrailers)
  -> [HTTP.Header]
  -> IO [CustomMetadata]
processResponseTrailers parse raw =
    case parse raw of
      Left  err      -> throwIO $ ResponseInvalidTrailers err
      Right trailers -> case grpcExceptionFromTrailers trailers of
                          Nothing  -> return $ GRPC.trailerMetadata trailers
                          Just err -> throwIO err

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
