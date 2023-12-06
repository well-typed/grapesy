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

import Network.GRPC.Common
import Network.GRPC.Spec
import Network.GRPC.Util.Session

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ClientSession rpc = ClientSession

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

data ClientInbound rpc
data ClientOutbound rpc

instance IsRPC rpc => DataFlow (ClientInbound rpc) where
  data Headers (ClientInbound rpc) = InboundHeaders {
        inbHeaders :: ResponseHeaders
      }
    deriving (Show)

  type Message    (ClientInbound rpc) = Output rpc
  type Trailers   (ClientInbound rpc) = [CustomMetadata]
  type NoMessages (ClientInbound rpc) = [CustomMetadata]

instance IsRPC rpc => DataFlow (ClientOutbound rpc) where
  data Headers (ClientOutbound rpc) = OutboundHeaders {
        outHeaders :: RequestHeaders
      }
    deriving (Show)

  type Message  (ClientOutbound rpc) = Input rpc
  type Trailers (ClientOutbound rpc) = NoMetadata

  -- gRPC does not support request trailers, but does not require that a request
  -- has any messages at all. When it does not, the request can be terminated
  -- immediately after the initial set of headers; this makes it essentially a
  -- 'Trailers-Only' case, but the headers are just the normal headers.
  type NoMessages (ClientOutbound rpc) = RequestHeaders

instance IsRPC rpc => IsSession (ClientSession rpc) where
  type Inbound  (ClientSession rpc) = ClientInbound  rpc
  type Outbound (ClientSession rpc) = ClientOutbound rpc

  buildOutboundTrailers _client = \NoMetadata -> []
  parseInboundTrailers _client =
      processResponseTrailers $ parseProperTrailers (Proxy @rpc)

  parseMsg _ _ = parseOutput (Proxy @rpc)
  buildMsg _ _ = buildInput  (Proxy @rpc)

instance IsRPC rpc => InitiateSession (ClientSession rpc) where
  parseResponseRegular _client info = do
      unless (HTTP.statusCode (responseStatus info) == 200) $
        throwIO $ ResponseInvalidStatus (responseStatus info)

      responseHeaders :: ResponseHeaders <-
        case parseResponseHeaders (Proxy @rpc) (responseHeaders info) of
          Left  err    -> throwIO $ ResponseInvalidHeaders err
          Right parsed -> return parsed

      return $ InboundHeaders {
          inbHeaders = responseHeaders
        }

  parseResponseNoMessages _ info = do
      unless (HTTP.statusCode (responseStatus info) == 200) $
        throwIO $ ResponseInvalidStatus (responseStatus info)
      processResponseTrailers
        (fmap getTrailersOnly . parseTrailersOnly (Proxy @rpc))
        (responseHeaders info)

  buildRequestInfo _ start = RequestInfo {
        requestMethod  = rawMethod resourceHeaders
      , requestPath    = rawPath resourceHeaders
      , requestHeaders = buildRequestHeaders (Proxy @rpc) $
            case start of
              FlowStartRegular    headers -> outHeaders headers
              FlowStartNoMessages headers ->            headers
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
-- conversation over. To distinguish, we look at 'trailerGrpcStatus'.
--
-- TODO: We are throwing away 'trailerGrpcMessage' in case of 'GrpcOk'; not
-- sure if that is problematic.
processResponseTrailers ::
     ([HTTP.Header] -> Either String ProperTrailers)
  -> [HTTP.Header]
  -> IO [CustomMetadata]
processResponseTrailers parse raw =
    case parse raw of
      Left  err      -> throwIO $ ResponseInvalidTrailers err
      Right trailers -> case grpcExceptionFromTrailers trailers of
                          Left (_msg, metadata) -> return metadata
                          Right err -> throwIO err

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
