module Network.GRPC.Client.Session (
    ClientSession(..)
  , ClientInbound
  , ClientOutbound
  , Headers(..)
    -- * Exceptions
  , CallSetupFailure(..)
  , InvalidTrailers(..)
  ) where

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Spec
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

data ClientInbound rpc
data ClientOutbound rpc

instance IsRPC rpc => DataFlow (ClientInbound rpc) where
  data Headers (ClientInbound rpc) = InboundHeaders {
        inbHeaders     :: ResponseHeaders
      , inbCompression :: Compression
      }
    deriving (Show)

  type Message    (ClientInbound rpc) = (InboundEnvelope, Output rpc)
  type Trailers   (ClientInbound rpc) = ProperTrailers
  type NoMessages (ClientInbound rpc) = TrailersOnly

instance IsRPC rpc => DataFlow (ClientOutbound rpc) where
  data Headers (ClientOutbound rpc) = OutboundHeaders {
        outHeaders     :: RequestHeaders
      , outCompression :: Compression
      }
    deriving (Show)

  type Message  (ClientOutbound rpc) = (OutboundEnvelope, Input rpc)
  type Trailers (ClientOutbound rpc) = NoMetadata

  -- gRPC does not support request trailers, but does not require that a request
  -- has any messages at all. When it does not, the request can be terminated
  -- immediately after the initial set of headers; this makes it essentially a
  -- 'Trailers-Only' case, but the headers are just the normal headers.
  type NoMessages (ClientOutbound rpc) = RequestHeaders

instance SupportsClientRpc rpc => IsSession (ClientSession rpc) where
  type Inbound  (ClientSession rpc) = ClientInbound  rpc
  type Outbound (ClientSession rpc) = ClientOutbound rpc

  buildOutboundTrailers _client = \NoMetadata -> []
  parseInboundTrailers  _client = \raw ->
      case parseProperTrailers (Proxy @rpc) raw of
        Left  err -> throwIO $ InvalidTrailers err
        Right res -> return res

  parseMsg _ = parseOutput (Proxy @rpc) . inbCompression
  buildMsg _ = buildInput  (Proxy @rpc) . outCompression

instance SupportsClientRpc rpc => InitiateSession (ClientSession rpc) where
  parseResponseRegular client info = do
      unless (HTTP.statusCode (responseStatus info) == 200) $
        throwIO $ CallSetupUnexpectedStatus
                    (responseStatus info)
                    (fromMaybe BS.Lazy.empty $ responseBody info)

      responseHeaders :: ResponseHeaders <-
        case parseResponseHeaders (Proxy @rpc) (responseHeaders info) of
          Left  err    -> throwIO $ CallSetupInvalidResponseHeaders err
          Right parsed -> return parsed

      let cInId :: Maybe CompressionId
          cInId = responseCompression responseHeaders
      cIn :: Compression <-
        case cInId of
          Nothing  -> return noCompression
          Just cid ->
            case Compr.getSupported (clientCompression client) cid of
              Nothing    -> throwIO $ CallSetupUnsupportedCompression cid
              Just compr -> return compr

      clientUpdateMeta client responseHeaders

      return $ InboundHeaders {
          inbHeaders     = responseHeaders
        , inbCompression = cIn
        }

  parseResponseNoMessages _ info = do
      unless (HTTP.statusCode (responseStatus info) == 200) $
        throwIO $ CallSetupUnexpectedStatus
                    (responseStatus info)
                    (fromMaybe BS.Lazy.empty $ responseBody info)

      case parseTrailersOnly (Proxy @rpc) (responseHeaders info) of
        Left  err -> throwIO $ InvalidTrailers err
        Right res -> return res

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

instance NoTrailers (ClientSession rpc) where
  noTrailers _ = NoMetadata

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data CallSetupFailure =
    -- | Server sent a HTTP status other than 200 OK
    --
    -- This can happen in one of two situations:
    --
    -- * We sent a malformed request to the server. This cannot happen in
    --   @grapesy@ unless 'Network.GRPC.Client.ConnParams' is misconfigured.
    -- * We are dealing with non-compliant server.
    --
    -- We also include the response body.
    --
    -- TODO: <https://github.com/well-typed/grapesy/issues/22>.
    -- The spec /does/ require us to deal with non-compliant servers in limited
    -- ways. For example, some non-compliant servers might return a HTTP 404
    -- instead of a HTTP 200 with a 'GrpcStatus' of 'GrpcUnimplemented'. We do
    -- not yet do this.
    CallSetupUnexpectedStatus HTTP.Status Lazy.ByteString

    -- | Server chose an unsupported compression algorithm
  | CallSetupUnsupportedCompression CompressionId

    -- | We failed to parse the response headers
  | CallSetupInvalidResponseHeaders String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | We failed to parse the response trailers
data InvalidTrailers = InvalidTrailers String
  deriving stock (Show)
  deriving anyclass (Exception)
