{-# LANGUAGE OverloadedStrings #-}

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
import Data.Void
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
    , clientUpdateMeta  :: ResponseHeaders' -> IO ()
    }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

data ClientInbound rpc
data ClientOutbound rpc

instance IsRPC rpc => DataFlow (ClientInbound rpc) where
  data Headers (ClientInbound rpc) = InboundHeaders {
        inbHeaders     :: ResponseHeaders'
      , inbCompression :: Compression
      }
    deriving (Show)

  type Message    (ClientInbound rpc) = (InboundEnvelope, Output rpc)
  type Trailers   (ClientInbound rpc) = ProperTrailers'
  type NoMessages (ClientInbound rpc) = TrailersOnly'

instance IsRPC rpc => DataFlow (ClientOutbound rpc) where
  data Headers (ClientOutbound rpc) = OutboundHeaders {
        outHeaders     :: RequestHeaders
      , outCompression :: Compression
      }
    deriving (Show)

  type Message  (ClientOutbound rpc) = (OutboundEnvelope, Input rpc)
  type Trailers (ClientOutbound rpc) = NoMetadata

  -- gRPC does not support a Trailers-Only case for requests
  -- (indeed, does not support request trailers at all).
  type NoMessages (ClientOutbound rpc) = Void

instance SupportsClientRpc rpc => IsSession (ClientSession rpc) where
  type Inbound  (ClientSession rpc) = ClientInbound  rpc
  type Outbound (ClientSession rpc) = ClientOutbound rpc

  buildOutboundTrailers _ = \NoMetadata -> []
  parseInboundTrailers  _ = parseTrailers (parseProperTrailers' (Proxy @rpc))

  parseMsg _ = parseOutput (Proxy @rpc) . inbCompression
  buildMsg _ = buildInput  (Proxy @rpc) . outCompression

instance SupportsClientRpc rpc => InitiateSession (ClientSession rpc) where
  parseResponse session info = do
      -- TODO: <https://github.com/well-typed/grapesy/issues/22>
      unless (HTTP.statusCode (responseStatus info) == 200) $
        throwIO $ CallSetupUnexpectedStatus
                    (responseStatus info)
                    (fromMaybe BS.Lazy.empty $ responseBody info)

      if isTrailersOnly info then fmap FlowStartNoMessages $
        parseTrailers (parseTrailersOnly' (Proxy @rpc)) (responseHeaders info)
      else fmap FlowStartRegular $ do
        let responseHeaders' :: ResponseHeaders'
            responseHeaders' = parseResponseHeaders' (Proxy @rpc) $
                                 responseHeaders info
        cIn <- getInboundCompression session $
                 responseCompression responseHeaders'
        clientUpdateMeta session responseHeaders'

        return $ InboundHeaders {
            inbHeaders     = responseHeaders'
          , inbCompression = cIn
          }

  buildRequestInfo _ start = RequestInfo {
        requestMethod  = rawMethod resourceHeaders
      , requestPath    = rawPath resourceHeaders
      , requestHeaders = buildRequestHeaders (Proxy @rpc) $
            case start of
              FlowStartRegular    headers    -> outHeaders headers
              FlowStartNoMessages impossible -> absurd impossible
      }
    where
      resourceHeaders :: RawResourceHeaders
      resourceHeaders = buildResourceHeaders $ ResourceHeaders {
            resourceMethod = Post
          , resourcePath   = rpcPath (Proxy @rpc)
          }

instance NoTrailers (ClientSession rpc) where
  noTrailers _ = NoMetadata

-- | Check if we are in the Trailers-Only case
--
-- It is tempting to use the HTTP @Content-Length@ header to determine whether
-- we are in the Trailers-Only case or not, but this is doubly wrong:
--
-- * There might be servers who use the Trailers-Only case but do not set the
--   @Content-Length@ header (although such a server would not conform to the
--   HTTP spec: "An origin server SHOULD send a @Content-Length@ header field
--   when the content size is known prior to sending the complete header
--   section"; see
--   <https://www.rfc-editor.org/rfc/rfc9110.html#name-content-length>).
-- * Conversely, there might be servers or proxies who /do/ set @Content-Length@
--   header even when it's /not/ the Trailers-Only case (e.g., see
--   <https://github.com/grpc/grpc-web/issues/1101> or
--   <https://github.com/envoyproxy/envoy/issues/5554>).
--
-- We therefore check for the presence of the @grpc-status@ header instead.
isTrailersOnly :: ResponseInfo -> Bool
isTrailersOnly = any isGrpcStatus . responseHeaders
  where
    isGrpcStatus :: HTTP.Header -> Bool
    isGrpcStatus (name, _value) = name == "grpc-status"

-- | Determine compression used for messages from the peer
getInboundCompression ::
     ClientSession rpc
  -> Either InvalidHeaders (Maybe CompressionId)
  -> IO Compression
getInboundCompression session = \case
    Left  err        -> throwIO $ CallSetupInvalidResponseHeaders err
    Right Nothing    -> return noCompression
    Right (Just cid) ->
      case Compr.getSupported (clientCompression session) cid of
        Just compr -> return compr
        Nothing    -> throwIO $ CallSetupUnsupportedCompression cid

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Parse proper trailers
--
-- Although we parse the trailers in a lenient fashion (like all headers),
-- only throwing errors for headers that we really need, if we get no trailers
-- at /all/, then most likely something has gone wrong; for example, perhaps
-- an intermediate cache has dropped the gRPC trailers entirely. We therefore
-- check for this case separately and throw a different error.
parseTrailers ::
     ([HTTP.Header] ->    trailers)
  ->  [HTTP.Header] -> IO trailers
parseTrailers _ []  = throwIO $ CallClosedWithoutTrailers
parseTrailers f raw = return $ f raw

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
  | CallSetupInvalidResponseHeaders InvalidHeaders
  deriving stock (Show)
  deriving anyclass (Exception)

-- | We failed to parse the response trailers
data InvalidTrailers =
    -- | Some of the trailers could not be parsed
    InvalidTrailers {
        invalidTrailers      :: [HTTP.Header]
      , invalidTrailersError :: String
      }

    -- | The server terminated without sending trailers at all
    --
    -- This can also happen when a server just suddenly disappears (or the
    -- network connection is dropped). We cannot reliably distinguish between
    -- "we lost the connection" and "the server closed the connection but did
    -- not send us any trailers".
  | CallClosedWithoutTrailers
  deriving stock (Show)
  deriving anyclass (Exception)
