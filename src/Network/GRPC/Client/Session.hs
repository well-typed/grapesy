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
  parseInboundTrailers  _ = \trailers ->
      if null trailers then
        -- Although we parse the trailers in a lenient fashion (like all
        -- headers), only throwing errors for headers that we really need, if we
        -- get no trailers at /all/, then most likely something has gone wrong;
        -- for example, perhaps an intermediate cache has dropped the gRPC
        -- trailers entirely. We therefore check for this case separately and
        -- throw a different error.
        throwIO $ CallClosedWithoutTrailers
      else
        return $ parseProperTrailers' (Proxy @rpc) trailers

  parseMsg _ = parseOutput (Proxy @rpc) . inbCompression
  buildMsg _ = buildInput  (Proxy @rpc) . outCompression

instance SupportsClientRpc rpc => InitiateSession (ClientSession rpc) where
  parseResponse session (ResponseInfo status headers body) =
      case classifyServerResponse (Proxy @rpc) status headers body of
        Left trailersOnly ->
          -- We classify the response as Trailers-Only if the grpc-status header
          -- is present, or when the HTTP status is anything other than 200 OK
          -- (which we treat, as per the spec, as an implicit grpc-status).
          -- The 'CallClosedWithoutTrailers' case is therefore not relevant.
          return $ FlowStartNoMessages trailersOnly
        Right responseHeaders' -> do
          cIn <- getInboundCompression session $
                   responseCompression responseHeaders'
          clientUpdateMeta session responseHeaders'
          return $ FlowStartRegular $ InboundHeaders {
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
  Exceptions
-------------------------------------------------------------------------------}

data CallSetupFailure =
    -- | Server chose an unsupported compression algorithm
    CallSetupUnsupportedCompression CompressionId

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
