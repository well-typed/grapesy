module Network.GRPC.Server.Session (
    ServerSession(..)
  , ServerInbound
  , ServerOutbound
  , Headers(..)
    -- * Exceptions
  , CallSetupFailure(..)
  ) where

import Control.Exception
import Data.Proxy
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server.Context
import Network.GRPC.Spec
import Network.GRPC.Util.Session

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ServerSession rpc = ServerSession {
      serverSessionContext :: ServerContext
    }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

data ServerInbound rpc
data ServerOutbound rpc

instance IsRPC rpc => DataFlow (ServerInbound rpc) where
  data Headers (ServerInbound rpc) = InboundHeaders {
        inbHeaders     :: RequestHeaders'
      , inbCompression :: Compression
      }
    deriving (Show)

  type Message  (ServerInbound rpc) = (InboundEnvelope, Input rpc)
  type Trailers (ServerInbound rpc) = NoMetadata

  -- See discussion of 'TrailersOnly' in 'ClientOutbound'
  type NoMessages (ServerInbound rpc) = RequestHeaders'

instance IsRPC rpc => DataFlow (ServerOutbound rpc) where
  data Headers (ServerOutbound rpc) = OutboundHeaders {
        outHeaders     :: ResponseHeaders
      , outCompression :: Compression
      }
    deriving (Show)

  type Message    (ServerOutbound rpc) = (OutboundEnvelope, Output rpc)
  type Trailers   (ServerOutbound rpc) = ProperTrailers
  type NoMessages (ServerOutbound rpc) = TrailersOnly

instance SupportsServerRpc rpc => IsSession (ServerSession rpc) where
  type Inbound  (ServerSession rpc) = ServerInbound rpc
  type Outbound (ServerSession rpc) = ServerOutbound rpc

  parseInboundTrailers  _ = \_ -> return NoMetadata
  buildOutboundTrailers _ = buildProperTrailers

  parseMsg _ = parseInput  (Proxy @rpc) . inbCompression
  buildMsg _ = buildOutput (Proxy @rpc) . outCompression

instance SupportsServerRpc rpc => AcceptSession (ServerSession rpc) where
  parseRequestRegular session headers = do
      cIn <- getInboundCompression session $ requestCompression requestHeaders'
      return InboundHeaders {
          inbHeaders     = requestHeaders'
        , inbCompression = cIn
        }
    where
      requestHeaders' :: RequestHeaders'
      requestHeaders' = parseRequestHeaders' (Proxy @rpc) headers

  parseRequestNoMessages _ headers =
      return requestHeaders'
    where
      requestHeaders' :: RequestHeaders'
      requestHeaders' = parseRequestHeaders' (Proxy @rpc) headers

  buildResponseInfo _ start = ResponseInfo {
        responseStatus  = HTTP.ok200
      , responseHeaders =
          case start of
            FlowStartRegular headers ->
              buildResponseHeaders (Proxy @rpc) (outHeaders headers)
            FlowStartNoMessages trailers ->
              buildTrailersOnly (Proxy @rpc) trailers
      , responseBody = Nothing
      }

-- | Determine compression used for messages from the peer
getInboundCompression ::
     ServerSession rpc
  -> Either InvalidRequestHeaders (Maybe CompressionId)
  -> IO Compression
getInboundCompression session = \case
    Left  err        -> throwIO $ CallSetupInvalidRequestHeaders err
    Right Nothing    -> return noCompression
    Right (Just cid) ->
      case Compr.getSupported serverCompression cid of
        Just compr -> return compr
        Nothing    -> throwIO $ CallSetupUnsupportedCompression cid
  where
    ServerSession{serverSessionContext} = session
    ServerContext{serverParams} = serverSessionContext
    ServerParams{serverCompression} = serverParams

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | We failed to setup the call from the client
data CallSetupFailure =
    -- | Client sent resource headers that were not conform the gRPC spec
    CallSetupInvalidResourceHeaders InvalidResourceHeaders

    -- | Invalid request headers
    --
    -- 'CallSetupInvalidResourceHeaders' refers to an invalid method (anything
    -- other than POST) or an invalid path; 'CallSetupInvalidRequestHeaders'
    -- means we could not parse the HTTP headers according to the gRPC spec.
  | CallSetupInvalidRequestHeaders InvalidRequestHeaders

    -- | Client chose unsupported compression algorithm
    --
    -- This is indicative of a misbehaving peer: a client should not use a
    -- compression algorithm unless they have evidence that the server supports
    -- it. The server cannot process such a request, as it has no way of
    -- decompression messages sent by the client.
  | CallSetupUnsupportedCompression CompressionId

    -- | No registered handler for the specified path
    --
    -- Note on terminology: HTTP has \"methods\" such as POST, GET, etc; gRPC
    -- supports only POST, and when another HTTP method is chosen, this will
    -- result in 'CallSetupInvalidResourceHeaders'. However, gRPC itself also
    -- has the concept of a "method" (a method, or gRPC call, supported by a
    -- particular service); it's these methods that
    -- 'CallSetupUnimplementedMethod' is referring to.
  | forall rpc. IsRPC rpc => CallSetupUnimplementedMethod (Proxy rpc) Path

deriving stock    instance Show      CallSetupFailure
deriving anyclass instance Exception CallSetupFailure

