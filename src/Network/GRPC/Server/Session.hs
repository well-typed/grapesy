module Network.GRPC.Server.Session (
    ServerSession(..)
  , ServerInbound
  , ServerOutbound
  , Headers(..)
  ) where

import Control.Monad.Except
import Data.Proxy
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Spec
import Network.GRPC.Util.Session

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ServerSession rpc = ServerSession {
      serverCompression :: Compr.Negotation
    }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

data ServerInbound rpc
data ServerOutbound rpc

instance IsRPC rpc => DataFlow (ServerInbound rpc) where
  data Headers (ServerInbound rpc) = InboundHeaders {
        inbHeaders     :: RequestHeaders
      , inbCompression :: Compression
      }
    deriving (Show)

  type Message  (ServerInbound rpc) = Input rpc
  type Trailers (ServerInbound rpc) = NoMetadata

  -- See discussion of 'TrailersOnly' in 'ClientOutbound'
  type NoMessages (ServerInbound rpc) = RequestHeaders

instance IsRPC rpc => DataFlow (ServerOutbound rpc) where
  data Headers (ServerOutbound rpc) = OutboundHeaders {
        outHeaders     :: ResponseHeaders
      , outCompression :: Compression
      }
    deriving (Show)

  type Message    (ServerOutbound rpc) = Output rpc
  type Trailers   (ServerOutbound rpc) = ProperTrailers
  type NoMessages (ServerOutbound rpc) = TrailersOnly

instance IsRPC rpc => IsSession (ServerSession rpc) where
  type Inbound  (ServerSession rpc) = ServerInbound rpc
  type Outbound (ServerSession rpc) = ServerOutbound rpc

  parseInboundTrailers _ = \_ -> return NoMetadata
  buildOutboundTrailers _ = buildProperTrailers

  parseMsg _ = parseInput  (Proxy @rpc) . inbCompression
  buildMsg _ = buildOutput (Proxy @rpc) . outCompression

instance IsRPC rpc => AcceptSession (ServerSession rpc) where
  parseRequestRegular server headers = runExcept $ do
      requestHeaders :: RequestHeaders <-
        case parseRequestHeaders (Proxy @rpc) headers of
          Left  err  -> throwError $ PeerSentInvalidHeaders err
          Right hdrs -> return hdrs

      let cInId :: Maybe CompressionId
          cInId = requestCompression requestHeaders
      cIn :: Compression <-
        case cInId of
          Nothing  -> return noCompression
          Just cid ->
            case Compr.getSupported (serverCompression server) cid of
              Nothing    -> throwError $ PeerChoseUnsupportedCompression cid
              Just compr -> return compr

      return InboundHeaders {
          inbHeaders    = requestHeaders
        , inbCompression = cIn
        }

  parseRequestNoMessages _ headers = runExcept $
      case parseRequestHeaders (Proxy @rpc) headers of
        Left  err  -> throwError $ PeerSentInvalidHeaders err
        Right hdrs -> return hdrs

  buildResponseInfo _ start = ResponseInfo {
        responseStatus  = HTTP.ok200
      , responseHeaders =
          case start of
            FlowStartRegular headers ->
              buildResponseHeaders (Proxy @rpc) (outHeaders headers)
            FlowStartNoMessages trailers ->
              buildTrailersOnly trailers
      }
