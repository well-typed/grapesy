module Network.GRPC.Server.Session (
    ServerSession(..)
  , ServerInbound
  , ServerOutbound
  , Headers(..)
  ) where

import Control.Exception
import Data.Proxy
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec
import Network.GRPC.Util.Session

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ServerSession rpc = ServerSession

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

data ServerInbound rpc
data ServerOutbound rpc

instance IsRPC rpc => DataFlow (ServerInbound rpc) where
  data Headers (ServerInbound rpc) = InboundHeaders {
        inbHeaders :: RequestHeaders
      }
    deriving (Show)

  type Message  (ServerInbound rpc) = Input rpc
  type Trailers (ServerInbound rpc) = NoMetadata

  -- See discussion of 'TrailersOnly' in 'ClientOutbound'
  type NoMessages (ServerInbound rpc) = RequestHeaders

instance IsRPC rpc => DataFlow (ServerOutbound rpc) where
  data Headers (ServerOutbound rpc) = OutboundHeaders {
        outHeaders :: ResponseHeaders
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

  parseMsg _ _ = parseInput  (Proxy @rpc)
  buildMsg _ _ = buildOutput (Proxy @rpc)

instance IsRPC rpc => AcceptSession (ServerSession rpc) where
  parseRequestRegular _server info = do
      requestHeaders :: RequestHeaders <-
        case parseRequestHeaders (Proxy @rpc) (requestHeaders info) of
          Left  err  -> throwIO $ RequestInvalidHeaders err
          Right hdrs -> return hdrs

      return InboundHeaders {
          inbHeaders = requestHeaders
        }

  parseRequestNoMessages _ info =
      case parseRequestHeaders (Proxy @rpc) (requestHeaders info) of
        Left  err  -> throwIO $ RequestInvalidHeaders err
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

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data InvalidRequest =
    -- | We failed to parse the request headers
    RequestInvalidHeaders String
  deriving stock (Show)
  deriving anyclass (Exception)