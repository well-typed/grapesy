module Network.GRPC.Server.Session (
    ServerSession(..)
  , ServerInbound
  , ServerOutbound
  , Headers(..)
  ) where

import Control.Exception
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Common.Compression (Compression, CompressionId)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Spec qualified as GRPC
import Network.GRPC.Spec.LengthPrefixed qualified as LP
import Network.GRPC.Spec.Request qualified as Req
import Network.GRPC.Spec.Response qualified as Resp
import Network.GRPC.Spec.RPC
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
        inbHeaders     :: GRPC.RequestHeaders
      , inbCompression :: Compression
      }
    deriving (Show)

  type Msg      (ServerInbound rpc) = Input rpc
  type Trailers (ServerInbound rpc) = ()

instance IsRPC rpc => DataFlow (ServerOutbound rpc) where
  data Headers (ServerOutbound rpc) = OutboundHeaders {
        outHeaders     :: GRPC.ResponseHeaders
      , outCompression :: Compression
      }
    deriving (Show)

  type Msg      (ServerOutbound rpc) = Output rpc
  type Trailers (ServerOutbound rpc) = GRPC.Trailers

instance IsRPC rpc => IsSession (ServerSession rpc) where
  type Inbound  (ServerSession rpc) = ServerInbound rpc
  type Outbound (ServerSession rpc) = ServerOutbound rpc

  parseTrailers _ = \_ -> return ()
  buildTrailers _ = return . Resp.buildTrailers

  parseMsg _ = LP.parseInput  (Proxy @rpc) . inbCompression
  buildMsg _ = LP.buildOutput (Proxy @rpc) . outCompression

instance IsRPC rpc => AcceptSession (ServerSession rpc) where
  parseRequestInfo server info = do
      requestHeaders :: GRPC.RequestHeaders <-
        case Req.parseHeaders (Proxy @rpc) (requestHeaders info) of
          Left  err  -> throwIO $ RequestInvalidHeaders err
          Right hdrs -> return hdrs

      cIn :: Compression <-
        Compr.getSupported (serverCompression server) $
          GRPC.requestCompression requestHeaders

      return InboundHeaders {
          inbHeaders    = requestHeaders
        , inbCompression = cIn
        }

  buildResponseInfo _ outbound =
      return ResponseInfo {
          responseStatus  = HTTP.ok200
        , responseHeaders = Resp.buildHeaders (Proxy @rpc) $ outHeaders outbound
        }

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data InvalidRequest =
    -- | We failed to parse the request headers
    RequestInvalidHeaders String

    -- | The client chose an unsupported compression algorithm
  | RequestUnsupportedInboundCompression CompressionId

    -- | We don't support any of the client's requested compression algorithms
  | RequestUnsupportedOutboundCompression (NonEmpty CompressionId)
  deriving stock (Show)
  deriving anyclass (Exception)