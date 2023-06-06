module Network.GRPC.Server.Session (
    ServerSession(..)
  , InboundHeaders(..)
  , OutboundHeaders(..)
  ) where

import Control.Exception
import Data.List.NonEmpty (NonEmpty)
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Common.Compression (Compression, CompressionId)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Spec
import Network.GRPC.Spec.LengthPrefixed qualified as LP
import Network.GRPC.Spec.Request qualified as Req
import Network.GRPC.Spec.Response qualified as Resp
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Session

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ServerSession rpc = ServerSession {
      serverRPC         :: rpc
    , serverCompression :: Compr.Negotation
    }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance IsRPC rpc => IsSession (ServerSession rpc) where
  data InboundHeaders (ServerSession rpc) = InboundHeaders {
        inbHeaders     :: RequestHeaders
      , inbCompression :: Compression
      }
    deriving (Show)
  data OutboundHeaders (ServerSession rpc) = OutboundHeaders {
        outHeaders     :: ResponseHeaders
      , outCompression :: Compression
      }
    deriving (Show)

  type InboundTrailers  (ServerSession rpc) = ()
  type OutboundTrailers (ServerSession rpc) = Trailers

  type InboundMsg  (ServerSession rpc) = Input  rpc
  type OutboundMsg (ServerSession rpc) = Output rpc

  parseTrailers _ = \_ -> return ()
  buildTrailers _ = return . Resp.buildTrailers

  parseMsg ServerSession{serverRPC} = LP.parseInput  serverRPC . inbCompression
  buildMsg ServerSession{serverRPC} = LP.buildOutput serverRPC . outCompression

instance IsRPC rpc => AcceptSession (ServerSession rpc) where
  parseRequestInfo server@ServerSession{serverRPC} info = do
      requestHeaders :: RequestHeaders <-
        case Req.parseHeaders serverRPC (requestHeaders info) of
          Left  err  -> throwIO $ RequestInvalidHeaders err
          Right hdrs -> return hdrs

      cIn :: Compression <-
        Compr.getSupported (serverCompression server) $
          requestCompression requestHeaders

      return InboundHeaders {
          inbHeaders    = requestHeaders
        , inbCompression = cIn
        }

  buildResponseInfo ServerSession{serverRPC} outbound =
      return ResponseInfo {
          responseStatus  = HTTP.ok200
        , responseHeaders = Resp.buildHeaders serverRPC $ outHeaders outbound
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