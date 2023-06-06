module Network.GRPC.Util.Session.API (
    -- * Preliminaries
    RequestInfo(..)
  , ResponseInfo(..)
    -- * Main definitions
  , IsSession(..)
  , InitiateSession(..)
  , AcceptSession(..)
    -- * Exceptions
  , PeerException(..)
  ) where

import Control.Exception
import Data.ByteString.Builder (Builder)
import Data.Kind
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Util.Parser

-- We import from @.Internal@ to avoid biasing towards @.Server@ or @.Client@
-- (this is actually defined in a hidden module @Network.HTTP2.Arch.Types@).
import Network.HTTP2.Internal qualified as HTTP2

{-------------------------------------------------------------------------------
  Preliminaries
-------------------------------------------------------------------------------}

data RequestInfo = RequestInfo {
      requestMethod  :: HTTP.Method
    , requestPath    :: HTTP2.Path
    , requestHeaders :: [HTTP.Header]
    }

data ResponseInfo = ResponseInfo {
      responseStatus  :: HTTP.Status
    , responseHeaders :: [HTTP.Header]
    }

{-------------------------------------------------------------------------------
  Main definition
-------------------------------------------------------------------------------}

-- | Session between two nodes in the network
--
-- The session is described from the point of view of /this/ node, who is
-- talking to a peer node. For example, if this node is a client, then the peer
-- is a server, the outbound headers correspond to a request and the inbound
-- headers correspond to a response (see also 'InitiateSession').
--
-- We avoid referring to \"inputs\" or \"outputs\" here, but instead talk about
-- \"inbound\" or \"outbound\". When we are dealing with gRPC, \"inputs\" are
-- outbound for the client and inbound for the server, and \"outputs\" are
-- inbound for the client and outbound for the server.
class ( Show (InboundHeaders   sess)
      , Show (InboundTrailers  sess)
      , Show (InboundMsg       sess)
      , Show (OutboundHeaders  sess)
      , Show (OutboundTrailers sess)
      , Show (OutboundMsg      sess)
      ) => IsSession sess where
  data InboundHeaders  sess :: Type
  data OutboundHeaders sess :: Type

  type InboundTrailers  sess :: Type
  type OutboundTrailers sess :: Type

  type InboundMsg  sess :: Type
  type OutboundMsg sess :: Type

  -- | Parse trailers
  --
  -- NOTE: The a stream is determined after the initial set of headers, that
  -- set of headers can be regarded as both \"headers\" or \"trailers\", and
  -- we will accordingly parse them using /both/ 'parseTrailers' /and/
  -- 'parseResponseInfo' or 'parseRequestInfo' (depending on whether we are
  -- a client or a server).
  --
  -- TODO: If it turns out that this is too coarse, we will need to introduce
  -- a separate type variable that describes this "trailers only" case.
  parseTrailers :: sess -> [HTTP.Header] -> IO (InboundTrailers sess)

  -- | Build trailers
  buildTrailers :: sess -> OutboundTrailers sess -> IO [HTTP.Header]

  -- | Parse message
  parseMsg :: sess -> InboundHeaders  sess -> Parser (InboundMsg sess)

  -- | Build message
  buildMsg :: sess -> OutboundHeaders sess -> OutboundMsg sess -> Builder

-- | Initiate new session
--
-- A client node connects to a server, and initiates the request.
class IsSession sess => InitiateSession sess where
  -- | Build 'RequestInfo' for the server
  buildRequestInfo  :: sess -> OutboundHeaders sess -> IO RequestInfo

  -- | Parse 'ResponseInfo' from the server
  parseResponseInfo :: sess -> ResponseInfo -> IO (InboundHeaders sess)

-- | Accept session
--
-- A server node listens and accepts incoming requests from client nodes.
class IsSession sess => AcceptSession sess where
  -- | Parse 'RequestInfo' from the client
  parseRequestInfo  :: sess -> RequestInfo -> IO (InboundHeaders sess)

  -- | Build 'ResponseInfo' for the client
  buildResponseInfo :: sess -> OutboundHeaders sess -> IO ResponseInfo

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Misbehaving peer
--
-- Although this exception could in principle be caught, there is not much that
-- can be done to rectify the situation: probably this peer should just be
-- avoided (although perhaps one can hope that the problem was transient).
data PeerException =
    -- | Peer sent a malformed message (parser returned an error)
    PeerSentMalformedMessage String

    -- | Peer sent an incomplete message (parser did not consume all data)
  | PeerSentIncompleteMessage

    -- | HTTP request missing @:method@ pseudo-header
  | PeerMissingPseudoHeaderMethod

    -- | HTTP request missing @:path@ pseudo-header
  | PeerMissingPseudoHeaderPath

    -- | HTTP response missing @:status@ pseudo-header
  | PeerMissingPseudoHeaderStatus
  deriving stock (Show)
  deriving anyclass (Exception)
