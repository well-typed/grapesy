module Network.GRPC.Util.Session.API (
    -- * Preliminaries
    RequestInfo(..)
  , ResponseInfo(..)
    -- * Main definitions
  , DataFlow(..)
  , FlowStart(..)
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

-- | Flow of data in a session
--
-- This describes the flow of data in /one/ direction. The normal flow of data
-- is as follows:
--
-- 1. (Proper) Headers
-- 2. Messages
-- 3. Trailers
--
-- However, in the case that there /are/ no messages, this whole thing collapses
-- into a single 'TrailersOnly'. We need to treat this case separately, because
--
-- * It looks different on the wire: in the regular case, we will have /two/
--   HTTP @Headers@ frames, but in the Trailers-Only case, we only have one.
-- * Applications may in turn treat the Trailers-Only case special, using a
--   different set of headers (specifically, this is the case for gRPC).
--
-- To avoid confusion, we refer to the trailers in the non-Trailers-Only case
-- as "proper" trailers.
class ( Show (Headers        flow)
      , Show (Message        flow)
      , Show (ProperTrailers flow)
      , Show (TrailersOnly   flow)
      ) => DataFlow flow where
  data Headers        flow :: Type
  type Message        flow :: Type
  type ProperTrailers flow :: Type
  type TrailersOnly   flow :: Type

-- | Start of data flow
--
-- See 'DataFlow' for discussion.
data FlowStart flow =
    FlowStartRegular      (Headers      flow)
  | FlowStartTrailersOnly (TrailersOnly flow)

deriving instance DataFlow flow => Show (FlowStart flow)

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
class ( DataFlow (Inbound  sess)
      , DataFlow (Outbound sess)
      ) => IsSession sess where
  type Inbound  sess :: Type
  type Outbound sess :: Type

  -- | Parse proper trailers
  parseProperTrailers ::
       sess
    -> [HTTP.Header] -> IO (ProperTrailers (Inbound sess))

  -- | Build proper trailers
  buildProperTrailers ::
       sess
    -> ProperTrailers (Outbound sess) -> [HTTP.Header]

  -- | Parse message
  parseMsg ::
       sess
    -> Headers (Inbound sess)
    -> Parser (Message (Inbound sess))

  -- | Build message
  buildMsg ::
       sess
    -> Headers (Outbound sess)
    -> Message (Outbound sess) -> Builder

-- | Initiate new session
--
-- A client node connects to a server, and initiates the request.
class IsSession sess => InitiateSession sess where
  -- | Build 'RequestInfo' for the server
  buildRequestInfo ::
       sess
    -> FlowStart (Outbound sess) -> RequestInfo

  -- | Parse 'ResponseInfo' from the server, regular case
  --
  -- See 'parseResponseTrailersOnly' for the Trailers-Only case.
  parseResponseRegular ::
       sess
    -> ResponseInfo -> IO (Headers (Inbound sess))

  -- | Parse 'ResponseInfo' from the server, Trailers-Only case
  parseResponseTrailersOnly ::
       sess
    -> ResponseInfo -> IO (TrailersOnly (Inbound sess))

-- | Accept session
--
-- A server node listens and accepts incoming requests from client nodes.
class IsSession sess => AcceptSession sess where
  -- | Parse 'RequestInfo' from the client, regular case
  --
  -- See 'parseRequestTrailersOnly' for the Trailers-Only case.
  parseRequestRegular ::
       sess
    -> RequestInfo -> IO (Headers (Inbound sess))

  --  | Parse 'RequestInfo' from the client, Trailers-Only case
  parseRequestTrailersOnly ::
       sess
    -> RequestInfo -> IO (TrailersOnly (Inbound sess))

  -- | Build 'ResponseInfo' for the client
  buildResponseInfo ::
       sess
    -> FlowStart (Outbound sess)
    -> ResponseInfo

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
