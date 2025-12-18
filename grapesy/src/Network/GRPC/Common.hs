-- | General infrastructure used by both the client and the server
--
-- Intended for unqualified import.
module Network.GRPC.Common (
    -- * Abstraction over different serialization formats
    IsRPC(..)
  , Input
  , Output
  , SupportsClientRpc(..)
  , SupportsServerRpc(..)
  , defaultRpcContentType

    -- * Stream elements
    --
    -- We export only the main type here; for operations on 'StreamElem', see
    -- "Network.GRPC.Common.StreamElem" (intended for qualified import).
  , StreamElem(..)
  , NextElem(..)

    -- * Custom metadata
    --
    -- Clients can include custom metadata in the initial request to the server,
    -- and servers can include custom metadata boh in the initial response to
    -- the client as well as in the response trailers.
  , CustomMetadata(CustomMetadata)
  , customMetadataName
  , customMetadataValue
  , HeaderName(BinaryHeader, AsciiHeader)
  , NoMetadata(..)
    -- ** Typed
  , RequestMetadata
  , ResponseInitialMetadata
  , ResponseTrailingMetadata
  , ResponseMetadata(..)
    -- ** Serialization
  , BuildMetadata(..)
  , ParseMetadata(..)
  , StaticMetadata(..)

    -- * Configuration
  , SslKeyLog(..)

  -- * HTTP\/2 Settings
  , HTTP2Settings(..)

    -- * Defaults
  , defaultInsecurePort
  , defaultSecurePort
  , defaultHTTP2Settings

    -- * Message metadata
  , OutboundMeta(..)
  , InboundMeta(..)

    -- * Exceptions

    -- ** gRPC status and exceptions
  , GrpcStatus(..)
  , GrpcError(..)
  , GrpcException(..)
  , throwGrpcError

    -- ** Low-level
  , ProtocolException(..)
  , Session.ChannelDiscarded(..)
  , Session.PeerException(..)
  , SomeProtocolException(..)
  , UnexpectedMetadata(..)
  , InvalidHeaders(..)
  , InvalidHeader(..)
  , invalidHeaders
  , HandledSynthesized

    -- ** User errors
  , Session.SendAfterFinal(..)
  , Session.RecvAfterFinal(..)

    -- * Convenience re-exports
  , Proxy(..)
  , Default(..)
  ) where

import Network.GRPC.Util.Imports

import Network.Socket (PortNumber)

import Network.GRPC.Common.HTTP2Settings
import Network.GRPC.Common.ProtocolException
import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Util.Session.API qualified as Session
import Network.GRPC.Util.Session.Channel qualified as Session
import Network.GRPC.Util.TLS

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

-- | Default port number for insecure servers
--
-- By convention, @50051@ is often used as the default port for gRPC servers.
defaultInsecurePort :: PortNumber
defaultInsecurePort = 50051

-- | Default port number for secure servers (50052)
--
-- Unlike 'defaultInsecurePort', this is a @grapesy@ internal convention: we use
-- @50052@ as the defualt port for secure gRPC servers.
defaultSecurePort :: PortNumber
defaultSecurePort = 50052


