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
  , RawMetadata(..)
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

    -- ** User errors
  , Session.SendAfterFinal(..)
  , Session.RecvAfterFinal(..)

    -- * Convenience re-exports
  , Proxy(..)
  , Default(..)
  ) where

import Data.Default
import Data.Proxy
import Network.Socket (PortNumber)

import Control.Exception

import Network.GRPC.Common.HTTP2Settings
import Network.GRPC.Common.NextElem (NextElem(..))
import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Spec
import Network.GRPC.Util.Session qualified as Session
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

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Protocol exception
--
-- A protocol exception arises when the client and the server disagree on the
-- sequence of inputs and outputs exchanged. This agreement might be part of a
-- formal specification such as Protobuf, or it might be implicit in the
-- implementation of a specific RPC.
data ProtocolException rpc =
    -- | We expected an input but got none
    TooFewInputs

    -- | We received an input when we expected no more inputs
  | TooManyInputs (Input rpc)

    -- | We expected an output, but got trailers instead
  | TooFewOutputs (ResponseTrailingMetadata rpc)

    -- | We expected trailers, but got an output instead
  | TooManyOutputs (Output rpc)

deriving stock instance IsRPC rpc => Show (ProtocolException rpc)

-- | Existential wrapper around 'ProtocolException'
--
-- This makes it easier to catch these exceptions (without this, you'd have to
-- catch the exception for a /specific/ instance of @rpc@).
data SomeProtocolException where
    ProtocolException :: forall rpc.
         IsRPC rpc
      => ProtocolException rpc
      -> SomeProtocolException

deriving stock    instance Show SomeProtocolException
deriving anyclass instance Exception SomeProtocolException