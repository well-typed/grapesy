-- | General infrastructure used by both the client and the server
--
-- Intended for unqualified import.
module Network.GRPC.Common (
    -- * Abstraction over different serialization formats
    IsRPC(..)

    -- * Stream elements
    --
    -- We export only the main type here; for operations on 'StreamElem', see
    -- "Network.GRPC.Common.StreamElem" (intended for qualified import).
  , StreamElem(..)

    -- * Custom metadata
    --
    -- Clients can include custom metadata in the initial request to the server,
    -- and servers can include custom metadata boh in the initial response to
    -- the client as well as in the response trailers.
  , CustomMetadata(..)
  , HeaderName(HeaderName)
  , AsciiValue(AsciiValue)
  , BinaryValue(..)
  , NoMetadata(..)
  , customHeaderName

    -- * Configuration
  , SslKeyLog(..)

    -- * Exceptions
  , GrpcException(..)
  , GrpcError(..)
  , ProtocolException(..)
  , SomeProtocolException(..)

    -- ** Low-level
  , Session.ChannelDiscarded(..)
  , Session.PeerException(..)
  ) where

import Control.Exception

import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Spec
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Util.TLS

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
  | TooFewOutputs [CustomMetadata]

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