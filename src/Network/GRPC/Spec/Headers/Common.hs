-- | Functionality shared between requests and responses
--
-- The following headers are used both in requests and in responses:
--
-- * @Content-Type@
-- * @Message-Encoding@
-- * @Message-Accept-Encoding@
-- * @Custom-Metadata@ (see "Network.GRPC.Spec.CustomMetadata")
--
-- We also define 'MessageType' here, which is currently only used for request
-- headers, but at least morally speaking seems to apply the response just the
-- same.
--
-- Intended for unqualified import.
module Network.GRPC.Spec.Headers.Common (
    -- * Definition
    ContentType(..)
  , MessageType(..)
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.Default
import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  ContentType
-------------------------------------------------------------------------------}

-- | Content type
data ContentType =
    -- | The default content type for this RPC
    --
    -- This is given by 'rpcContentType', and is typically
    -- @application/grpc+format@, where @format@ is @proto@, @json@, .. (see
    -- also 'defaultRpcContentType').
    ContentTypeDefault

    -- | Override the content type
    --
    -- Depending on the choice of override, this may or may not be conform spec.
    -- See <https://datatracker.ietf.org/doc/html/rfc2045#section-5> for a spec
    -- of the Content-Type header; the gRPC spec however disallows most of what
    -- is technically allowed by this RPC.
  | ContentTypeOverride Strict.ByteString
  deriving stock (Show, Eq, Generic)

instance Default ContentType where
  def = ContentTypeDefault

{-------------------------------------------------------------------------------
  MessageType
-------------------------------------------------------------------------------}

-- | Message type
data MessageType =
    -- | Default message type for this RPC
    --
    -- This is given by 'rpcMessageType'. For the specific case Protobuf this
    -- is the fully qualified proto message name (and we currently omit the
    -- @grpc-message-type@ header altogether for JSON).
    MessageTypeDefault

    -- | Override the message type
  | MessageTypeOverride Strict.ByteString
  deriving stock (Show, Eq, Generic)

instance Default MessageType where
  def = MessageTypeDefault

