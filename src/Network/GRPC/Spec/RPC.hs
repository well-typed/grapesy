{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.RPC (
    IsRPC(..)
  , Input
  , Output
  , SupportsServerRpc(..)
  , SupportsClientRpc(..)
  , defaultRpcContentType
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Kind
import Data.Typeable
import GHC.Stack

import Network.GRPC.Spec.CustomMetadata.Typed

{-------------------------------------------------------------------------------
  RPC call
-------------------------------------------------------------------------------}

-- | Messages from the client to the server
type family Input (rpc :: k) :: Type

-- | Messages from the server to the client
type family Output (rpc :: k) :: Type

-- | Abstract definition of an RPC
--
-- Note on encoding: the gRPC specification does not say anything about text
-- encoding issues for paths (service names and method names) or message types.
-- The Protobuf compiler (by far the most common instantation of gRPC) does not
-- allow for non-ASCII character at all ("interpreting non ascii codepoint").
-- We therefore punt on the encoding issue here, and use bytestrings. /If/
-- applications want to use non-ASCII characters, they can choose their own
-- encoding.
class ( -- Debug constraints
        --
        -- For debugging it is useful when we have 'Show' instances in scope.
        -- This is not that strong a requirement; after all, we must be able
        -- to serialize inputs and deserialize outputs, so they must also be
        -- 'Show'able.
        Show (Input rpc)
      , Show (Output rpc)
      , Show (RequestMetadata rpc)
      , Show (ResponseInitialMetadata rpc)
      , Show (ResponseTrailingMetadata rpc)
      ) => IsRPC (rpc :: k) where
  -- | Content-type
  --
  -- gRPC is agnostic to the message format; the spec defines the @Content-Type@
  -- header as
  --
  -- > Content-Type →
  -- >   "content-type"
  -- >   "application/grpc"
  -- >   [("+proto" / "+json" / {custom})]
  --
  -- 'defaultRpcContentType' can be used in the case that the format (such as
  -- @proto@) is known.
  --
  -- Note on terminology: throughout this codebase we avoid the terms "encoding"
  -- and "decoding", which can be ambiguous. Instead we use
  -- \"serialize\"\/\"deserialize\" and \"compress\"\/\"decompress\".
  rpcContentType :: Proxy rpc -> Strict.ByteString

  -- | Service name
  --
  -- For Protobuf, this is the fully qualified service name.
  rpcServiceName :: HasCallStack => Proxy rpc -> Strict.ByteString

  -- | Method name
  --
  -- For Protobuf, this is /just/ the method name (no qualifier required).
  rpcMethodName :: HasCallStack => Proxy rpc -> Strict.ByteString

  -- | Message type, if specified
  --
  -- This is used to set the (optional) @grpc-message-type@ header.
  -- For Protobuf, this is the fully qualified message type.
  rpcMessageType :: HasCallStack => Proxy rpc -> Maybe Strict.ByteString

-- | Default content type string
--
-- This is equal to @"application/grpc+format@ for some @format@ such as
-- @proto@ or @json@. See also 'rpcContentType'.
defaultRpcContentType :: Strict.ByteString -> Strict.ByteString
defaultRpcContentType format = "application/grpc+" <> format

class ( IsRPC rpc

        -- Serialization
      , BuildMetadata (RequestMetadata rpc)
      , ParseMetadata (ResponseInitialMetadata rpc)
      , ParseMetadata (ResponseTrailingMetadata rpc)
      ) => SupportsClientRpc rpc where

  -- | Serialize RPC input
  --
  -- We don't ask for a builder here, but instead ask for the complete
  -- serialized form. gRPC insists that individual messages are length prefixed,
  -- so we /must/ compute the full serialization in memory before we can send
  -- anything.
  --
  -- We use the terms \"serialize\" and \"deserialize\" here, and
  -- \"compress\"/\"decompress\" for compression here, rather than
  -- \"encode\"/\"decode\", which could refer to either process.
  rpcSerializeInput :: Proxy rpc -> Input rpc -> Lazy.ByteString

  -- | Deserialize RPC output
  --
  -- Discussion of 'deserializeInput' applies here, also.
  rpcDeserializeOutput ::
       Proxy rpc
    -> Lazy.ByteString
    -> Either String (Output rpc)

class ( IsRPC rpc

        -- Serialization
      , ParseMetadata (RequestMetadata rpc)
      , BuildMetadata (ResponseInitialMetadata rpc)
      , StaticMetadata (ResponseTrailingMetadata rpc)
      ) => SupportsServerRpc rpc where

  -- | Deserialize RPC input
  --
  -- This function does not have to deal with compression or length prefixes,
  -- and can assume fully consume the given bytestring (if there are unconsumed
  -- bytes, this should be considered a parse failure).
  rpcDeserializeInput ::
       Proxy rpc
    -> Lazy.ByteString
    -> Either String (Input rpc)

  -- | Serialize RPC output
  rpcSerializeOutput :: Proxy rpc -> Output rpc -> Lazy.ByteString

