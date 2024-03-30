{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.RPC (
    IsRPC(..)
  , SupportsServerRpc(..)
  , SupportsClientRpc(..)
  , defaultRpcContentType
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Kind
import Data.Text (Text)
import Data.Typeable
import GHC.Stack

import Network.GRPC.Spec.CustomMetadata.Typed

{-------------------------------------------------------------------------------
  RPC call
-------------------------------------------------------------------------------}

-- | Abstract definition of an RPC
--
-- Note on encoding: the gRPC specification does not say anything about text
-- encoding issues for paths (service names and method names) or message types.
-- We allow them to be arbitrary 'Text' here, and then use the gRPC defined
-- percent encoding (which it mandates for status messages).
--
-- TODO: We need to check interop with existing libraries to see if they all
-- agree on this.
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
  -- | Messages from the client to the server
  type Input rpc :: Type

  -- | Messages from the server to the client
  type Output rpc :: Type

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
  -- See <https://grpc.io/blog/grpc-with-json/> for a discussion of gRPC with
  -- JSON. TODO: We don't currently support this, but it should just be an
  -- alternative 'RPC' instance.
  --
  -- Note on terminology: throughout this codebase we avoid the terms "encoding"
  -- and "decoding", which can be ambiguous. Instead we use
  -- \"serialize\"\/\"deserialize\" and \"compress\"\/\"decompress\".
  rpcContentType :: Proxy rpc -> Strict.ByteString

  -- | Service name
  --
  -- For Protobuf, this is the fully qualified service name.
  rpcServiceName :: HasCallStack => Proxy rpc -> Text

  -- | Method name
  --
  -- For Protobuf, this is /just/ the method name (no qualifier required).
  rpcMethodName :: HasCallStack => Proxy rpc -> Text

  -- | Message type
  --
  -- For Protobuf, this is the fully qualified message type.
  rpcMessageType :: HasCallStack => Proxy rpc -> Text

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

