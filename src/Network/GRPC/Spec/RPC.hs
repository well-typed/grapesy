module Network.GRPC.Spec.RPC (
    IsRPC(..)
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Kind

{-------------------------------------------------------------------------------
  RPC call
-------------------------------------------------------------------------------}

class ( -- Debug constraints
        --
        -- For debugging it is useful when we have 'Show' instances in scope.
        -- This is not that strong a requirement; after all, we must be able
        -- to serialize inputs and deserialize outputs, so they must also be
        -- 'Show'able.
        Show rpc
      , Show (Input rpc)
      , Show (Output rpc)
      ) => IsRPC rpc where
  -- Messages from the client to the server
  type Input rpc :: Type

  -- Messages from the server to the client
  type Output rpc :: Type

  -- | Content encoding
  --
  -- gRPC is encoding agnostic; the spec defines the @Content-Type@ header as
  --
  -- > Content-Type →
  -- >   "content-type"
  -- >   "application/grpc"
  -- >   [("+proto" / "+json" / {custom})]
  --
  -- 'contentEncoding' should return the encoding part (e.g., @"proto"@).
  --
  -- See <https://grpc.io/blog/grpc-with-json/> for a discussion of gRPC with
  -- JSON (we don't currently support this, but it should just be an alternative
  -- 'RPC' instance).
  contentEncoding :: rpc -> Strict.ByteString

  -- | Service name
  --
  -- For Protobuf, this is the fully qualified service name.
  serviceName :: rpc -> Strict.ByteString

  -- | Method name
  --
  -- For Protobuf, this is /just/ the method name (no qualifier required).
  methodName :: rpc -> Strict.ByteString

  -- | Message type
  --
  -- For Protobuf, this is the fully qualified message type.
  messageType :: rpc -> Strict.ByteString

  -- | Serialize messages to the server
  --
  -- We don't ask for a builder here, but instead ask for the complete
  -- serialized form. gRPC insists that individual messages are length prefixed,
  -- so we /must/ compute the full serialization in memory before we can send
  -- anything.
  --
  -- We use the terms \"serialize\" and \"deserialize\" here, and
  -- \"compress\"/\"decompress\" for compression here, rather than
  -- \"encode\"/\"decode\", which could refer to either process.
  serializeInput :: rpc -> Input rpc -> Lazy.ByteString

  -- | Deserialize message
  --
  -- This function does not have to deal with compression or length prefixes,
  -- and can assume fully consume the given bytestring (if there are unconsumed
  -- bytes, this should be considered a parse failure).
  deserializeOutput :: rpc -> Lazy.ByteString -> Either String (Output rpc)
