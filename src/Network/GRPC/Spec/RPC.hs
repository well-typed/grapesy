module Network.GRPC.Spec.RPC (
    IsRPC(..)
  , SomeRPC(..)
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Kind
import Data.Text (Text)
import Data.Typeable

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
class ( Typeable rpc -- for use in exceptions

        -- Debug constraints
        --
        -- For debugging it is useful when we have 'Show' instances in scope.
        -- This is not that strong a requirement; after all, we must be able
        -- to serialize inputs and deserialize outputs, so they must also be
        -- 'Show'able.
      , Show (Input rpc)
      , Show (Output rpc)
      ) => IsRPC (rpc :: Type) where
  -- | Messages from the client to the server
  type Input rpc :: Type

  -- | Messages from the server to the client
  type Output rpc :: Type

  -- | Message format
  --
  -- gRPC is agnostic to the message format; the spec defines the @Content-Type@
  -- header as
  --
  -- > Content-Type →
  -- >   "content-type"
  -- >   "application/grpc"
  -- >   [("+proto" / "+json" / {custom})]
  --
  -- 'serializationFormat' should return the last part (e.g., @"proto"@).
  --
  -- See <https://grpc.io/blog/grpc-with-json/> for a discussion of gRPC with
  -- JSON (we don't currently support this, but it should just be an alternative
  -- 'RPC' instance).
  --
  -- Note on terminology: throughout this codebase we avoid the terms "encoding"
  -- and "decoding", which can be ambiguous. Instead we use
  -- \"serialize\"/\"deserialize\" and \"compress\"/\"decompress\".
  serializationFormat :: Proxy rpc -> Strict.ByteString

  -- | Service name
  --
  -- For Protobuf, this is the fully qualified service name.
  serviceName :: Proxy rpc -> Text

  -- | Method name
  --
  -- For Protobuf, this is /just/ the method name (no qualifier required).
  methodName :: Proxy rpc -> Text

  -- | Message type
  --
  -- For Protobuf, this is the fully qualified message type.
  messageType :: Proxy rpc -> Text

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
  serializeInput :: Proxy rpc -> Input rpc -> Lazy.ByteString

  -- | Serialize RPC output
  serializeOutput :: Proxy rpc -> Output rpc -> Lazy.ByteString

  -- | Deserialize RPC input
  --
  -- This function does not have to deal with compression or length prefixes,
  -- and can assume fully consume the given bytestring (if there are unconsumed
  -- bytes, this should be considered a parse failure).
  deserializeInput :: Proxy rpc -> Lazy.ByteString -> Either String (Input rpc)

  -- | Deserialize RPC output
  --
  -- Discussion of 'deserializeInput' applies here, also.
  deserializeOutput :: Proxy rpc -> Lazy.ByteString -> Either String (Output rpc)

-- | Existential RPC
data SomeRPC f where
  SomeRPC :: forall f rpc. IsRPC rpc => f rpc -> SomeRPC f

deriving instance (forall rpc. IsRPC rpc => Show (f rpc)) => Show (SomeRPC f)