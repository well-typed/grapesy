{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.RPC.Raw (RawRpc) where

import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Proxy
import Data.Text qualified as Text
import GHC.TypeLits

import Network.GRPC.Spec.CustomMetadata.Typed
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.StreamType

{-------------------------------------------------------------------------------
  Raw format
-------------------------------------------------------------------------------}

-- | Custom gRPC format
--
-- Normally @gRPC@ runs over Protobuf, but it does not have to. 'RawRpc' provides
-- an alternative format, which does not use serialization/deserialization at
-- all, just using raw bytestrings for messages. This is a non-standard format
-- (which the gRPC specification explicitly permits).
--
-- An example use case is provided by the various @.Binary@ modules in
-- @grapesy@, which use Haskell's 'Data.Binary.Binary' encoding.
data RawRpc (serv :: Symbol) (meth :: Symbol)

type instance Input  (RawRpc serv meth) = Lazy.ByteString
type instance Output (RawRpc serv meth) = Lazy.ByteString

instance ( KnownSymbol serv
         , KnownSymbol meth

           -- Metadata constraints
         , Show (RequestMetadata (RawRpc serv meth))
         , Show (ResponseInitialMetadata (RawRpc serv meth))
         , Show (ResponseTrailingMetadata (RawRpc serv meth))
         ) => IsRPC (RawRpc serv meth) where
  rpcContentType _ = defaultRpcContentType "grapesy-raw"
  rpcServiceName _ = Text.pack $ symbolVal (Proxy @serv)
  rpcMethodName  _ = Text.pack $ symbolVal (Proxy @meth)
  rpcMessageType _ = "bytestring"

instance ( IsRPC (RawRpc serv meth)

           -- Metadata constraints
         , BuildMetadata (RequestMetadata (RawRpc serv meth))
         , ParseMetadata (ResponseInitialMetadata (RawRpc serv meth))
         , ParseMetadata (ResponseTrailingMetadata (RawRpc serv meth))
         ) => SupportsClientRpc (RawRpc serv meth) where
  rpcSerializeInput    _ = id
  rpcDeserializeOutput _ = return

instance ( IsRPC (RawRpc serv meth)

           -- Metadata constraints
         , ParseMetadata (RequestMetadata (RawRpc serv meth))
         , BuildMetadata (ResponseInitialMetadata (RawRpc serv meth))
         , StaticMetadata (ResponseTrailingMetadata (RawRpc serv meth))
         ) => SupportsServerRpc (RawRpc serv meth) where
  rpcDeserializeInput _ = return
  rpcSerializeOutput  _ = id

-- | For the raw protocol we do not check communication protocols
instance ValidStreamingType styp
      => SupportsStreamingType (RawRpc serv meth) styp
