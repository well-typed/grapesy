{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.RPC.Binary (BinaryRpc) where

import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Proxy
import Data.Text qualified as Text
import GHC.TypeLits

import Network.GRPC.Spec.CustomMetadata.NoMetadata
import Network.GRPC.Spec.CustomMetadata.Typed
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.StreamType

{-------------------------------------------------------------------------------
  Binary format
-------------------------------------------------------------------------------}

-- | Custom gRPC format
--
-- gRPC normally runs over Protobuf, but it does not have to. 'BinaryRpc'
-- provides an alternative format, which does not use
-- serialization/deserialization at all.
--
-- This is a non-standard format (which the gRPC specification explicitly
-- permits).
data BinaryRpc (serv :: Symbol) (meth :: Symbol)

type instance RequestMetadata          (BinaryRpc serv meth) = NoMetadata
type instance ResponseInitialMetadata  (BinaryRpc serv meth) = NoMetadata
type instance ResponseTrailingMetadata (BinaryRpc serv meth) = NoMetadata

instance ( KnownSymbol serv
         , KnownSymbol meth
         ) => IsRPC (BinaryRpc serv meth) where
  type Input  (BinaryRpc serv meth) = Lazy.ByteString
  type Output (BinaryRpc serv meth) = Lazy.ByteString

  rpcContentType _ = defaultRpcContentType "binary"
  rpcServiceName _ = Text.pack $ symbolVal (Proxy @serv)
  rpcMethodName  _ = Text.pack $ symbolVal (Proxy @meth)
  rpcMessageType _ = "bytestring"

instance ( KnownSymbol serv
         , KnownSymbol meth
         ) => SupportsClientRpc (BinaryRpc serv meth) where
  rpcSerializeInput    _ = id
  rpcDeserializeOutput _ = return

instance ( KnownSymbol serv
         , KnownSymbol meth
         ) => SupportsServerRpc (BinaryRpc serv meth) where
  rpcDeserializeInput _ = return
  rpcSerializeOutput  _ = id

-- | For the binary protocol we do not check communication protocols
instance SupportsStreamingType (BinaryRpc serv meth) styp
