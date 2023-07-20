{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.RPC.Binary (BinaryRpc) where

import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Proxy
import Data.Text qualified as Text
import GHC.TypeLits

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

instance ( KnownSymbol serv
         , KnownSymbol meth
         ) => IsRPC (BinaryRpc serv meth) where
  type Input  (BinaryRpc serv meth) = Lazy.ByteString
  type Output (BinaryRpc serv meth) = Lazy.ByteString

  serializationFormat _ = "binary"
  serviceName         _ = Text.pack $ symbolVal (Proxy @serv)
  methodName          _ = Text.pack $ symbolVal (Proxy @meth)
  messageType         _ = "bytestring"
  serializeInput      _ = id
  serializeOutput     _ = id
  deserializeInput    _ = return
  deserializeOutput   _ = return

-- | For the binary protocol we do not check communication protocols
instance SupportsStreamingType (BinaryRpc serv meth) styp
