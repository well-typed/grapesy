{-# LANGUAGE OverloadedStrings #-}

-- | gRPC with Protobuf
module Network.GRPC.Spec.RPC.Protobuf (Protobuf) where

import Data.Kind
import Data.ProtoLens
import Data.ProtoLens.Service.Types
import Data.Proxy
import Data.Text qualified as Text
import GHC.TypeLits

import Network.GRPC.Spec.CustomMetadata.Typed
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.StreamType
import Network.GRPC.Util.Protobuf qualified as Protobuf

{-------------------------------------------------------------------------------
  The spec defines the following in Appendix A, "GRPC for Protobuf":

  > Service-Name → ?( {proto package name} "." ) {service name}
  > Message-Type → {fully qualified proto message name}
  > Content-Type → "application/grpc+proto"
-------------------------------------------------------------------------------}

-- | Protobuf RPC
--
-- This exists only as a type-level marker
data Protobuf (serv :: Type) (meth :: Symbol)

instance ( HasMethodImpl      serv meth
         , Show (MethodInput  serv meth)
         , Show (MethodOutput serv meth)

           -- Metadata constraints
         , Show (RequestMetadata (Protobuf serv meth))
         , Show (ResponseInitialMetadata (Protobuf serv meth))
         , Show (ResponseTrailingMetadata (Protobuf serv meth))
         ) => IsRPC (Protobuf serv meth) where
  type Input  (Protobuf serv meth) = MethodInput  serv meth
  type Output (Protobuf serv meth) = MethodOutput serv meth

  rpcContentType _ = defaultRpcContentType "proto"
  rpcServiceName _ = Text.pack $ concat [
                         symbolVal $ Proxy @(ServicePackage serv)
                       , "."
                       , symbolVal $ Proxy @(ServiceName serv)
                       ]
  rpcMethodName  _ = Text.pack . symbolVal $ Proxy @(MethodName  serv meth)
  rpcMessageType _ = messageName  $ Proxy @(MethodInput serv meth)

instance ( IsRPC (Protobuf serv meth)
         , HasMethodImpl serv meth

           -- Metadata constraints
         , BuildMetadata (RequestMetadata (Protobuf serv meth))
         , ParseMetadata (ResponseInitialMetadata (Protobuf serv meth))
         , ParseMetadata (ResponseTrailingMetadata (Protobuf serv meth))
         ) => SupportsClientRpc (Protobuf serv meth) where
  rpcSerializeInput    _ = Protobuf.buildLazy
  rpcDeserializeOutput _ = Protobuf.parseLazy

instance ( IsRPC (Protobuf serv meth)
         , HasMethodImpl serv meth

           -- Metadata constraints
         , ParseMetadata (RequestMetadata (Protobuf serv meth))
         , BuildMetadata (ResponseInitialMetadata (Protobuf serv meth))
         , StaticMetadata (ResponseTrailingMetadata (Protobuf serv meth))
         ) => SupportsServerRpc (Protobuf serv meth) where
  rpcDeserializeInput _ = Protobuf.parseLazy
  rpcSerializeOutput  _ = Protobuf.buildLazy

instance styp ~ MethodStreamingType serv meth
      => SupportsStreamingType (Protobuf serv meth) styp

instance HasStreamingType (Protobuf serv meth) where
  type RpcStreamingType (Protobuf serv meth) = MethodStreamingType serv meth

