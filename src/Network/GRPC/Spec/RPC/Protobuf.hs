{-# LANGUAGE OverloadedStrings #-}

-- | gRPC with Protobuf
module Network.GRPC.Spec.RPC.Protobuf (Protobuf) where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.Kind
import Data.ProtoLens qualified as Protobuf
import Data.ProtoLens.Encoding.Parser (Parser)
import Data.ProtoLens.Encoding.Parser qualified as Protobuf
import Data.ProtoLens.Service.Types as Protobuf
import Data.Proxy
import Data.Text qualified as Text
import GHC.TypeLits

import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.StreamType

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
  rpcMessageType _ = Protobuf.messageName  $ Proxy @(MethodInput serv meth)

instance ( HasMethodImpl      serv meth
         , Show (MethodInput  serv meth)
         , Show (MethodOutput serv meth)
         ) => SupportsClientRpc (Protobuf serv meth) where
  rpcSerializeInput    _ = Builder.toLazyByteString . Protobuf.buildMessage
  rpcDeserializeOutput _ = Protobuf.runParser parseMessage . BS.Lazy.toStrict

instance ( HasMethodImpl      serv meth
         , Show (MethodInput  serv meth)
         , Show (MethodOutput serv meth)
         ) => SupportsServerRpc (Protobuf serv meth) where
  rpcDeserializeInput _ = Protobuf.runParser parseMessage . BS.Lazy.toStrict
  rpcSerializeOutput  _ = Builder.toLazyByteString . Protobuf.buildMessage

instance styp ~ MethodStreamingType serv meth
      => SupportsStreamingType (Protobuf serv meth) styp

instance HasStreamingType (Protobuf serv meth) where
  type RpcStreamingType (Protobuf serv meth) = MethodStreamingType serv meth

parseMessage :: forall msg. Protobuf.Message msg => Parser msg
parseMessage = do
    msg   <- Protobuf.parseMessage
    atEnd <- Protobuf.atEnd
    if atEnd then
      return msg
    else
      fail $ concat [
          Text.unpack $ Protobuf.messageName $ Proxy @msg
        , ": unconsumed bytes"
        ]
