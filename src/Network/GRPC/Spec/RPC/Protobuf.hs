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
import Data.Typeable
import GHC.TypeLits

import Network.GRPC.Common.StreamType
import Network.GRPC.Spec.RPC

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

instance ( Typeable           serv
         , HasMethodImpl      serv meth
         , Show (MethodInput  serv meth)
         , Show (MethodOutput serv meth)
         ) => IsRPC (Protobuf serv meth) where
  type Input  (Protobuf serv meth) = MethodInput  serv meth
  type Output (Protobuf serv meth) = MethodOutput serv meth

  serializationFormat _ = "proto"
  serviceName         _ = Text.pack $ concat [
                              symbolVal $ Proxy @(ServicePackage serv)
                            , "."
                            , symbolVal $ Proxy @(ServiceName serv)
                            ]
  methodName          _ = Text.pack $
                              symbolVal $ Proxy @(MethodName serv meth)
  messageType         _ = Protobuf.messageName $ Proxy @(MethodInput serv meth)
  serializeInput      _ = Builder.toLazyByteString . Protobuf.buildMessage
  serializeOutput     _ = Builder.toLazyByteString . Protobuf.buildMessage
  deserializeInput    _ = Protobuf.runParser parseMessage . BS.Lazy.toStrict
  deserializeOutput   _ = Protobuf.runParser parseMessage . BS.Lazy.toStrict

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
