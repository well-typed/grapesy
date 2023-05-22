{-# LANGUAGE OverloadedStrings #-}

-- | gRPC with Protobuf
module Network.GRPC.Spec.RPC.Protobuf (
    RPC(..)
  ) where

import Data.ByteString.Builder qualified as BS.Builder
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.Kind
import Data.ProtoLens qualified as Protobuf
import Data.ProtoLens.Encoding.Parser (Parser)
import Data.ProtoLens.Encoding.Parser qualified as Protobuf
import Data.ProtoLens.Service.Types as Protobuf
import Data.Proxy
import Data.Text qualified as Text
import Data.Typeable
import GHC.Show
import GHC.TypeLits

import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  The spec defines the following in Appendix A, "GRPC for Protobuf":

  > Service-Name → ?( {proto package name} "." ) {service name}
  > Message-Type → {fully qualified proto message name}
  > Content-Type → "application/grpc+proto"
-------------------------------------------------------------------------------}

data RPC (serv :: Type) (meth :: Symbol) = RPC

instance ( Typeable serv
         , HasMethodImpl serv meth
         ) => Show (RPC serv meth) where
  showsPrec p RPC = showParen (p >= appPrec1) $
        showString "RPC @"
      . showsPrec appPrec1 (typeRep (Proxy @serv))
      . showSpace
      . showsPrec appPrec1 (symbolVal (Proxy @meth))

instance ( Typeable           serv
         , HasMethodImpl      serv meth
         , Show (MethodInput  serv meth)
         , Show (MethodOutput serv meth)
         ) => IsRPC (RPC serv meth) where
  type Input  (RPC serv meth) = MethodInput  serv meth
  type Output (RPC serv meth) = MethodOutput serv meth

  serializationFormat _ = "proto"
  serviceName         _ = Text.pack $ concat [
                              symbolVal $ Proxy @(ServicePackage serv)
                            , "."
                            , symbolVal $ Proxy @(ServiceName serv)
                            ]
  methodName          _ = Text.pack $
                              symbolVal $ Proxy @(MethodName serv meth)
  messageType         _ = Protobuf.messageName $ Proxy @(MethodInput serv meth)
  serializeInput      _ = BS.Builder.toLazyByteString . Protobuf.buildMessage
  serializeOutput     _ = BS.Builder.toLazyByteString . Protobuf.buildMessage
  deserializeInput    _ = Protobuf.runParser parseMessage . BS.Lazy.toStrict
  deserializeOutput   _ = Protobuf.runParser parseMessage . BS.Lazy.toStrict

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
