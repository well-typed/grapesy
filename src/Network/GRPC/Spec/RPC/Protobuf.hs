{-# LANGUAGE OverloadedStrings #-}

-- | gRPC with Protobuf
module Network.GRPC.Spec.RPC.Protobuf (
    RPC(..)
  ) where

import Data.ByteString.Builder qualified as BS.Builder
import Data.ByteString.Char8 qualified as BS.Strict.C8
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

data RPC (s :: Type) (m :: Symbol) = RPC

instance (Typeable s, HasMethodImpl s m) => Show (RPC s m) where
  showsPrec p RPC = showParen (p >= appPrec1) $
        showString "RPC @"
      . showsPrec appPrec1 (typeRep (Proxy @s))
      . showSpace
      . showsPrec appPrec1 (symbolVal (Proxy @m))

instance ( Typeable           s
         , HasMethodImpl      s m
         , Show (MethodInput  s m)
         , Show (MethodOutput s m)
         ) => IsRPC (RPC s m) where
  type Input  (RPC s m) = MethodInput  s m
  type Output (RPC s m) = MethodOutput s m

  serializationFormat _ = "proto"
  serviceName         _ = BS.Strict.C8.pack $ concat [
                              symbolVal $ Proxy @(ServicePackage s)
                            , "."
                            , symbolVal $ Proxy @(ServiceName s)
                            ]
  methodName          _ = BS.Strict.C8.pack $
                              symbolVal $ Proxy @(MethodName s m)
  messageType         _ = BS.Strict.C8.pack $ Text.unpack $
                              -- 'messageName' returns a qualified name
                              Protobuf.messageName $ Proxy @(MethodInput s m)
  serializeInput      _ = BS.Builder.toLazyByteString . Protobuf.buildMessage
  deserializeOutput   _ = Protobuf.runParser parser . BS.Lazy.toStrict
    where
      parser :: Parser (MethodOutput s m)
      parser = do
          msg   <- Protobuf.parseMessage
          atEnd <- Protobuf.atEnd
          if atEnd then
            return msg
          else
            fail $ concat [
                Text.unpack $ Protobuf.messageName $ Proxy @(MethodOutput s m)
              , ": unconsumed bytes"
              ]
