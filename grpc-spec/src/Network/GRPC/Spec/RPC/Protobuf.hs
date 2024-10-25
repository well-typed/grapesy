{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE OverloadedStrings      #-}

-- | gRPC with Protobuf
module Network.GRPC.Spec.RPC.Protobuf (
    Protobuf
  , Proto(..)
  , getProto
  ) where

import Control.DeepSeq (NFData)
import Control.Lens hiding (lens)
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Int
import Data.Kind
import Data.Map (Map)
import Data.ProtoLens
import Data.ProtoLens.Field qualified as ProtoLens
import Data.ProtoLens.Service.Types
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Boxed (Vector)
import Data.Vector.Unboxed qualified as Unboxed (Vector)
import Data.Word
import GHC.Exts (Proxy#, proxy#)
import GHC.Records qualified as GHC
import GHC.Records.Compat qualified as GHC.Compat
import GHC.TypeLits

import Network.GRPC.Spec.CustomMetadata.Typed
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.StreamType
import Network.GRPC.Spec.Util.Protobuf qualified as Protobuf

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

type instance Input  (Protobuf serv meth) = Proto (MethodInput  serv meth)
type instance Output (Protobuf serv meth) = Proto (MethodOutput serv meth)

instance ( HasMethodImpl      serv meth

           -- Debugging
         , Show (MethodInput  serv meth)
         , Show (MethodOutput serv meth)

           -- Serialization
         , NFData (MethodInput  serv meth)
         , NFData (MethodOutput serv meth)

           -- Metadata constraints
         , Show (RequestMetadata (Protobuf serv meth))
         , Show (ResponseInitialMetadata (Protobuf serv meth))
         , Show (ResponseTrailingMetadata (Protobuf serv meth))
         ) => IsRPC (Protobuf serv meth) where
  rpcContentType _ = defaultRpcContentType "proto"
  rpcServiceName _ = BS.Char8.pack $ concat [
                         symbolVal $ Proxy @(ServicePackage serv)
                       , "."
                       , symbolVal $ Proxy @(ServiceName serv)
                       ]
  rpcMethodName  _ = BS.Char8.pack . symbolVal $
                       Proxy @(MethodName  serv meth)
  rpcMessageType _ = Just . BS.Char8.pack . Text.unpack . messageName $
                       Proxy @(MethodInput serv meth)

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

instance ( styp ~ MethodStreamingType serv meth
         , ValidStreamingType styp
         )
      => SupportsStreamingType (Protobuf serv meth) styp

instance ValidStreamingType (MethodStreamingType serv meth)
      => HasStreamingType (Protobuf serv meth) where
  type RpcStreamingType (Protobuf serv meth) = MethodStreamingType serv meth

{-------------------------------------------------------------------------------
  Wrapper around Protobuf messages
-------------------------------------------------------------------------------}

-- | Wrapper around Protobuf messages and Protobuf enums
--
-- Protobuf messages and enums behave differently to normal Haskell datatypes.
-- Fields in messages always have defaults, enums can have unknown values, etc.
-- We therefore mark them at the type-level with this t'Proto' wrapper. Most of
-- the time you can work with t'Proto' values as if the wrapper is not there,
-- because @Proto msg@ inherits 'Message' and @Data.ProtoLens.Field@
-- 'ProtoLens.HasField' instances from @msg@. For example, you can create a
-- 'Proto Point' value as
--
-- > p = defMessage
-- >       & #latitude  .~ ..
-- >       & #longitude .~ ..
--
-- and access fields /from/ such a value using
--
-- > p ^. #latitude
--
-- as per usual.
--
-- One advantage of the t'Proto' wrapper is that we can give blanket instances
-- for /all/ Protobuf messages; we use this to provide @GHC.Records@
-- 'GHC.HasField' and @GHC.Records.Compat@ 'GHC.Compat.HasField' instances.
-- This means that you can also use @OverloadedRecordDot@ to access fields
--
-- > p.latitude
--
-- or even @OverloadedRecordUpdate@ to set fields
--
-- > p{latitude = ..}
newtype Proto msg = Proto msg
  deriving stock (Show)
  deriving newtype (
      Eq
    , Ord
    , Bounded
    , Enum
    , FieldDefault
    , MessageEnum
    , NFData
    )

-- | Field accessor for t'Proto'
getProto :: Proto msg -> msg
-- Implementation note: This /must/ be defined separately from the 'Proto'
-- newtype, otherwise ghc won't let us define a 'GHC.HasField' instance.
getProto (Proto msg) = msg

instance Message msg => Message (Proto msg) where
  messageName             _ = messageName             (Proxy @msg)
  packedMessageDescriptor _ = packedMessageDescriptor (Proxy @msg)
  packedFileDescriptor    _ = packedFileDescriptor    (Proxy @msg)

  defMessage    = Proto defMessage
  buildMessage  = buildMessage . getProto
  parseMessage  = Proto <$> parseMessage
  unknownFields = _proto . unknownFields
  fieldsByTag   = protoFieldDescriptor <$> fieldsByTag

{-------------------------------------------------------------------------------
  ProtoLens.HasField instance for Proto

  We want to piggy-back on the generated HasField instance, but re-wrap nested
  messages in 'Proto' (but not primitive fields).

  See

  * <https://protobuf.dev/programming-guides/proto3/>
  * @Data.ProtoLens.Compiler.Generate.Field@ in @proto-lens-protoc@
-------------------------------------------------------------------------------}

-- | Field description
data FieldDesc = MkFieldDesc FieldLabel IsScalar

-- | Is this a scalar field or an enum/nested message?
data IsScalar = Scalar | NotScalar

-- | Field label
--
-- <https://protobuf.dev/programming-guides/proto3/#field-labels>
data FieldLabel =
    LabelImplicit
  | LabelOptional
  | LabelRepeated
  | LabelMap

type family Describe (a :: Type) :: FieldDesc where
  Describe (Maybe a)          = MkFieldDesc LabelOptional (CheckIsScalar a)
  Describe [a]                = MkFieldDesc LabelRepeated (CheckIsScalar a)
  Describe (Unboxed.Vector a) = MkFieldDesc LabelRepeated Scalar
  Describe (Boxed.Vector a)   = MkFieldDesc LabelRepeated (CheckIsScalar a)
  Describe (Map _ a)          = MkFieldDesc LabelMap      (CheckIsScalar a)
  Describe a                  = MkFieldDesc LabelImplicit (CheckIsScalar a)

type family CheckIsScalar (a :: Type) :: IsScalar where
  CheckIsScalar Bool              = Scalar
  CheckIsScalar Double            = Scalar
  CheckIsScalar Float             = Scalar
  CheckIsScalar Int32             = Scalar
  CheckIsScalar Int64             = Scalar
  CheckIsScalar Strict.ByteString = Scalar
  CheckIsScalar Text              = Scalar
  CheckIsScalar Word32            = Scalar
  CheckIsScalar Word64            = Scalar
  CheckIsScalar _                 = NotScalar

class RewrapField (desc :: FieldDesc) (x :: Type) (y :: Type) | desc x -> y where
  rewrapField :: Proxy# desc -> Lens' x y

instance RewrapField (MkFieldDesc label Scalar) a a where
  rewrapField _ = coerced

instance RewrapField (MkFieldDesc LabelImplicit NotScalar) a (Proto a) where
  rewrapField _ = coerced

instance RewrapField (MkFieldDesc LabelOptional NotScalar) (Maybe a) (Maybe (Proto a)) where
  rewrapField _ = coerced

instance RewrapField (MkFieldDesc LabelRepeated NotScalar) [a] [Proto a] where
  rewrapField _ = coerced

instance RewrapField (MkFieldDesc LabelRepeated NotScalar) (Boxed.Vector a) (Boxed.Vector (Proto a)) where
  rewrapField _ = coerced

instance RewrapField (MkFieldDesc LabelMap NotScalar) (Map k a) (Map k (Proto a)) where
  rewrapField _ = coerced

instance
       ( ProtoLens.HasField rec fldName x
       , RewrapField (Describe x) x fldType
       )
    => ProtoLens.HasField (Proto rec) fldName fldType where
  fieldOf p = _proto . ProtoLens.fieldOf p . rewrapField (proxy# @(Describe x))

instance ProtoLens.HasField (Proto rec) fldName fldType
      => GHC.HasField fldName (Proto rec) fldType where
  getField r = (
        r ^. ProtoLens.fieldOf (proxy# @fldName)
      )

instance ProtoLens.HasField (Proto rec) fldName fldType
      => GHC.Compat.HasField fldName (Proto rec) fldType where
  hasField r = (
        \a -> r & ProtoLens.fieldOf (proxy# @fldName) .~ a
      , r ^. ProtoLens.fieldOf (proxy# @fldName)
      )

{-------------------------------------------------------------------------------
  Internal auxiliary: Proto wrapper
-------------------------------------------------------------------------------}

_proto :: Iso' (Proto msg) msg
_proto = coerced

protoFieldDescriptor :: FieldDescriptor msg -> FieldDescriptor (Proto msg)
protoFieldDescriptor (FieldDescriptor name typ acc) =
    FieldDescriptor name typ (protoFieldAccessor acc)

protoFieldAccessor :: FieldAccessor msg value -> FieldAccessor (Proto msg) value
protoFieldAccessor (PlainField def lens) =
    PlainField def (coerced . lens)
protoFieldAccessor (OptionalField lens) =
    OptionalField (coerced . lens)
protoFieldAccessor (RepeatedField packing lens) =
    RepeatedField packing (coerced . lens)
protoFieldAccessor (MapField lensKey lensValue lensMap) =
    MapField lensKey lensValue (coerced . lensMap)
