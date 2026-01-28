{-# OPTIONS_GHC -Wno-prepositive-qualified-module -Wno-identities #-}
{- This file was auto-generated from spec.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Spec (
        ExampleService(..), AnotherMessage(), Empty(), ExampleEnum(..),
        ExampleEnum(), ExampleEnum'UnrecognizedValue, ExampleMessage(),
        ExampleMessage'ExampleOneOf(..), _ExampleMessage'OneofScalar01,
        _ExampleMessage'OneofScalar02, _ExampleMessage'OneofScalar03,
        _ExampleMessage'OneofScalar04, _ExampleMessage'OneofScalar05,
        _ExampleMessage'OneofScalar06, _ExampleMessage'OneofScalar07,
        _ExampleMessage'OneofScalar08, _ExampleMessage'OneofScalar09,
        _ExampleMessage'OneofScalar10, _ExampleMessage'OneofScalar11,
        _ExampleMessage'OneofScalar12, _ExampleMessage'OneofScalar13,
        _ExampleMessage'OneofScalar14, _ExampleMessage'OneofScalar15,
        _ExampleMessage'OneofAnother, _ExampleMessage'OneofNested,
        _ExampleMessage'OneofEnum, ExampleMessage'MapAnotherEntry(),
        ExampleMessage'MapEnumEntry(), ExampleMessage'MapNestedEntry(),
        ExampleMessage'MapScalar01Entry(),
        ExampleMessage'MapScalar02Entry(),
        ExampleMessage'MapScalar03Entry(),
        ExampleMessage'MapScalar04Entry(),
        ExampleMessage'MapScalar05Entry(),
        ExampleMessage'MapScalar06Entry(),
        ExampleMessage'MapScalar07Entry(),
        ExampleMessage'MapScalar08Entry(),
        ExampleMessage'MapScalar09Entry(),
        ExampleMessage'MapScalar10Entry(),
        ExampleMessage'MapScalar11Entry(),
        ExampleMessage'MapScalar12Entry(),
        ExampleMessage'MapScalar13Entry(),
        ExampleMessage'MapScalar14Entry(),
        ExampleMessage'MapScalar15Entry(), ExampleMessage'NestedMessage()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
{- | Fields :
      -}
data AnotherMessage
  = AnotherMessage'_constructor {_AnotherMessage'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AnotherMessage where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message AnotherMessage where
  messageName _ = Data.Text.pack "AnotherMessage"
  packedMessageDescriptor _
    = "\n\
      \\SOAnotherMessage"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AnotherMessage'_unknownFields
        (\ x__ y__ -> x__ {_AnotherMessage'_unknownFields = y__})
  defMessage
    = AnotherMessage'_constructor {_AnotherMessage'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AnotherMessage
          -> Data.ProtoLens.Encoding.Bytes.Parser AnotherMessage
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "AnotherMessage"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData AnotherMessage where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_AnotherMessage'_unknownFields x__) ()
{- | Fields :
      -}
data Empty
  = Empty'_constructor {_Empty'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Empty where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message Empty where
  messageName _ = Data.Text.pack "Empty"
  packedMessageDescriptor _
    = "\n\
      \\ENQEmpty"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Empty'_unknownFields
        (\ x__ y__ -> x__ {_Empty'_unknownFields = y__})
  defMessage = Empty'_constructor {_Empty'_unknownFields = []}
  parseMessage
    = let
        loop :: Empty -> Data.ProtoLens.Encoding.Bytes.Parser Empty
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Empty"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData Empty where
  rnf
    = \ x__ -> Control.DeepSeq.deepseq (_Empty'_unknownFields x__) ()
newtype ExampleEnum'UnrecognizedValue
  = ExampleEnum'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data ExampleEnum
  = ENUM_A |
    ENUM_B |
    ENUM_C |
    ExampleEnum'Unrecognized !ExampleEnum'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum ExampleEnum where
  maybeToEnum 0 = Prelude.Just ENUM_A
  maybeToEnum 1 = Prelude.Just ENUM_B
  maybeToEnum 2 = Prelude.Just ENUM_C
  maybeToEnum k
    = Prelude.Just
        (ExampleEnum'Unrecognized
           (ExampleEnum'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum ENUM_A = "ENUM_A"
  showEnum ENUM_B = "ENUM_B"
  showEnum ENUM_C = "ENUM_C"
  showEnum
    (ExampleEnum'Unrecognized (ExampleEnum'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "ENUM_A" = Prelude.Just ENUM_A
    | (Prelude.==) k "ENUM_B" = Prelude.Just ENUM_B
    | (Prelude.==) k "ENUM_C" = Prelude.Just ENUM_C
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded ExampleEnum where
  minBound = ENUM_A
  maxBound = ENUM_C
instance Prelude.Enum ExampleEnum where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum ExampleEnum: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum ENUM_A = 0
  fromEnum ENUM_B = 1
  fromEnum ENUM_C = 2
  fromEnum
    (ExampleEnum'Unrecognized (ExampleEnum'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ ENUM_C
    = Prelude.error
        "ExampleEnum.succ: bad argument ENUM_C. This value would be out of bounds."
  succ ENUM_A = ENUM_B
  succ ENUM_B = ENUM_C
  succ (ExampleEnum'Unrecognized _)
    = Prelude.error
        "ExampleEnum.succ: bad argument: unrecognized value"
  pred ENUM_A
    = Prelude.error
        "ExampleEnum.pred: bad argument ENUM_A. This value would be out of bounds."
  pred ENUM_B = ENUM_A
  pred ENUM_C = ENUM_B
  pred (ExampleEnum'Unrecognized _)
    = Prelude.error
        "ExampleEnum.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault ExampleEnum where
  fieldDefault = ENUM_A
instance Control.DeepSeq.NFData ExampleEnum where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Spec_Fields.defaultScalar01' @:: Lens' ExampleMessage Prelude.Double@
         * 'Proto.Spec_Fields.defaultScalar02' @:: Lens' ExampleMessage Prelude.Float@
         * 'Proto.Spec_Fields.defaultScalar03' @:: Lens' ExampleMessage Data.Int.Int32@
         * 'Proto.Spec_Fields.defaultScalar04' @:: Lens' ExampleMessage Data.Int.Int64@
         * 'Proto.Spec_Fields.defaultScalar05' @:: Lens' ExampleMessage Data.Word.Word32@
         * 'Proto.Spec_Fields.defaultScalar06' @:: Lens' ExampleMessage Data.Word.Word64@
         * 'Proto.Spec_Fields.defaultScalar07' @:: Lens' ExampleMessage Data.Int.Int32@
         * 'Proto.Spec_Fields.defaultScalar08' @:: Lens' ExampleMessage Data.Int.Int64@
         * 'Proto.Spec_Fields.defaultScalar09' @:: Lens' ExampleMessage Data.Word.Word32@
         * 'Proto.Spec_Fields.defaultScalar10' @:: Lens' ExampleMessage Data.Word.Word64@
         * 'Proto.Spec_Fields.defaultScalar11' @:: Lens' ExampleMessage Data.Int.Int32@
         * 'Proto.Spec_Fields.defaultScalar12' @:: Lens' ExampleMessage Data.Int.Int64@
         * 'Proto.Spec_Fields.defaultScalar13' @:: Lens' ExampleMessage Prelude.Bool@
         * 'Proto.Spec_Fields.defaultScalar14' @:: Lens' ExampleMessage Data.Text.Text@
         * 'Proto.Spec_Fields.defaultScalar15' @:: Lens' ExampleMessage Data.ByteString.ByteString@
         * 'Proto.Spec_Fields.defaultAnother' @:: Lens' ExampleMessage AnotherMessage@
         * 'Proto.Spec_Fields.maybe'defaultAnother' @:: Lens' ExampleMessage (Prelude.Maybe AnotherMessage)@
         * 'Proto.Spec_Fields.defaultNested' @:: Lens' ExampleMessage ExampleMessage'NestedMessage@
         * 'Proto.Spec_Fields.maybe'defaultNested' @:: Lens' ExampleMessage (Prelude.Maybe ExampleMessage'NestedMessage)@
         * 'Proto.Spec_Fields.defaultEnum' @:: Lens' ExampleMessage ExampleEnum@
         * 'Proto.Spec_Fields.optionalScalar01' @:: Lens' ExampleMessage Prelude.Double@
         * 'Proto.Spec_Fields.maybe'optionalScalar01' @:: Lens' ExampleMessage (Prelude.Maybe Prelude.Double)@
         * 'Proto.Spec_Fields.optionalScalar02' @:: Lens' ExampleMessage Prelude.Float@
         * 'Proto.Spec_Fields.maybe'optionalScalar02' @:: Lens' ExampleMessage (Prelude.Maybe Prelude.Float)@
         * 'Proto.Spec_Fields.optionalScalar03' @:: Lens' ExampleMessage Data.Int.Int32@
         * 'Proto.Spec_Fields.maybe'optionalScalar03' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int32)@
         * 'Proto.Spec_Fields.optionalScalar04' @:: Lens' ExampleMessage Data.Int.Int64@
         * 'Proto.Spec_Fields.maybe'optionalScalar04' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int64)@
         * 'Proto.Spec_Fields.optionalScalar05' @:: Lens' ExampleMessage Data.Word.Word32@
         * 'Proto.Spec_Fields.maybe'optionalScalar05' @:: Lens' ExampleMessage (Prelude.Maybe Data.Word.Word32)@
         * 'Proto.Spec_Fields.optionalScalar06' @:: Lens' ExampleMessage Data.Word.Word64@
         * 'Proto.Spec_Fields.maybe'optionalScalar06' @:: Lens' ExampleMessage (Prelude.Maybe Data.Word.Word64)@
         * 'Proto.Spec_Fields.optionalScalar07' @:: Lens' ExampleMessage Data.Int.Int32@
         * 'Proto.Spec_Fields.maybe'optionalScalar07' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int32)@
         * 'Proto.Spec_Fields.optionalScalar08' @:: Lens' ExampleMessage Data.Int.Int64@
         * 'Proto.Spec_Fields.maybe'optionalScalar08' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int64)@
         * 'Proto.Spec_Fields.optionalScalar09' @:: Lens' ExampleMessage Data.Word.Word32@
         * 'Proto.Spec_Fields.maybe'optionalScalar09' @:: Lens' ExampleMessage (Prelude.Maybe Data.Word.Word32)@
         * 'Proto.Spec_Fields.optionalScalar10' @:: Lens' ExampleMessage Data.Word.Word64@
         * 'Proto.Spec_Fields.maybe'optionalScalar10' @:: Lens' ExampleMessage (Prelude.Maybe Data.Word.Word64)@
         * 'Proto.Spec_Fields.optionalScalar11' @:: Lens' ExampleMessage Data.Int.Int32@
         * 'Proto.Spec_Fields.maybe'optionalScalar11' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int32)@
         * 'Proto.Spec_Fields.optionalScalar12' @:: Lens' ExampleMessage Data.Int.Int64@
         * 'Proto.Spec_Fields.maybe'optionalScalar12' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int64)@
         * 'Proto.Spec_Fields.optionalScalar13' @:: Lens' ExampleMessage Prelude.Bool@
         * 'Proto.Spec_Fields.maybe'optionalScalar13' @:: Lens' ExampleMessage (Prelude.Maybe Prelude.Bool)@
         * 'Proto.Spec_Fields.optionalScalar14' @:: Lens' ExampleMessage Data.Text.Text@
         * 'Proto.Spec_Fields.maybe'optionalScalar14' @:: Lens' ExampleMessage (Prelude.Maybe Data.Text.Text)@
         * 'Proto.Spec_Fields.optionalScalar15' @:: Lens' ExampleMessage Data.ByteString.ByteString@
         * 'Proto.Spec_Fields.maybe'optionalScalar15' @:: Lens' ExampleMessage (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Spec_Fields.optionalAnother' @:: Lens' ExampleMessage AnotherMessage@
         * 'Proto.Spec_Fields.maybe'optionalAnother' @:: Lens' ExampleMessage (Prelude.Maybe AnotherMessage)@
         * 'Proto.Spec_Fields.optionalNested' @:: Lens' ExampleMessage ExampleMessage'NestedMessage@
         * 'Proto.Spec_Fields.maybe'optionalNested' @:: Lens' ExampleMessage (Prelude.Maybe ExampleMessage'NestedMessage)@
         * 'Proto.Spec_Fields.optionalEnum' @:: Lens' ExampleMessage ExampleEnum@
         * 'Proto.Spec_Fields.maybe'optionalEnum' @:: Lens' ExampleMessage (Prelude.Maybe ExampleEnum)@
         * 'Proto.Spec_Fields.repeatedScalar01' @:: Lens' ExampleMessage [Prelude.Double]@
         * 'Proto.Spec_Fields.vec'repeatedScalar01' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Prelude.Double)@
         * 'Proto.Spec_Fields.repeatedScalar02' @:: Lens' ExampleMessage [Prelude.Float]@
         * 'Proto.Spec_Fields.vec'repeatedScalar02' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Prelude.Float)@
         * 'Proto.Spec_Fields.repeatedScalar03' @:: Lens' ExampleMessage [Data.Int.Int32]@
         * 'Proto.Spec_Fields.vec'repeatedScalar03' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Data.Int.Int32)@
         * 'Proto.Spec_Fields.repeatedScalar04' @:: Lens' ExampleMessage [Data.Int.Int64]@
         * 'Proto.Spec_Fields.vec'repeatedScalar04' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Data.Int.Int64)@
         * 'Proto.Spec_Fields.repeatedScalar05' @:: Lens' ExampleMessage [Data.Word.Word32]@
         * 'Proto.Spec_Fields.vec'repeatedScalar05' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Data.Word.Word32)@
         * 'Proto.Spec_Fields.repeatedScalar06' @:: Lens' ExampleMessage [Data.Word.Word64]@
         * 'Proto.Spec_Fields.vec'repeatedScalar06' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Data.Word.Word64)@
         * 'Proto.Spec_Fields.repeatedScalar07' @:: Lens' ExampleMessage [Data.Int.Int32]@
         * 'Proto.Spec_Fields.vec'repeatedScalar07' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Data.Int.Int32)@
         * 'Proto.Spec_Fields.repeatedScalar08' @:: Lens' ExampleMessage [Data.Int.Int64]@
         * 'Proto.Spec_Fields.vec'repeatedScalar08' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Data.Int.Int64)@
         * 'Proto.Spec_Fields.repeatedScalar09' @:: Lens' ExampleMessage [Data.Word.Word32]@
         * 'Proto.Spec_Fields.vec'repeatedScalar09' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Data.Word.Word32)@
         * 'Proto.Spec_Fields.repeatedScalar10' @:: Lens' ExampleMessage [Data.Word.Word64]@
         * 'Proto.Spec_Fields.vec'repeatedScalar10' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Data.Word.Word64)@
         * 'Proto.Spec_Fields.repeatedScalar11' @:: Lens' ExampleMessage [Data.Int.Int32]@
         * 'Proto.Spec_Fields.vec'repeatedScalar11' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Data.Int.Int32)@
         * 'Proto.Spec_Fields.repeatedScalar12' @:: Lens' ExampleMessage [Data.Int.Int64]@
         * 'Proto.Spec_Fields.vec'repeatedScalar12' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Data.Int.Int64)@
         * 'Proto.Spec_Fields.repeatedScalar13' @:: Lens' ExampleMessage [Prelude.Bool]@
         * 'Proto.Spec_Fields.vec'repeatedScalar13' @:: Lens' ExampleMessage (Data.Vector.Unboxed.Vector Prelude.Bool)@
         * 'Proto.Spec_Fields.repeatedScalar14' @:: Lens' ExampleMessage [Data.Text.Text]@
         * 'Proto.Spec_Fields.vec'repeatedScalar14' @:: Lens' ExampleMessage (Data.Vector.Vector Data.Text.Text)@
         * 'Proto.Spec_Fields.repeatedScalar15' @:: Lens' ExampleMessage [Data.ByteString.ByteString]@
         * 'Proto.Spec_Fields.vec'repeatedScalar15' @:: Lens' ExampleMessage (Data.Vector.Vector Data.ByteString.ByteString)@
         * 'Proto.Spec_Fields.repeatedAnother' @:: Lens' ExampleMessage [AnotherMessage]@
         * 'Proto.Spec_Fields.vec'repeatedAnother' @:: Lens' ExampleMessage (Data.Vector.Vector AnotherMessage)@
         * 'Proto.Spec_Fields.repeatedNested' @:: Lens' ExampleMessage [ExampleMessage'NestedMessage]@
         * 'Proto.Spec_Fields.vec'repeatedNested' @:: Lens' ExampleMessage (Data.Vector.Vector ExampleMessage'NestedMessage)@
         * 'Proto.Spec_Fields.repeatedEnum' @:: Lens' ExampleMessage [ExampleEnum]@
         * 'Proto.Spec_Fields.vec'repeatedEnum' @:: Lens' ExampleMessage (Data.Vector.Vector ExampleEnum)@
         * 'Proto.Spec_Fields.mapScalar01' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Prelude.Double)@
         * 'Proto.Spec_Fields.mapScalar02' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Prelude.Float)@
         * 'Proto.Spec_Fields.mapScalar03' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.Int.Int32)@
         * 'Proto.Spec_Fields.mapScalar04' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.Int.Int64)@
         * 'Proto.Spec_Fields.mapScalar05' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.Word.Word32)@
         * 'Proto.Spec_Fields.mapScalar06' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.Word.Word64)@
         * 'Proto.Spec_Fields.mapScalar07' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.Int.Int32)@
         * 'Proto.Spec_Fields.mapScalar08' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.Int.Int64)@
         * 'Proto.Spec_Fields.mapScalar09' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.Word.Word32)@
         * 'Proto.Spec_Fields.mapScalar10' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.Word.Word64)@
         * 'Proto.Spec_Fields.mapScalar11' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.Int.Int32)@
         * 'Proto.Spec_Fields.mapScalar12' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.Int.Int64)@
         * 'Proto.Spec_Fields.mapScalar13' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Prelude.Bool)@
         * 'Proto.Spec_Fields.mapScalar14' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.Text.Text)@
         * 'Proto.Spec_Fields.mapScalar15' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text Data.ByteString.ByteString)@
         * 'Proto.Spec_Fields.mapAnother' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text AnotherMessage)@
         * 'Proto.Spec_Fields.mapNested' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text ExampleMessage'NestedMessage)@
         * 'Proto.Spec_Fields.mapEnum' @:: Lens' ExampleMessage (Data.Map.Map Data.Text.Text ExampleEnum)@
         * 'Proto.Spec_Fields.maybe'exampleOneOf' @:: Lens' ExampleMessage (Prelude.Maybe ExampleMessage'ExampleOneOf)@
         * 'Proto.Spec_Fields.maybe'oneofScalar01' @:: Lens' ExampleMessage (Prelude.Maybe Prelude.Double)@
         * 'Proto.Spec_Fields.oneofScalar01' @:: Lens' ExampleMessage Prelude.Double@
         * 'Proto.Spec_Fields.maybe'oneofScalar02' @:: Lens' ExampleMessage (Prelude.Maybe Prelude.Float)@
         * 'Proto.Spec_Fields.oneofScalar02' @:: Lens' ExampleMessage Prelude.Float@
         * 'Proto.Spec_Fields.maybe'oneofScalar03' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int32)@
         * 'Proto.Spec_Fields.oneofScalar03' @:: Lens' ExampleMessage Data.Int.Int32@
         * 'Proto.Spec_Fields.maybe'oneofScalar04' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int64)@
         * 'Proto.Spec_Fields.oneofScalar04' @:: Lens' ExampleMessage Data.Int.Int64@
         * 'Proto.Spec_Fields.maybe'oneofScalar05' @:: Lens' ExampleMessage (Prelude.Maybe Data.Word.Word32)@
         * 'Proto.Spec_Fields.oneofScalar05' @:: Lens' ExampleMessage Data.Word.Word32@
         * 'Proto.Spec_Fields.maybe'oneofScalar06' @:: Lens' ExampleMessage (Prelude.Maybe Data.Word.Word64)@
         * 'Proto.Spec_Fields.oneofScalar06' @:: Lens' ExampleMessage Data.Word.Word64@
         * 'Proto.Spec_Fields.maybe'oneofScalar07' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int32)@
         * 'Proto.Spec_Fields.oneofScalar07' @:: Lens' ExampleMessage Data.Int.Int32@
         * 'Proto.Spec_Fields.maybe'oneofScalar08' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int64)@
         * 'Proto.Spec_Fields.oneofScalar08' @:: Lens' ExampleMessage Data.Int.Int64@
         * 'Proto.Spec_Fields.maybe'oneofScalar09' @:: Lens' ExampleMessage (Prelude.Maybe Data.Word.Word32)@
         * 'Proto.Spec_Fields.oneofScalar09' @:: Lens' ExampleMessage Data.Word.Word32@
         * 'Proto.Spec_Fields.maybe'oneofScalar10' @:: Lens' ExampleMessage (Prelude.Maybe Data.Word.Word64)@
         * 'Proto.Spec_Fields.oneofScalar10' @:: Lens' ExampleMessage Data.Word.Word64@
         * 'Proto.Spec_Fields.maybe'oneofScalar11' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int32)@
         * 'Proto.Spec_Fields.oneofScalar11' @:: Lens' ExampleMessage Data.Int.Int32@
         * 'Proto.Spec_Fields.maybe'oneofScalar12' @:: Lens' ExampleMessage (Prelude.Maybe Data.Int.Int64)@
         * 'Proto.Spec_Fields.oneofScalar12' @:: Lens' ExampleMessage Data.Int.Int64@
         * 'Proto.Spec_Fields.maybe'oneofScalar13' @:: Lens' ExampleMessage (Prelude.Maybe Prelude.Bool)@
         * 'Proto.Spec_Fields.oneofScalar13' @:: Lens' ExampleMessage Prelude.Bool@
         * 'Proto.Spec_Fields.maybe'oneofScalar14' @:: Lens' ExampleMessage (Prelude.Maybe Data.Text.Text)@
         * 'Proto.Spec_Fields.oneofScalar14' @:: Lens' ExampleMessage Data.Text.Text@
         * 'Proto.Spec_Fields.maybe'oneofScalar15' @:: Lens' ExampleMessage (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Spec_Fields.oneofScalar15' @:: Lens' ExampleMessage Data.ByteString.ByteString@
         * 'Proto.Spec_Fields.maybe'oneofAnother' @:: Lens' ExampleMessage (Prelude.Maybe AnotherMessage)@
         * 'Proto.Spec_Fields.oneofAnother' @:: Lens' ExampleMessage AnotherMessage@
         * 'Proto.Spec_Fields.maybe'oneofNested' @:: Lens' ExampleMessage (Prelude.Maybe ExampleMessage'NestedMessage)@
         * 'Proto.Spec_Fields.oneofNested' @:: Lens' ExampleMessage ExampleMessage'NestedMessage@
         * 'Proto.Spec_Fields.maybe'oneofEnum' @:: Lens' ExampleMessage (Prelude.Maybe ExampleEnum)@
         * 'Proto.Spec_Fields.oneofEnum' @:: Lens' ExampleMessage ExampleEnum@ -}
data ExampleMessage
  = ExampleMessage'_constructor {_ExampleMessage'defaultScalar01 :: !Prelude.Double,
                                 _ExampleMessage'defaultScalar02 :: !Prelude.Float,
                                 _ExampleMessage'defaultScalar03 :: !Data.Int.Int32,
                                 _ExampleMessage'defaultScalar04 :: !Data.Int.Int64,
                                 _ExampleMessage'defaultScalar05 :: !Data.Word.Word32,
                                 _ExampleMessage'defaultScalar06 :: !Data.Word.Word64,
                                 _ExampleMessage'defaultScalar07 :: !Data.Int.Int32,
                                 _ExampleMessage'defaultScalar08 :: !Data.Int.Int64,
                                 _ExampleMessage'defaultScalar09 :: !Data.Word.Word32,
                                 _ExampleMessage'defaultScalar10 :: !Data.Word.Word64,
                                 _ExampleMessage'defaultScalar11 :: !Data.Int.Int32,
                                 _ExampleMessage'defaultScalar12 :: !Data.Int.Int64,
                                 _ExampleMessage'defaultScalar13 :: !Prelude.Bool,
                                 _ExampleMessage'defaultScalar14 :: !Data.Text.Text,
                                 _ExampleMessage'defaultScalar15 :: !Data.ByteString.ByteString,
                                 _ExampleMessage'defaultAnother :: !(Prelude.Maybe AnotherMessage),
                                 _ExampleMessage'defaultNested :: !(Prelude.Maybe ExampleMessage'NestedMessage),
                                 _ExampleMessage'defaultEnum :: !ExampleEnum,
                                 _ExampleMessage'optionalScalar01 :: !(Prelude.Maybe Prelude.Double),
                                 _ExampleMessage'optionalScalar02 :: !(Prelude.Maybe Prelude.Float),
                                 _ExampleMessage'optionalScalar03 :: !(Prelude.Maybe Data.Int.Int32),
                                 _ExampleMessage'optionalScalar04 :: !(Prelude.Maybe Data.Int.Int64),
                                 _ExampleMessage'optionalScalar05 :: !(Prelude.Maybe Data.Word.Word32),
                                 _ExampleMessage'optionalScalar06 :: !(Prelude.Maybe Data.Word.Word64),
                                 _ExampleMessage'optionalScalar07 :: !(Prelude.Maybe Data.Int.Int32),
                                 _ExampleMessage'optionalScalar08 :: !(Prelude.Maybe Data.Int.Int64),
                                 _ExampleMessage'optionalScalar09 :: !(Prelude.Maybe Data.Word.Word32),
                                 _ExampleMessage'optionalScalar10 :: !(Prelude.Maybe Data.Word.Word64),
                                 _ExampleMessage'optionalScalar11 :: !(Prelude.Maybe Data.Int.Int32),
                                 _ExampleMessage'optionalScalar12 :: !(Prelude.Maybe Data.Int.Int64),
                                 _ExampleMessage'optionalScalar13 :: !(Prelude.Maybe Prelude.Bool),
                                 _ExampleMessage'optionalScalar14 :: !(Prelude.Maybe Data.Text.Text),
                                 _ExampleMessage'optionalScalar15 :: !(Prelude.Maybe Data.ByteString.ByteString),
                                 _ExampleMessage'optionalAnother :: !(Prelude.Maybe AnotherMessage),
                                 _ExampleMessage'optionalNested :: !(Prelude.Maybe ExampleMessage'NestedMessage),
                                 _ExampleMessage'optionalEnum :: !(Prelude.Maybe ExampleEnum),
                                 _ExampleMessage'repeatedScalar01 :: !(Data.Vector.Unboxed.Vector Prelude.Double),
                                 _ExampleMessage'repeatedScalar02 :: !(Data.Vector.Unboxed.Vector Prelude.Float),
                                 _ExampleMessage'repeatedScalar03 :: !(Data.Vector.Unboxed.Vector Data.Int.Int32),
                                 _ExampleMessage'repeatedScalar04 :: !(Data.Vector.Unboxed.Vector Data.Int.Int64),
                                 _ExampleMessage'repeatedScalar05 :: !(Data.Vector.Unboxed.Vector Data.Word.Word32),
                                 _ExampleMessage'repeatedScalar06 :: !(Data.Vector.Unboxed.Vector Data.Word.Word64),
                                 _ExampleMessage'repeatedScalar07 :: !(Data.Vector.Unboxed.Vector Data.Int.Int32),
                                 _ExampleMessage'repeatedScalar08 :: !(Data.Vector.Unboxed.Vector Data.Int.Int64),
                                 _ExampleMessage'repeatedScalar09 :: !(Data.Vector.Unboxed.Vector Data.Word.Word32),
                                 _ExampleMessage'repeatedScalar10 :: !(Data.Vector.Unboxed.Vector Data.Word.Word64),
                                 _ExampleMessage'repeatedScalar11 :: !(Data.Vector.Unboxed.Vector Data.Int.Int32),
                                 _ExampleMessage'repeatedScalar12 :: !(Data.Vector.Unboxed.Vector Data.Int.Int64),
                                 _ExampleMessage'repeatedScalar13 :: !(Data.Vector.Unboxed.Vector Prelude.Bool),
                                 _ExampleMessage'repeatedScalar14 :: !(Data.Vector.Vector Data.Text.Text),
                                 _ExampleMessage'repeatedScalar15 :: !(Data.Vector.Vector Data.ByteString.ByteString),
                                 _ExampleMessage'repeatedAnother :: !(Data.Vector.Vector AnotherMessage),
                                 _ExampleMessage'repeatedNested :: !(Data.Vector.Vector ExampleMessage'NestedMessage),
                                 _ExampleMessage'repeatedEnum :: !(Data.Vector.Vector ExampleEnum),
                                 _ExampleMessage'mapScalar01 :: !(Data.Map.Map Data.Text.Text Prelude.Double),
                                 _ExampleMessage'mapScalar02 :: !(Data.Map.Map Data.Text.Text Prelude.Float),
                                 _ExampleMessage'mapScalar03 :: !(Data.Map.Map Data.Text.Text Data.Int.Int32),
                                 _ExampleMessage'mapScalar04 :: !(Data.Map.Map Data.Text.Text Data.Int.Int64),
                                 _ExampleMessage'mapScalar05 :: !(Data.Map.Map Data.Text.Text Data.Word.Word32),
                                 _ExampleMessage'mapScalar06 :: !(Data.Map.Map Data.Text.Text Data.Word.Word64),
                                 _ExampleMessage'mapScalar07 :: !(Data.Map.Map Data.Text.Text Data.Int.Int32),
                                 _ExampleMessage'mapScalar08 :: !(Data.Map.Map Data.Text.Text Data.Int.Int64),
                                 _ExampleMessage'mapScalar09 :: !(Data.Map.Map Data.Text.Text Data.Word.Word32),
                                 _ExampleMessage'mapScalar10 :: !(Data.Map.Map Data.Text.Text Data.Word.Word64),
                                 _ExampleMessage'mapScalar11 :: !(Data.Map.Map Data.Text.Text Data.Int.Int32),
                                 _ExampleMessage'mapScalar12 :: !(Data.Map.Map Data.Text.Text Data.Int.Int64),
                                 _ExampleMessage'mapScalar13 :: !(Data.Map.Map Data.Text.Text Prelude.Bool),
                                 _ExampleMessage'mapScalar14 :: !(Data.Map.Map Data.Text.Text Data.Text.Text),
                                 _ExampleMessage'mapScalar15 :: !(Data.Map.Map Data.Text.Text Data.ByteString.ByteString),
                                 _ExampleMessage'mapAnother :: !(Data.Map.Map Data.Text.Text AnotherMessage),
                                 _ExampleMessage'mapNested :: !(Data.Map.Map Data.Text.Text ExampleMessage'NestedMessage),
                                 _ExampleMessage'mapEnum :: !(Data.Map.Map Data.Text.Text ExampleEnum),
                                 _ExampleMessage'exampleOneOf :: !(Prelude.Maybe ExampleMessage'ExampleOneOf),
                                 _ExampleMessage'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data ExampleMessage'ExampleOneOf
  = ExampleMessage'OneofScalar01 !Prelude.Double |
    ExampleMessage'OneofScalar02 !Prelude.Float |
    ExampleMessage'OneofScalar03 !Data.Int.Int32 |
    ExampleMessage'OneofScalar04 !Data.Int.Int64 |
    ExampleMessage'OneofScalar05 !Data.Word.Word32 |
    ExampleMessage'OneofScalar06 !Data.Word.Word64 |
    ExampleMessage'OneofScalar07 !Data.Int.Int32 |
    ExampleMessage'OneofScalar08 !Data.Int.Int64 |
    ExampleMessage'OneofScalar09 !Data.Word.Word32 |
    ExampleMessage'OneofScalar10 !Data.Word.Word64 |
    ExampleMessage'OneofScalar11 !Data.Int.Int32 |
    ExampleMessage'OneofScalar12 !Data.Int.Int64 |
    ExampleMessage'OneofScalar13 !Prelude.Bool |
    ExampleMessage'OneofScalar14 !Data.Text.Text |
    ExampleMessage'OneofScalar15 !Data.ByteString.ByteString |
    ExampleMessage'OneofAnother !AnotherMessage |
    ExampleMessage'OneofNested !ExampleMessage'NestedMessage |
    ExampleMessage'OneofEnum !ExampleEnum
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar01" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar01
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar01 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar02" Prelude.Float where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar02
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar02 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar03" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar03
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar03 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar04" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar04
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar04 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar05" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar05
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar05 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar06" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar06
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar06 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar07" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar07
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar07 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar08" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar08
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar08 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar09" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar09
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar09 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar10" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar10
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar10 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar11" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar11
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar11 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar12" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar12
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar12 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar13" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar13
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar13 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar14" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar14
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar14 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultScalar15" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultScalar15
           (\ x__ y__ -> x__ {_ExampleMessage'defaultScalar15 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultAnother" AnotherMessage where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultAnother
           (\ x__ y__ -> x__ {_ExampleMessage'defaultAnother = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'defaultAnother" (Prelude.Maybe AnotherMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultAnother
           (\ x__ y__ -> x__ {_ExampleMessage'defaultAnother = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultNested" ExampleMessage'NestedMessage where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultNested
           (\ x__ y__ -> x__ {_ExampleMessage'defaultNested = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'defaultNested" (Prelude.Maybe ExampleMessage'NestedMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultNested
           (\ x__ y__ -> x__ {_ExampleMessage'defaultNested = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "defaultEnum" ExampleEnum where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'defaultEnum
           (\ x__ y__ -> x__ {_ExampleMessage'defaultEnum = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar01" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar01
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar01 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar01" (Prelude.Maybe Prelude.Double) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar01
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar01 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar02" Prelude.Float where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar02
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar02 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar02" (Prelude.Maybe Prelude.Float) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar02
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar02 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar03" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar03
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar03 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar03" (Prelude.Maybe Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar03
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar03 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar04" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar04
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar04 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar04" (Prelude.Maybe Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar04
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar04 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar05" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar05
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar05 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar05" (Prelude.Maybe Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar05
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar05 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar06" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar06
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar06 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar06" (Prelude.Maybe Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar06
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar06 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar07" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar07
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar07 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar07" (Prelude.Maybe Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar07
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar07 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar08" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar08
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar08 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar08" (Prelude.Maybe Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar08
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar08 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar09" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar09
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar09 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar09" (Prelude.Maybe Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar09
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar09 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar10" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar10
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar10 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar10" (Prelude.Maybe Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar10
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar10 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar11" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar11
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar11 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar11" (Prelude.Maybe Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar11
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar11 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar12" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar12
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar12 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar12" (Prelude.Maybe Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar12
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar12 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar13" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar13
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar13 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar13" (Prelude.Maybe Prelude.Bool) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar13
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar13 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar14" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar14
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar14 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar14" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar14
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar14 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalScalar15" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar15
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar15 = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalScalar15" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalScalar15
           (\ x__ y__ -> x__ {_ExampleMessage'optionalScalar15 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalAnother" AnotherMessage where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalAnother
           (\ x__ y__ -> x__ {_ExampleMessage'optionalAnother = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalAnother" (Prelude.Maybe AnotherMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalAnother
           (\ x__ y__ -> x__ {_ExampleMessage'optionalAnother = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalNested" ExampleMessage'NestedMessage where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalNested
           (\ x__ y__ -> x__ {_ExampleMessage'optionalNested = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalNested" (Prelude.Maybe ExampleMessage'NestedMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalNested
           (\ x__ y__ -> x__ {_ExampleMessage'optionalNested = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "optionalEnum" ExampleEnum where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalEnum
           (\ x__ y__ -> x__ {_ExampleMessage'optionalEnum = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'optionalEnum" (Prelude.Maybe ExampleEnum) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'optionalEnum
           (\ x__ y__ -> x__ {_ExampleMessage'optionalEnum = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar01" [Prelude.Double] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar01
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar01 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar01" (Data.Vector.Unboxed.Vector Prelude.Double) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar01
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar01 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar02" [Prelude.Float] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar02
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar02 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar02" (Data.Vector.Unboxed.Vector Prelude.Float) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar02
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar02 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar03" [Data.Int.Int32] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar03
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar03 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar03" (Data.Vector.Unboxed.Vector Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar03
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar03 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar04" [Data.Int.Int64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar04
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar04 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar04" (Data.Vector.Unboxed.Vector Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar04
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar04 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar05" [Data.Word.Word32] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar05
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar05 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar05" (Data.Vector.Unboxed.Vector Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar05
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar05 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar06" [Data.Word.Word64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar06
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar06 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar06" (Data.Vector.Unboxed.Vector Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar06
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar06 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar07" [Data.Int.Int32] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar07
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar07 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar07" (Data.Vector.Unboxed.Vector Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar07
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar07 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar08" [Data.Int.Int64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar08
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar08 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar08" (Data.Vector.Unboxed.Vector Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar08
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar08 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar09" [Data.Word.Word32] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar09
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar09 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar09" (Data.Vector.Unboxed.Vector Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar09
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar09 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar10" [Data.Word.Word64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar10
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar10 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar10" (Data.Vector.Unboxed.Vector Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar10
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar10 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar11" [Data.Int.Int32] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar11
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar11 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar11" (Data.Vector.Unboxed.Vector Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar11
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar11 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar12" [Data.Int.Int64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar12
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar12 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar12" (Data.Vector.Unboxed.Vector Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar12
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar12 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar13" [Prelude.Bool] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar13
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar13 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar13" (Data.Vector.Unboxed.Vector Prelude.Bool) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar13
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar13 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar14" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar14
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar14 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar14" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar14
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar14 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedScalar15" [Data.ByteString.ByteString] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar15
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar15 = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedScalar15" (Data.Vector.Vector Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedScalar15
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedScalar15 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedAnother" [AnotherMessage] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedAnother
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedAnother = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedAnother" (Data.Vector.Vector AnotherMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedAnother
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedAnother = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedNested" [ExampleMessage'NestedMessage] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedNested
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedNested = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedNested" (Data.Vector.Vector ExampleMessage'NestedMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedNested
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedNested = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "repeatedEnum" [ExampleEnum] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedEnum
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedEnum = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "vec'repeatedEnum" (Data.Vector.Vector ExampleEnum) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'repeatedEnum
           (\ x__ y__ -> x__ {_ExampleMessage'repeatedEnum = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar01" (Data.Map.Map Data.Text.Text Prelude.Double) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar01
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar01 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar02" (Data.Map.Map Data.Text.Text Prelude.Float) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar02
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar02 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar03" (Data.Map.Map Data.Text.Text Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar03
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar03 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar04" (Data.Map.Map Data.Text.Text Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar04
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar04 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar05" (Data.Map.Map Data.Text.Text Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar05
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar05 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar06" (Data.Map.Map Data.Text.Text Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar06
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar06 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar07" (Data.Map.Map Data.Text.Text Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar07
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar07 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar08" (Data.Map.Map Data.Text.Text Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar08
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar08 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar09" (Data.Map.Map Data.Text.Text Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar09
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar09 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar10" (Data.Map.Map Data.Text.Text Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar10
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar10 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar11" (Data.Map.Map Data.Text.Text Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar11
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar11 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar12" (Data.Map.Map Data.Text.Text Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar12
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar12 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar13" (Data.Map.Map Data.Text.Text Prelude.Bool) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar13
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar13 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar14" (Data.Map.Map Data.Text.Text Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar14
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar14 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapScalar15" (Data.Map.Map Data.Text.Text Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapScalar15
           (\ x__ y__ -> x__ {_ExampleMessage'mapScalar15 = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapAnother" (Data.Map.Map Data.Text.Text AnotherMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapAnother
           (\ x__ y__ -> x__ {_ExampleMessage'mapAnother = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapNested" (Data.Map.Map Data.Text.Text ExampleMessage'NestedMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapNested
           (\ x__ y__ -> x__ {_ExampleMessage'mapNested = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "mapEnum" (Data.Map.Map Data.Text.Text ExampleEnum) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'mapEnum
           (\ x__ y__ -> x__ {_ExampleMessage'mapEnum = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'exampleOneOf" (Prelude.Maybe ExampleMessage'ExampleOneOf) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar01" (Prelude.Maybe Prelude.Double) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar01 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar01 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar01" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar01 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar01 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar02" (Prelude.Maybe Prelude.Float) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar02 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar02 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar02" Prelude.Float where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar02 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar02 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar03" (Prelude.Maybe Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar03 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar03 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar03" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar03 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar03 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar04" (Prelude.Maybe Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar04 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar04 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar04" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar04 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar04 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar05" (Prelude.Maybe Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar05 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar05 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar05" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar05 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar05 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar06" (Prelude.Maybe Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar06 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar06 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar06" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar06 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar06 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar07" (Prelude.Maybe Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar07 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar07 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar07" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar07 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar07 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar08" (Prelude.Maybe Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar08 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar08 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar08" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar08 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar08 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar09" (Prelude.Maybe Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar09 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar09 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar09" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar09 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar09 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar10" (Prelude.Maybe Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar10 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar10 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar10" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar10 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar10 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar11" (Prelude.Maybe Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar11 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar11 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar11" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar11 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar11 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar12" (Prelude.Maybe Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar12 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar12 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar12" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar12 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar12 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar13" (Prelude.Maybe Prelude.Bool) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar13 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar13 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar13" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar13 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar13 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar14" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar14 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar14 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar14" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar14 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar14 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofScalar15" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofScalar15 x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar15 y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofScalar15" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofScalar15 x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofScalar15 y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofAnother" (Prelude.Maybe AnotherMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofAnother x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofAnother y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofAnother" AnotherMessage where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofAnother x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofAnother y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofNested" (Prelude.Maybe ExampleMessage'NestedMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofNested x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofNested y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofNested" ExampleMessage'NestedMessage where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofNested x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofNested y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField ExampleMessage "maybe'oneofEnum" (Prelude.Maybe ExampleEnum) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (ExampleMessage'OneofEnum x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap ExampleMessage'OneofEnum y__))
instance Data.ProtoLens.Field.HasField ExampleMessage "oneofEnum" ExampleEnum where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'exampleOneOf
           (\ x__ y__ -> x__ {_ExampleMessage'exampleOneOf = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (ExampleMessage'OneofEnum x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap ExampleMessage'OneofEnum y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Message ExampleMessage where
  messageName _ = Data.Text.pack "ExampleMessage"
  packedMessageDescriptor _
    = "\n\
      \\SOExampleMessage\DC2(\n\
      \\SIdefaultScalar01\CANe \SOH(\SOHR\SIdefaultScalar01\DC2(\n\
      \\SIdefaultScalar02\CANf \SOH(\STXR\SIdefaultScalar02\DC2(\n\
      \\SIdefaultScalar03\CANg \SOH(\ENQR\SIdefaultScalar03\DC2(\n\
      \\SIdefaultScalar04\CANh \SOH(\ETXR\SIdefaultScalar04\DC2(\n\
      \\SIdefaultScalar05\CANi \SOH(\rR\SIdefaultScalar05\DC2(\n\
      \\SIdefaultScalar06\CANj \SOH(\EOTR\SIdefaultScalar06\DC2(\n\
      \\SIdefaultScalar07\CANk \SOH(\DC1R\SIdefaultScalar07\DC2(\n\
      \\SIdefaultScalar08\CANl \SOH(\DC2R\SIdefaultScalar08\DC2(\n\
      \\SIdefaultScalar09\CANm \SOH(\aR\SIdefaultScalar09\DC2(\n\
      \\SIdefaultScalar10\CANn \SOH(\ACKR\SIdefaultScalar10\DC2(\n\
      \\SIdefaultScalar11\CANo \SOH(\SIR\SIdefaultScalar11\DC2(\n\
      \\SIdefaultScalar12\CANp \SOH(\DLER\SIdefaultScalar12\DC2(\n\
      \\SIdefaultScalar13\CANq \SOH(\bR\SIdefaultScalar13\DC2(\n\
      \\SIdefaultScalar14\CANr \SOH(\tR\SIdefaultScalar14\DC2(\n\
      \\SIdefaultScalar15\CANs \SOH(\fR\SIdefaultScalar15\DC27\n\
      \\SOdefaultAnother\CANt \SOH(\v2\SI.AnotherMessageR\SOdefaultAnother\DC2C\n\
      \\rdefaultNested\CANu \SOH(\v2\GS.ExampleMessage.NestedMessageR\rdefaultNested\DC2.\n\
      \\vdefaultEnum\CANv \SOH(\SO2\f.ExampleEnumR\vdefaultEnum\DC20\n\
      \\DLEoptionalScalar01\CAN\201\SOH \SOH(\SOHH\SOHR\DLEoptionalScalar01\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar02\CAN\202\SOH \SOH(\STXH\STXR\DLEoptionalScalar02\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar03\CAN\203\SOH \SOH(\ENQH\ETXR\DLEoptionalScalar03\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar04\CAN\204\SOH \SOH(\ETXH\EOTR\DLEoptionalScalar04\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar05\CAN\205\SOH \SOH(\rH\ENQR\DLEoptionalScalar05\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar06\CAN\206\SOH \SOH(\EOTH\ACKR\DLEoptionalScalar06\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar07\CAN\207\SOH \SOH(\DC1H\aR\DLEoptionalScalar07\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar08\CAN\208\SOH \SOH(\DC2H\bR\DLEoptionalScalar08\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar09\CAN\209\SOH \SOH(\aH\tR\DLEoptionalScalar09\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar10\CAN\210\SOH \SOH(\ACKH\n\
      \R\DLEoptionalScalar10\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar11\CAN\211\SOH \SOH(\SIH\vR\DLEoptionalScalar11\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar12\CAN\212\SOH \SOH(\DLEH\fR\DLEoptionalScalar12\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar13\CAN\213\SOH \SOH(\bH\rR\DLEoptionalScalar13\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar14\CAN\214\SOH \SOH(\tH\SOR\DLEoptionalScalar14\136\SOH\SOH\DC20\n\
      \\DLEoptionalScalar15\CAN\215\SOH \SOH(\fH\SIR\DLEoptionalScalar15\136\SOH\SOH\DC2?\n\
      \\SIoptionalAnother\CAN\216\SOH \SOH(\v2\SI.AnotherMessageH\DLER\SIoptionalAnother\136\SOH\SOH\DC2K\n\
      \\SOoptionalNested\CAN\217\SOH \SOH(\v2\GS.ExampleMessage.NestedMessageH\DC1R\SOoptionalNested\136\SOH\SOH\DC26\n\
      \\foptionalEnum\CAN\218\SOH \SOH(\SO2\f.ExampleEnumH\DC2R\foptionalEnum\136\SOH\SOH\DC2+\n\
      \\DLErepeatedScalar01\CAN\173\STX \ETX(\SOHR\DLErepeatedScalar01\DC2+\n\
      \\DLErepeatedScalar02\CAN\174\STX \ETX(\STXR\DLErepeatedScalar02\DC2+\n\
      \\DLErepeatedScalar03\CAN\175\STX \ETX(\ENQR\DLErepeatedScalar03\DC2+\n\
      \\DLErepeatedScalar04\CAN\176\STX \ETX(\ETXR\DLErepeatedScalar04\DC2+\n\
      \\DLErepeatedScalar05\CAN\177\STX \ETX(\rR\DLErepeatedScalar05\DC2+\n\
      \\DLErepeatedScalar06\CAN\178\STX \ETX(\EOTR\DLErepeatedScalar06\DC2+\n\
      \\DLErepeatedScalar07\CAN\179\STX \ETX(\DC1R\DLErepeatedScalar07\DC2+\n\
      \\DLErepeatedScalar08\CAN\180\STX \ETX(\DC2R\DLErepeatedScalar08\DC2+\n\
      \\DLErepeatedScalar09\CAN\181\STX \ETX(\aR\DLErepeatedScalar09\DC2+\n\
      \\DLErepeatedScalar10\CAN\182\STX \ETX(\ACKR\DLErepeatedScalar10\DC2+\n\
      \\DLErepeatedScalar11\CAN\183\STX \ETX(\SIR\DLErepeatedScalar11\DC2+\n\
      \\DLErepeatedScalar12\CAN\184\STX \ETX(\DLER\DLErepeatedScalar12\DC2+\n\
      \\DLErepeatedScalar13\CAN\185\STX \ETX(\bR\DLErepeatedScalar13\DC2+\n\
      \\DLErepeatedScalar14\CAN\186\STX \ETX(\tR\DLErepeatedScalar14\DC2+\n\
      \\DLErepeatedScalar15\CAN\187\STX \ETX(\fR\DLErepeatedScalar15\DC2:\n\
      \\SIrepeatedAnother\CAN\188\STX \ETX(\v2\SI.AnotherMessageR\SIrepeatedAnother\DC2F\n\
      \\SOrepeatedNested\CAN\189\STX \ETX(\v2\GS.ExampleMessage.NestedMessageR\SOrepeatedNested\DC21\n\
      \\frepeatedEnum\CAN\190\STX \ETX(\SO2\f.ExampleEnumR\frepeatedEnum\DC2C\n\
      \\vmapScalar01\CAN\145\ETX \ETX(\v2 .ExampleMessage.MapScalar01EntryR\vmapScalar01\DC2C\n\
      \\vmapScalar02\CAN\146\ETX \ETX(\v2 .ExampleMessage.MapScalar02EntryR\vmapScalar02\DC2C\n\
      \\vmapScalar03\CAN\147\ETX \ETX(\v2 .ExampleMessage.MapScalar03EntryR\vmapScalar03\DC2C\n\
      \\vmapScalar04\CAN\148\ETX \ETX(\v2 .ExampleMessage.MapScalar04EntryR\vmapScalar04\DC2C\n\
      \\vmapScalar05\CAN\149\ETX \ETX(\v2 .ExampleMessage.MapScalar05EntryR\vmapScalar05\DC2C\n\
      \\vmapScalar06\CAN\150\ETX \ETX(\v2 .ExampleMessage.MapScalar06EntryR\vmapScalar06\DC2C\n\
      \\vmapScalar07\CAN\151\ETX \ETX(\v2 .ExampleMessage.MapScalar07EntryR\vmapScalar07\DC2C\n\
      \\vmapScalar08\CAN\152\ETX \ETX(\v2 .ExampleMessage.MapScalar08EntryR\vmapScalar08\DC2C\n\
      \\vmapScalar09\CAN\153\ETX \ETX(\v2 .ExampleMessage.MapScalar09EntryR\vmapScalar09\DC2C\n\
      \\vmapScalar10\CAN\154\ETX \ETX(\v2 .ExampleMessage.MapScalar10EntryR\vmapScalar10\DC2C\n\
      \\vmapScalar11\CAN\155\ETX \ETX(\v2 .ExampleMessage.MapScalar11EntryR\vmapScalar11\DC2C\n\
      \\vmapScalar12\CAN\156\ETX \ETX(\v2 .ExampleMessage.MapScalar12EntryR\vmapScalar12\DC2C\n\
      \\vmapScalar13\CAN\157\ETX \ETX(\v2 .ExampleMessage.MapScalar13EntryR\vmapScalar13\DC2C\n\
      \\vmapScalar14\CAN\158\ETX \ETX(\v2 .ExampleMessage.MapScalar14EntryR\vmapScalar14\DC2C\n\
      \\vmapScalar15\CAN\159\ETX \ETX(\v2 .ExampleMessage.MapScalar15EntryR\vmapScalar15\DC2@\n\
      \\n\
      \mapAnother\CAN\160\ETX \ETX(\v2\US.ExampleMessage.MapAnotherEntryR\n\
      \mapAnother\DC2=\n\
      \\tmapNested\CAN\161\ETX \ETX(\v2\RS.ExampleMessage.MapNestedEntryR\tmapNested\DC27\n\
      \\amapEnum\CAN\162\ETX \ETX(\v2\FS.ExampleMessage.MapEnumEntryR\amapEnum\DC2'\n\
      \\roneofScalar01\CAN\245\ETX \SOH(\SOHH\NULR\roneofScalar01\DC2'\n\
      \\roneofScalar02\CAN\246\ETX \SOH(\STXH\NULR\roneofScalar02\DC2'\n\
      \\roneofScalar03\CAN\247\ETX \SOH(\ENQH\NULR\roneofScalar03\DC2'\n\
      \\roneofScalar04\CAN\248\ETX \SOH(\ETXH\NULR\roneofScalar04\DC2'\n\
      \\roneofScalar05\CAN\249\ETX \SOH(\rH\NULR\roneofScalar05\DC2'\n\
      \\roneofScalar06\CAN\250\ETX \SOH(\EOTH\NULR\roneofScalar06\DC2'\n\
      \\roneofScalar07\CAN\251\ETX \SOH(\DC1H\NULR\roneofScalar07\DC2'\n\
      \\roneofScalar08\CAN\252\ETX \SOH(\DC2H\NULR\roneofScalar08\DC2'\n\
      \\roneofScalar09\CAN\253\ETX \SOH(\aH\NULR\roneofScalar09\DC2'\n\
      \\roneofScalar10\CAN\254\ETX \SOH(\ACKH\NULR\roneofScalar10\DC2'\n\
      \\roneofScalar11\CAN\255\ETX \SOH(\SIH\NULR\roneofScalar11\DC2'\n\
      \\roneofScalar12\CAN\128\EOT \SOH(\DLEH\NULR\roneofScalar12\DC2'\n\
      \\roneofScalar13\CAN\129\EOT \SOH(\bH\NULR\roneofScalar13\DC2'\n\
      \\roneofScalar14\CAN\130\EOT \SOH(\tH\NULR\roneofScalar14\DC2'\n\
      \\roneofScalar15\CAN\131\EOT \SOH(\fH\NULR\roneofScalar15\DC26\n\
      \\foneofAnother\CAN\132\EOT \SOH(\v2\SI.AnotherMessageH\NULR\foneofAnother\DC2B\n\
      \\voneofNested\CAN\133\EOT \SOH(\v2\GS.ExampleMessage.NestedMessageH\NULR\voneofNested\DC2-\n\
      \\toneofEnum\CAN\134\EOT \SOH(\SO2\f.ExampleEnumH\NULR\toneofEnum\SUB\SI\n\
      \\rNestedMessage\SUB>\n\
      \\DLEMapScalar01Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar02Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\STXR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar03Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar04Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ETXR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar05Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\rR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar06Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\EOTR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar07Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\DC1R\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar08Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\DC2R\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar09Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\aR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar10Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ACKR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar11Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\SIR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar12Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\DLER\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar13Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\bR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar14Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\tR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEMapScalar15Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue:\STX8\SOH\SUBN\n\
      \\SIMapAnotherEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2%\n\
      \\ENQvalue\CAN\STX \SOH(\v2\SI.AnotherMessageR\ENQvalue:\STX8\SOH\SUB[\n\
      \\SOMapNestedEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC23\n\
      \\ENQvalue\CAN\STX \SOH(\v2\GS.ExampleMessage.NestedMessageR\ENQvalue:\STX8\SOH\SUBH\n\
      \\fMapEnumEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\"\n\
      \\ENQvalue\CAN\STX \SOH(\SO2\f.ExampleEnumR\ENQvalue:\STX8\SOHB\SO\n\
      \\fexampleOneOfB\DC3\n\
      \\DC1_optionalScalar01B\DC3\n\
      \\DC1_optionalScalar02B\DC3\n\
      \\DC1_optionalScalar03B\DC3\n\
      \\DC1_optionalScalar04B\DC3\n\
      \\DC1_optionalScalar05B\DC3\n\
      \\DC1_optionalScalar06B\DC3\n\
      \\DC1_optionalScalar07B\DC3\n\
      \\DC1_optionalScalar08B\DC3\n\
      \\DC1_optionalScalar09B\DC3\n\
      \\DC1_optionalScalar10B\DC3\n\
      \\DC1_optionalScalar11B\DC3\n\
      \\DC1_optionalScalar12B\DC3\n\
      \\DC1_optionalScalar13B\DC3\n\
      \\DC1_optionalScalar14B\DC3\n\
      \\DC1_optionalScalar15B\DC2\n\
      \\DLE_optionalAnotherB\DC1\n\
      \\SI_optionalNestedB\SI\n\
      \\r_optionalEnum"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        defaultScalar01__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar01"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar01")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar02__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar02"
              (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar02")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar03__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar03"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar03")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar04__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar04"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar04")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar05__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar05"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar05")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar06__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar06"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar06")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar07__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar07"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar07")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar08__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar08"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar08")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar09__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar09"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar09")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar10__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar10"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar10")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar11__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar11"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SFixed32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar11")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar12__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar12"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SFixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar12")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar13__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar13"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar13")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar14__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar14"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar14")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultScalar15__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultScalar15"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultScalar15")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultAnother__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultAnother"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnotherMessage)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'defaultAnother")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultNested__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultNested"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'NestedMessage)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'defaultNested")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        defaultEnum__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "defaultEnum"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleEnum)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultEnum")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar01__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar01"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar01")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar02__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar02"
              (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar02")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar03__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar03"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar03")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar04__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar04"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar04")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar05__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar05"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar05")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar06__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar06"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar06")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar07__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar07"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar07")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar08__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar08"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar08")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar09__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar09"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar09")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar10__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar10"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar10")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar11__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar11"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SFixed32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar11")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar12__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar12"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SFixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar12")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar13__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar13"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar13")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar14__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar14"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar14")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalScalar15__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalScalar15"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalScalar15")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalAnother__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalAnother"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnotherMessage)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalAnother")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalNested__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalNested"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'NestedMessage)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalNested")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        optionalEnum__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "optionalEnum"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleEnum)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'optionalEnum")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar01__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar01"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar01")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar02__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar02"
              (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar02")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar03__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar03"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar03")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar04__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar04"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar04")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar05__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar05"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar05")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar06__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar06"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar06")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar07__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar07"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar07")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar08__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar08"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar08")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar09__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar09"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar09")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar10__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar10"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar10")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar11__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar11"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SFixed32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar11")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar12__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar12"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SFixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar12")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar13__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar13"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedScalar13")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar14__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar14"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"repeatedScalar14")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedScalar15__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedScalar15"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"repeatedScalar15")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedAnother__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedAnother"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnotherMessage)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"repeatedAnother")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedNested__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedNested"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'NestedMessage)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"repeatedNested")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        repeatedEnum__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "repeatedEnum"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleEnum)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"repeatedEnum")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar01__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar01"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar01Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar01")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar02__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar02"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar02Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar02")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar03__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar03"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar03Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar03")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar04__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar04"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar04Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar04")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar05__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar05"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar05Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar05")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar06__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar06"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar06Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar06")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar07__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar07"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar07Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar07")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar08__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar08"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar08Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar08")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar09__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar09"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar09Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar09")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar10__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar10"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar10Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar10")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar11__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar11"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar11Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar11")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar12__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar12"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar12Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar12")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar13__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar13"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar13Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar13")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar14__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar14"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar14Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar14")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapScalar15__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapScalar15"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapScalar15Entry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapScalar15")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapAnother__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapAnother"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapAnotherEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapAnother")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapNested__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapNested"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapNestedEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapNested")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        mapEnum__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapEnum"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'MapEnumEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"mapEnum")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar01__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar01"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar01")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar02__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar02"
              (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar02")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar03__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar03"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar03")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar04__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar04"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar04")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar05__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar05"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar05")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar06__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar06"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar06")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar07__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar07"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar07")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar08__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar08"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar08")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar09__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar09"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar09")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar10__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar10"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar10")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar11__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar11"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SFixed32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar11")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar12__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar12"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SFixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar12")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar13__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar13"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar13")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar14__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar14"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar14")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofScalar15__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofScalar15"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofScalar15")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofAnother__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofAnother"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnotherMessage)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofAnother")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofNested__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofNested"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'NestedMessage)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofNested")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
        oneofEnum__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oneofEnum"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleEnum)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'oneofEnum")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 101, defaultScalar01__field_descriptor),
           (Data.ProtoLens.Tag 102, defaultScalar02__field_descriptor),
           (Data.ProtoLens.Tag 103, defaultScalar03__field_descriptor),
           (Data.ProtoLens.Tag 104, defaultScalar04__field_descriptor),
           (Data.ProtoLens.Tag 105, defaultScalar05__field_descriptor),
           (Data.ProtoLens.Tag 106, defaultScalar06__field_descriptor),
           (Data.ProtoLens.Tag 107, defaultScalar07__field_descriptor),
           (Data.ProtoLens.Tag 108, defaultScalar08__field_descriptor),
           (Data.ProtoLens.Tag 109, defaultScalar09__field_descriptor),
           (Data.ProtoLens.Tag 110, defaultScalar10__field_descriptor),
           (Data.ProtoLens.Tag 111, defaultScalar11__field_descriptor),
           (Data.ProtoLens.Tag 112, defaultScalar12__field_descriptor),
           (Data.ProtoLens.Tag 113, defaultScalar13__field_descriptor),
           (Data.ProtoLens.Tag 114, defaultScalar14__field_descriptor),
           (Data.ProtoLens.Tag 115, defaultScalar15__field_descriptor),
           (Data.ProtoLens.Tag 116, defaultAnother__field_descriptor),
           (Data.ProtoLens.Tag 117, defaultNested__field_descriptor),
           (Data.ProtoLens.Tag 118, defaultEnum__field_descriptor),
           (Data.ProtoLens.Tag 201, optionalScalar01__field_descriptor),
           (Data.ProtoLens.Tag 202, optionalScalar02__field_descriptor),
           (Data.ProtoLens.Tag 203, optionalScalar03__field_descriptor),
           (Data.ProtoLens.Tag 204, optionalScalar04__field_descriptor),
           (Data.ProtoLens.Tag 205, optionalScalar05__field_descriptor),
           (Data.ProtoLens.Tag 206, optionalScalar06__field_descriptor),
           (Data.ProtoLens.Tag 207, optionalScalar07__field_descriptor),
           (Data.ProtoLens.Tag 208, optionalScalar08__field_descriptor),
           (Data.ProtoLens.Tag 209, optionalScalar09__field_descriptor),
           (Data.ProtoLens.Tag 210, optionalScalar10__field_descriptor),
           (Data.ProtoLens.Tag 211, optionalScalar11__field_descriptor),
           (Data.ProtoLens.Tag 212, optionalScalar12__field_descriptor),
           (Data.ProtoLens.Tag 213, optionalScalar13__field_descriptor),
           (Data.ProtoLens.Tag 214, optionalScalar14__field_descriptor),
           (Data.ProtoLens.Tag 215, optionalScalar15__field_descriptor),
           (Data.ProtoLens.Tag 216, optionalAnother__field_descriptor),
           (Data.ProtoLens.Tag 217, optionalNested__field_descriptor),
           (Data.ProtoLens.Tag 218, optionalEnum__field_descriptor),
           (Data.ProtoLens.Tag 301, repeatedScalar01__field_descriptor),
           (Data.ProtoLens.Tag 302, repeatedScalar02__field_descriptor),
           (Data.ProtoLens.Tag 303, repeatedScalar03__field_descriptor),
           (Data.ProtoLens.Tag 304, repeatedScalar04__field_descriptor),
           (Data.ProtoLens.Tag 305, repeatedScalar05__field_descriptor),
           (Data.ProtoLens.Tag 306, repeatedScalar06__field_descriptor),
           (Data.ProtoLens.Tag 307, repeatedScalar07__field_descriptor),
           (Data.ProtoLens.Tag 308, repeatedScalar08__field_descriptor),
           (Data.ProtoLens.Tag 309, repeatedScalar09__field_descriptor),
           (Data.ProtoLens.Tag 310, repeatedScalar10__field_descriptor),
           (Data.ProtoLens.Tag 311, repeatedScalar11__field_descriptor),
           (Data.ProtoLens.Tag 312, repeatedScalar12__field_descriptor),
           (Data.ProtoLens.Tag 313, repeatedScalar13__field_descriptor),
           (Data.ProtoLens.Tag 314, repeatedScalar14__field_descriptor),
           (Data.ProtoLens.Tag 315, repeatedScalar15__field_descriptor),
           (Data.ProtoLens.Tag 316, repeatedAnother__field_descriptor),
           (Data.ProtoLens.Tag 317, repeatedNested__field_descriptor),
           (Data.ProtoLens.Tag 318, repeatedEnum__field_descriptor),
           (Data.ProtoLens.Tag 401, mapScalar01__field_descriptor),
           (Data.ProtoLens.Tag 402, mapScalar02__field_descriptor),
           (Data.ProtoLens.Tag 403, mapScalar03__field_descriptor),
           (Data.ProtoLens.Tag 404, mapScalar04__field_descriptor),
           (Data.ProtoLens.Tag 405, mapScalar05__field_descriptor),
           (Data.ProtoLens.Tag 406, mapScalar06__field_descriptor),
           (Data.ProtoLens.Tag 407, mapScalar07__field_descriptor),
           (Data.ProtoLens.Tag 408, mapScalar08__field_descriptor),
           (Data.ProtoLens.Tag 409, mapScalar09__field_descriptor),
           (Data.ProtoLens.Tag 410, mapScalar10__field_descriptor),
           (Data.ProtoLens.Tag 411, mapScalar11__field_descriptor),
           (Data.ProtoLens.Tag 412, mapScalar12__field_descriptor),
           (Data.ProtoLens.Tag 413, mapScalar13__field_descriptor),
           (Data.ProtoLens.Tag 414, mapScalar14__field_descriptor),
           (Data.ProtoLens.Tag 415, mapScalar15__field_descriptor),
           (Data.ProtoLens.Tag 416, mapAnother__field_descriptor),
           (Data.ProtoLens.Tag 417, mapNested__field_descriptor),
           (Data.ProtoLens.Tag 418, mapEnum__field_descriptor),
           (Data.ProtoLens.Tag 501, oneofScalar01__field_descriptor),
           (Data.ProtoLens.Tag 502, oneofScalar02__field_descriptor),
           (Data.ProtoLens.Tag 503, oneofScalar03__field_descriptor),
           (Data.ProtoLens.Tag 504, oneofScalar04__field_descriptor),
           (Data.ProtoLens.Tag 505, oneofScalar05__field_descriptor),
           (Data.ProtoLens.Tag 506, oneofScalar06__field_descriptor),
           (Data.ProtoLens.Tag 507, oneofScalar07__field_descriptor),
           (Data.ProtoLens.Tag 508, oneofScalar08__field_descriptor),
           (Data.ProtoLens.Tag 509, oneofScalar09__field_descriptor),
           (Data.ProtoLens.Tag 510, oneofScalar10__field_descriptor),
           (Data.ProtoLens.Tag 511, oneofScalar11__field_descriptor),
           (Data.ProtoLens.Tag 512, oneofScalar12__field_descriptor),
           (Data.ProtoLens.Tag 513, oneofScalar13__field_descriptor),
           (Data.ProtoLens.Tag 514, oneofScalar14__field_descriptor),
           (Data.ProtoLens.Tag 515, oneofScalar15__field_descriptor),
           (Data.ProtoLens.Tag 516, oneofAnother__field_descriptor),
           (Data.ProtoLens.Tag 517, oneofNested__field_descriptor),
           (Data.ProtoLens.Tag 518, oneofEnum__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'_unknownFields
        (\ x__ y__ -> x__ {_ExampleMessage'_unknownFields = y__})
  defMessage
    = ExampleMessage'_constructor
        {_ExampleMessage'defaultScalar01 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar02 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar03 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar04 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar05 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar06 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar07 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar08 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar09 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar10 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar11 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar12 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar13 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar14 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultScalar15 = Data.ProtoLens.fieldDefault,
         _ExampleMessage'defaultAnother = Prelude.Nothing,
         _ExampleMessage'defaultNested = Prelude.Nothing,
         _ExampleMessage'defaultEnum = Data.ProtoLens.fieldDefault,
         _ExampleMessage'optionalScalar01 = Prelude.Nothing,
         _ExampleMessage'optionalScalar02 = Prelude.Nothing,
         _ExampleMessage'optionalScalar03 = Prelude.Nothing,
         _ExampleMessage'optionalScalar04 = Prelude.Nothing,
         _ExampleMessage'optionalScalar05 = Prelude.Nothing,
         _ExampleMessage'optionalScalar06 = Prelude.Nothing,
         _ExampleMessage'optionalScalar07 = Prelude.Nothing,
         _ExampleMessage'optionalScalar08 = Prelude.Nothing,
         _ExampleMessage'optionalScalar09 = Prelude.Nothing,
         _ExampleMessage'optionalScalar10 = Prelude.Nothing,
         _ExampleMessage'optionalScalar11 = Prelude.Nothing,
         _ExampleMessage'optionalScalar12 = Prelude.Nothing,
         _ExampleMessage'optionalScalar13 = Prelude.Nothing,
         _ExampleMessage'optionalScalar14 = Prelude.Nothing,
         _ExampleMessage'optionalScalar15 = Prelude.Nothing,
         _ExampleMessage'optionalAnother = Prelude.Nothing,
         _ExampleMessage'optionalNested = Prelude.Nothing,
         _ExampleMessage'optionalEnum = Prelude.Nothing,
         _ExampleMessage'repeatedScalar01 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar02 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar03 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar04 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar05 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar06 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar07 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar08 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar09 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar10 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar11 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar12 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar13 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar14 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedScalar15 = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedAnother = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedNested = Data.Vector.Generic.empty,
         _ExampleMessage'repeatedEnum = Data.Vector.Generic.empty,
         _ExampleMessage'mapScalar01 = Data.Map.empty,
         _ExampleMessage'mapScalar02 = Data.Map.empty,
         _ExampleMessage'mapScalar03 = Data.Map.empty,
         _ExampleMessage'mapScalar04 = Data.Map.empty,
         _ExampleMessage'mapScalar05 = Data.Map.empty,
         _ExampleMessage'mapScalar06 = Data.Map.empty,
         _ExampleMessage'mapScalar07 = Data.Map.empty,
         _ExampleMessage'mapScalar08 = Data.Map.empty,
         _ExampleMessage'mapScalar09 = Data.Map.empty,
         _ExampleMessage'mapScalar10 = Data.Map.empty,
         _ExampleMessage'mapScalar11 = Data.Map.empty,
         _ExampleMessage'mapScalar12 = Data.Map.empty,
         _ExampleMessage'mapScalar13 = Data.Map.empty,
         _ExampleMessage'mapScalar14 = Data.Map.empty,
         _ExampleMessage'mapScalar15 = Data.Map.empty,
         _ExampleMessage'mapAnother = Data.Map.empty,
         _ExampleMessage'mapNested = Data.Map.empty,
         _ExampleMessage'mapEnum = Data.Map.empty,
         _ExampleMessage'exampleOneOf = Prelude.Nothing,
         _ExampleMessage'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld AnotherMessage
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ExampleEnum
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ExampleMessage'NestedMessage
                   -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Prelude.Double
                      -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Prelude.Float
                         -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int32
                            -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int64
                               -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Word.Word32
                                  -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Word.Word64
                                     -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int32
                                        -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int64
                                           -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Word.Word32
                                              -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Word.Word64
                                                 -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int32
                                                    -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int64
                                                       -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Prelude.Bool
                                                          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
                                                             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.ByteString.ByteString
                                                                -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage
        loop
          x
          mutable'repeatedAnother
          mutable'repeatedEnum
          mutable'repeatedNested
          mutable'repeatedScalar01
          mutable'repeatedScalar02
          mutable'repeatedScalar03
          mutable'repeatedScalar04
          mutable'repeatedScalar05
          mutable'repeatedScalar06
          mutable'repeatedScalar07
          mutable'repeatedScalar08
          mutable'repeatedScalar09
          mutable'repeatedScalar10
          mutable'repeatedScalar11
          mutable'repeatedScalar12
          mutable'repeatedScalar13
          mutable'repeatedScalar14
          mutable'repeatedScalar15
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'repeatedAnother <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                  (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                     mutable'repeatedAnother)
                      frozen'repeatedEnum <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                  mutable'repeatedEnum)
                      frozen'repeatedNested <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                 (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                    mutable'repeatedNested)
                      frozen'repeatedScalar01 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar01)
                      frozen'repeatedScalar02 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar02)
                      frozen'repeatedScalar03 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar03)
                      frozen'repeatedScalar04 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar04)
                      frozen'repeatedScalar05 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar05)
                      frozen'repeatedScalar06 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar06)
                      frozen'repeatedScalar07 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar07)
                      frozen'repeatedScalar08 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar08)
                      frozen'repeatedScalar09 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar09)
                      frozen'repeatedScalar10 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar10)
                      frozen'repeatedScalar11 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar11)
                      frozen'repeatedScalar12 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar12)
                      frozen'repeatedScalar13 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar13)
                      frozen'repeatedScalar14 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar14)
                      frozen'repeatedScalar15 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'repeatedScalar15)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'repeatedAnother")
                              frozen'repeatedAnother
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'repeatedEnum")
                                 frozen'repeatedEnum
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'repeatedNested")
                                    frozen'repeatedNested
                                    (Lens.Family2.set
                                       (Data.ProtoLens.Field.field @"vec'repeatedScalar01")
                                       frozen'repeatedScalar01
                                       (Lens.Family2.set
                                          (Data.ProtoLens.Field.field @"vec'repeatedScalar02")
                                          frozen'repeatedScalar02
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"vec'repeatedScalar03")
                                             frozen'repeatedScalar03
                                             (Lens.Family2.set
                                                (Data.ProtoLens.Field.field @"vec'repeatedScalar04")
                                                frozen'repeatedScalar04
                                                (Lens.Family2.set
                                                   (Data.ProtoLens.Field.field
                                                      @"vec'repeatedScalar05")
                                                   frozen'repeatedScalar05
                                                   (Lens.Family2.set
                                                      (Data.ProtoLens.Field.field
                                                         @"vec'repeatedScalar06")
                                                      frozen'repeatedScalar06
                                                      (Lens.Family2.set
                                                         (Data.ProtoLens.Field.field
                                                            @"vec'repeatedScalar07")
                                                         frozen'repeatedScalar07
                                                         (Lens.Family2.set
                                                            (Data.ProtoLens.Field.field
                                                               @"vec'repeatedScalar08")
                                                            frozen'repeatedScalar08
                                                            (Lens.Family2.set
                                                               (Data.ProtoLens.Field.field
                                                                  @"vec'repeatedScalar09")
                                                               frozen'repeatedScalar09
                                                               (Lens.Family2.set
                                                                  (Data.ProtoLens.Field.field
                                                                     @"vec'repeatedScalar10")
                                                                  frozen'repeatedScalar10
                                                                  (Lens.Family2.set
                                                                     (Data.ProtoLens.Field.field
                                                                        @"vec'repeatedScalar11")
                                                                     frozen'repeatedScalar11
                                                                     (Lens.Family2.set
                                                                        (Data.ProtoLens.Field.field
                                                                           @"vec'repeatedScalar12")
                                                                        frozen'repeatedScalar12
                                                                        (Lens.Family2.set
                                                                           (Data.ProtoLens.Field.field
                                                                              @"vec'repeatedScalar13")
                                                                           frozen'repeatedScalar13
                                                                           (Lens.Family2.set
                                                                              (Data.ProtoLens.Field.field
                                                                                 @"vec'repeatedScalar14")
                                                                              frozen'repeatedScalar14
                                                                              (Lens.Family2.set
                                                                                 (Data.ProtoLens.Field.field
                                                                                    @"vec'repeatedScalar15")
                                                                                 frozen'repeatedScalar15
                                                                                 x)))))))))))))))))))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        809
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToDouble
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "defaultScalar01"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar01") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        821
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToFloat
                                          Data.ProtoLens.Encoding.Bytes.getFixed32)
                                       "defaultScalar02"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar02") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        824
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "defaultScalar03"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar03") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        832
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "defaultScalar04"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar04") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        840
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "defaultScalar05"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar05") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        848
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "defaultScalar06"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar06") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        856
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToSignedInt32
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "defaultScalar07"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar07") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        864
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToSignedInt64
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "defaultScalar08"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar08") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        877
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed32 "defaultScalar09"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar09") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        881
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed64 "defaultScalar10"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar10") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        893
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getFixed32)
                                       "defaultScalar11"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar11") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        897
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "defaultScalar12"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar12") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        904
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "defaultScalar13"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar13") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        914
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "defaultScalar14"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar14") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        922
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "defaultScalar15"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultScalar15") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        930
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "defaultAnother"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultAnother") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        938
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "defaultNested"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultNested") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        944
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "defaultEnum"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"defaultEnum") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1609
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToDouble
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "optionalScalar01"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar01") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1621
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToFloat
                                          Data.ProtoLens.Encoding.Bytes.getFixed32)
                                       "optionalScalar02"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar02") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1624
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "optionalScalar03"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar03") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1632
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "optionalScalar04"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar04") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1640
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "optionalScalar05"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar05") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1648
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "optionalScalar06"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar06") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1656
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToSignedInt32
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "optionalScalar07"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar07") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1664
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToSignedInt64
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "optionalScalar08"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar08") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1677
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed32 "optionalScalar09"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar09") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1681
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed64 "optionalScalar10"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar10") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1693
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getFixed32)
                                       "optionalScalar11"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar11") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1697
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "optionalScalar12"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar12") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1704
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "optionalScalar13"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar13") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1714
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "optionalScalar14"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar14") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1722
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "optionalScalar15"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalScalar15") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1730
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "optionalAnother"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalAnother") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1738
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "optionalNested"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalNested") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        1744
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "optionalEnum"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"optionalEnum") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        2409
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Data.ProtoLens.Encoding.Bytes.wordToDouble
                                           Data.ProtoLens.Encoding.Bytes.getFixed64)
                                        "repeatedScalar01"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar01 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested v mutable'repeatedScalar02
                                  mutable'repeatedScalar03 mutable'repeatedScalar04
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2410
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                                       Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                                    "repeatedScalar01"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar01)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested y mutable'repeatedScalar02
                                  mutable'repeatedScalar03 mutable'repeatedScalar04
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2421
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Data.ProtoLens.Encoding.Bytes.wordToFloat
                                           Data.ProtoLens.Encoding.Bytes.getFixed32)
                                        "repeatedScalar02"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar02 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01 v
                                  mutable'repeatedScalar03 mutable'repeatedScalar04
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2418
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Data.ProtoLens.Encoding.Bytes.wordToFloat
                                                                       Data.ProtoLens.Encoding.Bytes.getFixed32)
                                                                    "repeatedScalar02"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar02)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01 y
                                  mutable'repeatedScalar03 mutable'repeatedScalar04
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2424
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "repeatedScalar03"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar03 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 v mutable'repeatedScalar04
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2426
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                                    "repeatedScalar03"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar03)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 y mutable'repeatedScalar04
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2432
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "repeatedScalar04"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar04 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03 v
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2434
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                                    "repeatedScalar04"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar04)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03 y
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2440
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "repeatedScalar05"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar05 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 v mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2442
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                                    "repeatedScalar05"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar05)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 y mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2448
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        Data.ProtoLens.Encoding.Bytes.getVarInt "repeatedScalar06"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar06 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05 v
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2450
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                    "repeatedScalar06"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar06)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05 y
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2456
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Data.ProtoLens.Encoding.Bytes.wordToSignedInt32
                                           (Prelude.fmap
                                              Prelude.fromIntegral
                                              Data.ProtoLens.Encoding.Bytes.getVarInt))
                                        "repeatedScalar07"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar07 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 v mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2458
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Data.ProtoLens.Encoding.Bytes.wordToSignedInt32
                                                                       (Prelude.fmap
                                                                          Prelude.fromIntegral
                                                                          Data.ProtoLens.Encoding.Bytes.getVarInt))
                                                                    "repeatedScalar07"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar07)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 y mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2464
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Data.ProtoLens.Encoding.Bytes.wordToSignedInt64
                                           (Prelude.fmap
                                              Prelude.fromIntegral
                                              Data.ProtoLens.Encoding.Bytes.getVarInt))
                                        "repeatedScalar08"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar08 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07 v
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2466
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Data.ProtoLens.Encoding.Bytes.wordToSignedInt64
                                                                       (Prelude.fmap
                                                                          Prelude.fromIntegral
                                                                          Data.ProtoLens.Encoding.Bytes.getVarInt))
                                                                    "repeatedScalar08"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar08)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07 y
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2477
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        Data.ProtoLens.Encoding.Bytes.getFixed32 "repeatedScalar09"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar09 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 v mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2474
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    Data.ProtoLens.Encoding.Bytes.getFixed32
                                                                    "repeatedScalar09"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar09)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 y mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2481
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        Data.ProtoLens.Encoding.Bytes.getFixed64 "repeatedScalar10"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar10 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09 v
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2482
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    Data.ProtoLens.Encoding.Bytes.getFixed64
                                                                    "repeatedScalar10"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar10)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09 y
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2493
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getFixed32)
                                        "repeatedScalar11"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar11 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 v mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2490
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getFixed32)
                                                                    "repeatedScalar11"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar11)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 y mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2497
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getFixed64)
                                        "repeatedScalar12"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar12 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11 v
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2498
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                                    "repeatedScalar12"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar12)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11 y
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2504
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "repeatedScalar13"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar13 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 v mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2506
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       ((Prelude./=) 0)
                                                                       Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                                    "repeatedScalar13"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedScalar13)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 y mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2514
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.getText
                                              (Prelude.fromIntegral len))
                                        "repeatedScalar14"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar14 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13 v
                                  mutable'repeatedScalar15
                        2522
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.getBytes
                                              (Prelude.fromIntegral len))
                                        "repeatedScalar15"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedScalar15 y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 v
                        2530
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "repeatedAnother"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedAnother y)
                                loop
                                  x v mutable'repeatedEnum mutable'repeatedNested
                                  mutable'repeatedScalar01 mutable'repeatedScalar02
                                  mutable'repeatedScalar03 mutable'repeatedScalar04
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2538
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "repeatedNested"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedNested y)
                                loop
                                  x mutable'repeatedAnother mutable'repeatedEnum v
                                  mutable'repeatedScalar01 mutable'repeatedScalar02
                                  mutable'repeatedScalar03 mutable'repeatedScalar04
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2544
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.toEnum
                                           (Prelude.fmap
                                              Prelude.fromIntegral
                                              Data.ProtoLens.Encoding.Bytes.getVarInt))
                                        "repeatedEnum"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'repeatedEnum y)
                                loop
                                  x mutable'repeatedAnother v mutable'repeatedNested
                                  mutable'repeatedScalar01 mutable'repeatedScalar02
                                  mutable'repeatedScalar03 mutable'repeatedScalar04
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        2546
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.toEnum
                                                                       (Prelude.fmap
                                                                          Prelude.fromIntegral
                                                                          Data.ProtoLens.Encoding.Bytes.getVarInt))
                                                                    "repeatedEnum"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'repeatedEnum)
                                loop
                                  x mutable'repeatedAnother y mutable'repeatedNested
                                  mutable'repeatedScalar01 mutable'repeatedScalar02
                                  mutable'repeatedScalar03 mutable'repeatedScalar04
                                  mutable'repeatedScalar05 mutable'repeatedScalar06
                                  mutable'repeatedScalar07 mutable'repeatedScalar08
                                  mutable'repeatedScalar09 mutable'repeatedScalar10
                                  mutable'repeatedScalar11 mutable'repeatedScalar12
                                  mutable'repeatedScalar13 mutable'repeatedScalar14
                                  mutable'repeatedScalar15
                        3210
                          -> do !(entry :: ExampleMessage'MapScalar01Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar01"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar01")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3218
                          -> do !(entry :: ExampleMessage'MapScalar02Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar02"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar02")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3226
                          -> do !(entry :: ExampleMessage'MapScalar03Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar03"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar03")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3234
                          -> do !(entry :: ExampleMessage'MapScalar04Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar04"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar04")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3242
                          -> do !(entry :: ExampleMessage'MapScalar05Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar05"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar05")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3250
                          -> do !(entry :: ExampleMessage'MapScalar06Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar06"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar06")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3258
                          -> do !(entry :: ExampleMessage'MapScalar07Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar07"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar07")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3266
                          -> do !(entry :: ExampleMessage'MapScalar08Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar08"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar08")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3274
                          -> do !(entry :: ExampleMessage'MapScalar09Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar09"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar09")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3282
                          -> do !(entry :: ExampleMessage'MapScalar10Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar10"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar10")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3290
                          -> do !(entry :: ExampleMessage'MapScalar11Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar11"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar11")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3298
                          -> do !(entry :: ExampleMessage'MapScalar12Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar12"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar12")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3306
                          -> do !(entry :: ExampleMessage'MapScalar13Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar13"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar13")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3314
                          -> do !(entry :: ExampleMessage'MapScalar14Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar14"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar14")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3322
                          -> do !(entry :: ExampleMessage'MapScalar15Entry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "mapScalar15"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapScalar15")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3330
                          -> do !(entry :: ExampleMessage'MapAnotherEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                    Data.ProtoLens.Encoding.Bytes.isolate
                                                                                      (Prelude.fromIntegral
                                                                                         len)
                                                                                      Data.ProtoLens.parseMessage)
                                                                                "mapAnother"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapAnother")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3338
                          -> do !(entry :: ExampleMessage'MapNestedEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                               (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                                                     (Prelude.fromIntegral
                                                                                        len)
                                                                                     Data.ProtoLens.parseMessage)
                                                                               "mapNested"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapNested")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        3346
                          -> do !(entry :: ExampleMessage'MapEnumEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                             (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                 Data.ProtoLens.Encoding.Bytes.isolate
                                                                                   (Prelude.fromIntegral
                                                                                      len)
                                                                                   Data.ProtoLens.parseMessage)
                                                                             "mapEnum"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"mapEnum")
                                        (\ !t -> Data.Map.insert key value t) x)
                                     mutable'repeatedAnother mutable'repeatedEnum
                                     mutable'repeatedNested mutable'repeatedScalar01
                                     mutable'repeatedScalar02 mutable'repeatedScalar03
                                     mutable'repeatedScalar04 mutable'repeatedScalar05
                                     mutable'repeatedScalar06 mutable'repeatedScalar07
                                     mutable'repeatedScalar08 mutable'repeatedScalar09
                                     mutable'repeatedScalar10 mutable'repeatedScalar11
                                     mutable'repeatedScalar12 mutable'repeatedScalar13
                                     mutable'repeatedScalar14 mutable'repeatedScalar15)
                        4009
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToDouble
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "oneofScalar01"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar01") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4021
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToFloat
                                          Data.ProtoLens.Encoding.Bytes.getFixed32)
                                       "oneofScalar02"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar02") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4024
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "oneofScalar03"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar03") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4032
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "oneofScalar04"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar04") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4040
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "oneofScalar05"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar05") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4048
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "oneofScalar06"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar06") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4056
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToSignedInt32
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "oneofScalar07"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar07") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4064
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToSignedInt64
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "oneofScalar08"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar08") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4077
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed32 "oneofScalar09"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar09") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4081
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed64 "oneofScalar10"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar10") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4093
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getFixed32)
                                       "oneofScalar11"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar11") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4097
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "oneofScalar12"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar12") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4104
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "oneofScalar13"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar13") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4114
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "oneofScalar14"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar14") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4122
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "oneofScalar15"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofScalar15") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4130
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "oneofAnother"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"oneofAnother") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4138
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "oneofNested"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"oneofNested") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        4144
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "oneofEnum"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"oneofEnum") y x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'repeatedAnother mutable'repeatedEnum
                                  mutable'repeatedNested mutable'repeatedScalar01
                                  mutable'repeatedScalar02 mutable'repeatedScalar03
                                  mutable'repeatedScalar04 mutable'repeatedScalar05
                                  mutable'repeatedScalar06 mutable'repeatedScalar07
                                  mutable'repeatedScalar08 mutable'repeatedScalar09
                                  mutable'repeatedScalar10 mutable'repeatedScalar11
                                  mutable'repeatedScalar12 mutable'repeatedScalar13
                                  mutable'repeatedScalar14 mutable'repeatedScalar15
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'repeatedAnother <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedEnum <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedNested <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar01 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar02 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar03 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar04 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar05 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar06 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar07 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar08 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar09 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar10 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar11 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar12 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar13 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar14 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'repeatedScalar15 <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage mutable'repeatedAnother
                mutable'repeatedEnum mutable'repeatedNested
                mutable'repeatedScalar01 mutable'repeatedScalar02
                mutable'repeatedScalar03 mutable'repeatedScalar04
                mutable'repeatedScalar05 mutable'repeatedScalar06
                mutable'repeatedScalar07 mutable'repeatedScalar08
                mutable'repeatedScalar09 mutable'repeatedScalar10
                mutable'repeatedScalar11 mutable'repeatedScalar12
                mutable'repeatedScalar13 mutable'repeatedScalar14
                mutable'repeatedScalar15)
          "ExampleMessage"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"defaultScalar01") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 809)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putFixed64
                         Data.ProtoLens.Encoding.Bytes.doubleToWord _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"defaultScalar02") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 821)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putFixed32
                            Data.ProtoLens.Encoding.Bytes.floatToWord _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"defaultScalar03") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 824)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view
                               (Data.ProtoLens.Field.field @"defaultScalar04") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 832)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                      ((Data.Monoid.<>)
                         (let
                            _v
                              = Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"defaultScalar05") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 840)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral
                                     _v))
                         ((Data.Monoid.<>)
                            (let
                               _v
                                 = Lens.Family2.view
                                     (Data.ProtoLens.Field.field @"defaultScalar06") _x
                             in
                               if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 848)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                            ((Data.Monoid.<>)
                               (let
                                  _v
                                    = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"defaultScalar07") _x
                                in
                                  if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                      Data.Monoid.mempty
                                  else
                                      (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 856)
                                        ((Prelude..)
                                           ((Prelude..)
                                              Data.ProtoLens.Encoding.Bytes.putVarInt
                                              Prelude.fromIntegral)
                                           Data.ProtoLens.Encoding.Bytes.signedInt32ToWord _v))
                               ((Data.Monoid.<>)
                                  (let
                                     _v
                                       = Lens.Family2.view
                                           (Data.ProtoLens.Field.field @"defaultScalar08") _x
                                   in
                                     if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                         Data.Monoid.mempty
                                     else
                                         (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt 864)
                                           ((Prelude..)
                                              ((Prelude..)
                                                 Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 Prelude.fromIntegral)
                                              Data.ProtoLens.Encoding.Bytes.signedInt64ToWord _v))
                                  ((Data.Monoid.<>)
                                     (let
                                        _v
                                          = Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"defaultScalar09") _x
                                      in
                                        if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                            Data.Monoid.mempty
                                        else
                                            (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt 877)
                                              (Data.ProtoLens.Encoding.Bytes.putFixed32 _v))
                                     ((Data.Monoid.<>)
                                        (let
                                           _v
                                             = Lens.Family2.view
                                                 (Data.ProtoLens.Field.field @"defaultScalar10") _x
                                         in
                                           if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                               Data.Monoid.mempty
                                           else
                                               (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 881)
                                                 (Data.ProtoLens.Encoding.Bytes.putFixed64 _v))
                                        ((Data.Monoid.<>)
                                           (let
                                              _v
                                                = Lens.Family2.view
                                                    (Data.ProtoLens.Field.field @"defaultScalar11")
                                                    _x
                                            in
                                              if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                  Data.Monoid.mempty
                                              else
                                                  (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 893)
                                                    ((Prelude..)
                                                       Data.ProtoLens.Encoding.Bytes.putFixed32
                                                       Prelude.fromIntegral _v))
                                           ((Data.Monoid.<>)
                                              (let
                                                 _v
                                                   = Lens.Family2.view
                                                       (Data.ProtoLens.Field.field
                                                          @"defaultScalar12")
                                                       _x
                                               in
                                                 if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                     Data.Monoid.mempty
                                                 else
                                                     (Data.Monoid.<>)
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt 897)
                                                       ((Prelude..)
                                                          Data.ProtoLens.Encoding.Bytes.putFixed64
                                                          Prelude.fromIntegral _v))
                                              ((Data.Monoid.<>)
                                                 (let
                                                    _v
                                                      = Lens.Family2.view
                                                          (Data.ProtoLens.Field.field
                                                             @"defaultScalar13")
                                                          _x
                                                  in
                                                    if (Prelude.==)
                                                         _v Data.ProtoLens.fieldDefault then
                                                        Data.Monoid.mempty
                                                    else
                                                        (Data.Monoid.<>)
                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             904)
                                                          ((Prelude..)
                                                             Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             (\ b -> if b then 1 else 0) _v))
                                                 ((Data.Monoid.<>)
                                                    (let
                                                       _v
                                                         = Lens.Family2.view
                                                             (Data.ProtoLens.Field.field
                                                                @"defaultScalar14")
                                                             _x
                                                     in
                                                       if (Prelude.==)
                                                            _v Data.ProtoLens.fieldDefault then
                                                           Data.Monoid.mempty
                                                       else
                                                           (Data.Monoid.<>)
                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                914)
                                                             ((Prelude..)
                                                                (\ bs
                                                                   -> (Data.Monoid.<>)
                                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                           (Prelude.fromIntegral
                                                                              (Data.ByteString.length
                                                                                 bs)))
                                                                        (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                           bs))
                                                                Data.Text.Encoding.encodeUtf8 _v))
                                                    ((Data.Monoid.<>)
                                                       (let
                                                          _v
                                                            = Lens.Family2.view
                                                                (Data.ProtoLens.Field.field
                                                                   @"defaultScalar15")
                                                                _x
                                                        in
                                                          if (Prelude.==)
                                                               _v Data.ProtoLens.fieldDefault then
                                                              Data.Monoid.mempty
                                                          else
                                                              (Data.Monoid.<>)
                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                   922)
                                                                ((\ bs
                                                                    -> (Data.Monoid.<>)
                                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                            (Prelude.fromIntegral
                                                                               (Data.ByteString.length
                                                                                  bs)))
                                                                         (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                            bs))
                                                                   _v))
                                                       ((Data.Monoid.<>)
                                                          (case
                                                               Lens.Family2.view
                                                                 (Data.ProtoLens.Field.field
                                                                    @"maybe'defaultAnother")
                                                                 _x
                                                           of
                                                             Prelude.Nothing -> Data.Monoid.mempty
                                                             (Prelude.Just _v)
                                                               -> (Data.Monoid.<>)
                                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                       930)
                                                                    ((Prelude..)
                                                                       (\ bs
                                                                          -> (Data.Monoid.<>)
                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                  (Prelude.fromIntegral
                                                                                     (Data.ByteString.length
                                                                                        bs)))
                                                                               (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                  bs))
                                                                       Data.ProtoLens.encodeMessage
                                                                       _v))
                                                          ((Data.Monoid.<>)
                                                             (case
                                                                  Lens.Family2.view
                                                                    (Data.ProtoLens.Field.field
                                                                       @"maybe'defaultNested")
                                                                    _x
                                                              of
                                                                Prelude.Nothing
                                                                  -> Data.Monoid.mempty
                                                                (Prelude.Just _v)
                                                                  -> (Data.Monoid.<>)
                                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                          938)
                                                                       ((Prelude..)
                                                                          (\ bs
                                                                             -> (Data.Monoid.<>)
                                                                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                     (Prelude.fromIntegral
                                                                                        (Data.ByteString.length
                                                                                           bs)))
                                                                                  (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                     bs))
                                                                          Data.ProtoLens.encodeMessage
                                                                          _v))
                                                             ((Data.Monoid.<>)
                                                                (let
                                                                   _v
                                                                     = Lens.Family2.view
                                                                         (Data.ProtoLens.Field.field
                                                                            @"defaultEnum")
                                                                         _x
                                                                 in
                                                                   if (Prelude.==)
                                                                        _v
                                                                        Data.ProtoLens.fieldDefault then
                                                                       Data.Monoid.mempty
                                                                   else
                                                                       (Data.Monoid.<>)
                                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                            944)
                                                                         ((Prelude..)
                                                                            ((Prelude..)
                                                                               Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                               Prelude.fromIntegral)
                                                                            Prelude.fromEnum _v))
                                                                ((Data.Monoid.<>)
                                                                   (case
                                                                        Lens.Family2.view
                                                                          (Data.ProtoLens.Field.field
                                                                             @"maybe'optionalScalar01")
                                                                          _x
                                                                    of
                                                                      Prelude.Nothing
                                                                        -> Data.Monoid.mempty
                                                                      (Prelude.Just _v)
                                                                        -> (Data.Monoid.<>)
                                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                1609)
                                                                             ((Prelude..)
                                                                                Data.ProtoLens.Encoding.Bytes.putFixed64
                                                                                Data.ProtoLens.Encoding.Bytes.doubleToWord
                                                                                _v))
                                                                   ((Data.Monoid.<>)
                                                                      (case
                                                                           Lens.Family2.view
                                                                             (Data.ProtoLens.Field.field
                                                                                @"maybe'optionalScalar02")
                                                                             _x
                                                                       of
                                                                         Prelude.Nothing
                                                                           -> Data.Monoid.mempty
                                                                         (Prelude.Just _v)
                                                                           -> (Data.Monoid.<>)
                                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                   1621)
                                                                                ((Prelude..)
                                                                                   Data.ProtoLens.Encoding.Bytes.putFixed32
                                                                                   Data.ProtoLens.Encoding.Bytes.floatToWord
                                                                                   _v))
                                                                      ((Data.Monoid.<>)
                                                                         (case
                                                                              Lens.Family2.view
                                                                                (Data.ProtoLens.Field.field
                                                                                   @"maybe'optionalScalar03")
                                                                                _x
                                                                          of
                                                                            Prelude.Nothing
                                                                              -> Data.Monoid.mempty
                                                                            (Prelude.Just _v)
                                                                              -> (Data.Monoid.<>)
                                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                      1624)
                                                                                   ((Prelude..)
                                                                                      Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                      Prelude.fromIntegral
                                                                                      _v))
                                                                         ((Data.Monoid.<>)
                                                                            (case
                                                                                 Lens.Family2.view
                                                                                   (Data.ProtoLens.Field.field
                                                                                      @"maybe'optionalScalar04")
                                                                                   _x
                                                                             of
                                                                               Prelude.Nothing
                                                                                 -> Data.Monoid.mempty
                                                                               (Prelude.Just _v)
                                                                                 -> (Data.Monoid.<>)
                                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                         1632)
                                                                                      ((Prelude..)
                                                                                         Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                         Prelude.fromIntegral
                                                                                         _v))
                                                                            ((Data.Monoid.<>)
                                                                               (case
                                                                                    Lens.Family2.view
                                                                                      (Data.ProtoLens.Field.field
                                                                                         @"maybe'optionalScalar05")
                                                                                      _x
                                                                                of
                                                                                  Prelude.Nothing
                                                                                    -> Data.Monoid.mempty
                                                                                  (Prelude.Just _v)
                                                                                    -> (Data.Monoid.<>)
                                                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                            1640)
                                                                                         ((Prelude..)
                                                                                            Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                            Prelude.fromIntegral
                                                                                            _v))
                                                                               ((Data.Monoid.<>)
                                                                                  (case
                                                                                       Lens.Family2.view
                                                                                         (Data.ProtoLens.Field.field
                                                                                            @"maybe'optionalScalar06")
                                                                                         _x
                                                                                   of
                                                                                     Prelude.Nothing
                                                                                       -> Data.Monoid.mempty
                                                                                     (Prelude.Just _v)
                                                                                       -> (Data.Monoid.<>)
                                                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                               1648)
                                                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                               _v))
                                                                                  ((Data.Monoid.<>)
                                                                                     (case
                                                                                          Lens.Family2.view
                                                                                            (Data.ProtoLens.Field.field
                                                                                               @"maybe'optionalScalar07")
                                                                                            _x
                                                                                      of
                                                                                        Prelude.Nothing
                                                                                          -> Data.Monoid.mempty
                                                                                        (Prelude.Just _v)
                                                                                          -> (Data.Monoid.<>)
                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                  1656)
                                                                                               ((Prelude..)
                                                                                                  ((Prelude..)
                                                                                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                     Prelude.fromIntegral)
                                                                                                  Data.ProtoLens.Encoding.Bytes.signedInt32ToWord
                                                                                                  _v))
                                                                                     ((Data.Monoid.<>)
                                                                                        (case
                                                                                             Lens.Family2.view
                                                                                               (Data.ProtoLens.Field.field
                                                                                                  @"maybe'optionalScalar08")
                                                                                               _x
                                                                                         of
                                                                                           Prelude.Nothing
                                                                                             -> Data.Monoid.mempty
                                                                                           (Prelude.Just _v)
                                                                                             -> (Data.Monoid.<>)
                                                                                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                     1664)
                                                                                                  ((Prelude..)
                                                                                                     ((Prelude..)
                                                                                                        Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                        Prelude.fromIntegral)
                                                                                                     Data.ProtoLens.Encoding.Bytes.signedInt64ToWord
                                                                                                     _v))
                                                                                        ((Data.Monoid.<>)
                                                                                           (case
                                                                                                Lens.Family2.view
                                                                                                  (Data.ProtoLens.Field.field
                                                                                                     @"maybe'optionalScalar09")
                                                                                                  _x
                                                                                            of
                                                                                              Prelude.Nothing
                                                                                                -> Data.Monoid.mempty
                                                                                              (Prelude.Just _v)
                                                                                                -> (Data.Monoid.<>)
                                                                                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                        1677)
                                                                                                     (Data.ProtoLens.Encoding.Bytes.putFixed32
                                                                                                        _v))
                                                                                           ((Data.Monoid.<>)
                                                                                              (case
                                                                                                   Lens.Family2.view
                                                                                                     (Data.ProtoLens.Field.field
                                                                                                        @"maybe'optionalScalar10")
                                                                                                     _x
                                                                                               of
                                                                                                 Prelude.Nothing
                                                                                                   -> Data.Monoid.mempty
                                                                                                 (Prelude.Just _v)
                                                                                                   -> (Data.Monoid.<>)
                                                                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                           1681)
                                                                                                        (Data.ProtoLens.Encoding.Bytes.putFixed64
                                                                                                           _v))
                                                                                              ((Data.Monoid.<>)
                                                                                                 (case
                                                                                                      Lens.Family2.view
                                                                                                        (Data.ProtoLens.Field.field
                                                                                                           @"maybe'optionalScalar11")
                                                                                                        _x
                                                                                                  of
                                                                                                    Prelude.Nothing
                                                                                                      -> Data.Monoid.mempty
                                                                                                    (Prelude.Just _v)
                                                                                                      -> (Data.Monoid.<>)
                                                                                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                              1693)
                                                                                                           ((Prelude..)
                                                                                                              Data.ProtoLens.Encoding.Bytes.putFixed32
                                                                                                              Prelude.fromIntegral
                                                                                                              _v))
                                                                                                 ((Data.Monoid.<>)
                                                                                                    (case
                                                                                                         Lens.Family2.view
                                                                                                           (Data.ProtoLens.Field.field
                                                                                                              @"maybe'optionalScalar12")
                                                                                                           _x
                                                                                                     of
                                                                                                       Prelude.Nothing
                                                                                                         -> Data.Monoid.mempty
                                                                                                       (Prelude.Just _v)
                                                                                                         -> (Data.Monoid.<>)
                                                                                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                 1697)
                                                                                                              ((Prelude..)
                                                                                                                 Data.ProtoLens.Encoding.Bytes.putFixed64
                                                                                                                 Prelude.fromIntegral
                                                                                                                 _v))
                                                                                                    ((Data.Monoid.<>)
                                                                                                       (case
                                                                                                            Lens.Family2.view
                                                                                                              (Data.ProtoLens.Field.field
                                                                                                                 @"maybe'optionalScalar13")
                                                                                                              _x
                                                                                                        of
                                                                                                          Prelude.Nothing
                                                                                                            -> Data.Monoid.mempty
                                                                                                          (Prelude.Just _v)
                                                                                                            -> (Data.Monoid.<>)
                                                                                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                    1704)
                                                                                                                 ((Prelude..)
                                                                                                                    Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                    (\ b
                                                                                                                       -> if b then
                                                                                                                              1
                                                                                                                          else
                                                                                                                              0)
                                                                                                                    _v))
                                                                                                       ((Data.Monoid.<>)
                                                                                                          (case
                                                                                                               Lens.Family2.view
                                                                                                                 (Data.ProtoLens.Field.field
                                                                                                                    @"maybe'optionalScalar14")
                                                                                                                 _x
                                                                                                           of
                                                                                                             Prelude.Nothing
                                                                                                               -> Data.Monoid.mempty
                                                                                                             (Prelude.Just _v)
                                                                                                               -> (Data.Monoid.<>)
                                                                                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                       1714)
                                                                                                                    ((Prelude..)
                                                                                                                       (\ bs
                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                  (Prelude.fromIntegral
                                                                                                                                     (Data.ByteString.length
                                                                                                                                        bs)))
                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                  bs))
                                                                                                                       Data.Text.Encoding.encodeUtf8
                                                                                                                       _v))
                                                                                                          ((Data.Monoid.<>)
                                                                                                             (case
                                                                                                                  Lens.Family2.view
                                                                                                                    (Data.ProtoLens.Field.field
                                                                                                                       @"maybe'optionalScalar15")
                                                                                                                    _x
                                                                                                              of
                                                                                                                Prelude.Nothing
                                                                                                                  -> Data.Monoid.mempty
                                                                                                                (Prelude.Just _v)
                                                                                                                  -> (Data.Monoid.<>)
                                                                                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                          1722)
                                                                                                                       ((\ bs
                                                                                                                           -> (Data.Monoid.<>)
                                                                                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                   (Prelude.fromIntegral
                                                                                                                                      (Data.ByteString.length
                                                                                                                                         bs)))
                                                                                                                                (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                   bs))
                                                                                                                          _v))
                                                                                                             ((Data.Monoid.<>)
                                                                                                                (case
                                                                                                                     Lens.Family2.view
                                                                                                                       (Data.ProtoLens.Field.field
                                                                                                                          @"maybe'optionalAnother")
                                                                                                                       _x
                                                                                                                 of
                                                                                                                   Prelude.Nothing
                                                                                                                     -> Data.Monoid.mempty
                                                                                                                   (Prelude.Just _v)
                                                                                                                     -> (Data.Monoid.<>)
                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                             1730)
                                                                                                                          ((Prelude..)
                                                                                                                             (\ bs
                                                                                                                                -> (Data.Monoid.<>)
                                                                                                                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                        (Prelude.fromIntegral
                                                                                                                                           (Data.ByteString.length
                                                                                                                                              bs)))
                                                                                                                                     (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                        bs))
                                                                                                                             Data.ProtoLens.encodeMessage
                                                                                                                             _v))
                                                                                                                ((Data.Monoid.<>)
                                                                                                                   (case
                                                                                                                        Lens.Family2.view
                                                                                                                          (Data.ProtoLens.Field.field
                                                                                                                             @"maybe'optionalNested")
                                                                                                                          _x
                                                                                                                    of
                                                                                                                      Prelude.Nothing
                                                                                                                        -> Data.Monoid.mempty
                                                                                                                      (Prelude.Just _v)
                                                                                                                        -> (Data.Monoid.<>)
                                                                                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                1738)
                                                                                                                             ((Prelude..)
                                                                                                                                (\ bs
                                                                                                                                   -> (Data.Monoid.<>)
                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                           (Prelude.fromIntegral
                                                                                                                                              (Data.ByteString.length
                                                                                                                                                 bs)))
                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                           bs))
                                                                                                                                Data.ProtoLens.encodeMessage
                                                                                                                                _v))
                                                                                                                   ((Data.Monoid.<>)
                                                                                                                      (case
                                                                                                                           Lens.Family2.view
                                                                                                                             (Data.ProtoLens.Field.field
                                                                                                                                @"maybe'optionalEnum")
                                                                                                                             _x
                                                                                                                       of
                                                                                                                         Prelude.Nothing
                                                                                                                           -> Data.Monoid.mempty
                                                                                                                         (Prelude.Just _v)
                                                                                                                           -> (Data.Monoid.<>)
                                                                                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                   1744)
                                                                                                                                ((Prelude..)
                                                                                                                                   ((Prelude..)
                                                                                                                                      Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                      Prelude.fromIntegral)
                                                                                                                                   Prelude.fromEnum
                                                                                                                                   _v))
                                                                                                                      ((Data.Monoid.<>)
                                                                                                                         (let
                                                                                                                            p = Lens.Family2.view
                                                                                                                                  (Data.ProtoLens.Field.field
                                                                                                                                     @"vec'repeatedScalar01")
                                                                                                                                  _x
                                                                                                                          in
                                                                                                                            if Data.Vector.Generic.null
                                                                                                                                 p then
                                                                                                                                Data.Monoid.mempty
                                                                                                                            else
                                                                                                                                (Data.Monoid.<>)
                                                                                                                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                     2410)
                                                                                                                                  ((\ bs
                                                                                                                                      -> (Data.Monoid.<>)
                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                              (Prelude.fromIntegral
                                                                                                                                                 (Data.ByteString.length
                                                                                                                                                    bs)))
                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                              bs))
                                                                                                                                     (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                           ((Prelude..)
                                                                                                                                              Data.ProtoLens.Encoding.Bytes.putFixed64
                                                                                                                                              Data.ProtoLens.Encoding.Bytes.doubleToWord)
                                                                                                                                           p))))
                                                                                                                         ((Data.Monoid.<>)
                                                                                                                            (let
                                                                                                                               p = Lens.Family2.view
                                                                                                                                     (Data.ProtoLens.Field.field
                                                                                                                                        @"vec'repeatedScalar02")
                                                                                                                                     _x
                                                                                                                             in
                                                                                                                               if Data.Vector.Generic.null
                                                                                                                                    p then
                                                                                                                                   Data.Monoid.mempty
                                                                                                                               else
                                                                                                                                   (Data.Monoid.<>)
                                                                                                                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                        2418)
                                                                                                                                     ((\ bs
                                                                                                                                         -> (Data.Monoid.<>)
                                                                                                                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                 (Prelude.fromIntegral
                                                                                                                                                    (Data.ByteString.length
                                                                                                                                                       bs)))
                                                                                                                                              (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                 bs))
                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                              ((Prelude..)
                                                                                                                                                 Data.ProtoLens.Encoding.Bytes.putFixed32
                                                                                                                                                 Data.ProtoLens.Encoding.Bytes.floatToWord)
                                                                                                                                              p))))
                                                                                                                            ((Data.Monoid.<>)
                                                                                                                               (let
                                                                                                                                  p = Lens.Family2.view
                                                                                                                                        (Data.ProtoLens.Field.field
                                                                                                                                           @"vec'repeatedScalar03")
                                                                                                                                        _x
                                                                                                                                in
                                                                                                                                  if Data.Vector.Generic.null
                                                                                                                                       p then
                                                                                                                                      Data.Monoid.mempty
                                                                                                                                  else
                                                                                                                                      (Data.Monoid.<>)
                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                           2426)
                                                                                                                                        ((\ bs
                                                                                                                                            -> (Data.Monoid.<>)
                                                                                                                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                    (Prelude.fromIntegral
                                                                                                                                                       (Data.ByteString.length
                                                                                                                                                          bs)))
                                                                                                                                                 (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                    bs))
                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                              (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                 ((Prelude..)
                                                                                                                                                    Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                    Prelude.fromIntegral)
                                                                                                                                                 p))))
                                                                                                                               ((Data.Monoid.<>)
                                                                                                                                  (let
                                                                                                                                     p = Lens.Family2.view
                                                                                                                                           (Data.ProtoLens.Field.field
                                                                                                                                              @"vec'repeatedScalar04")
                                                                                                                                           _x
                                                                                                                                   in
                                                                                                                                     if Data.Vector.Generic.null
                                                                                                                                          p then
                                                                                                                                         Data.Monoid.mempty
                                                                                                                                     else
                                                                                                                                         (Data.Monoid.<>)
                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                              2434)
                                                                                                                                           ((\ bs
                                                                                                                                               -> (Data.Monoid.<>)
                                                                                                                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                       (Prelude.fromIntegral
                                                                                                                                                          (Data.ByteString.length
                                                                                                                                                             bs)))
                                                                                                                                                    (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                       bs))
                                                                                                                                              (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                                 (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                    ((Prelude..)
                                                                                                                                                       Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                       Prelude.fromIntegral)
                                                                                                                                                    p))))
                                                                                                                                  ((Data.Monoid.<>)
                                                                                                                                     (let
                                                                                                                                        p = Lens.Family2.view
                                                                                                                                              (Data.ProtoLens.Field.field
                                                                                                                                                 @"vec'repeatedScalar05")
                                                                                                                                              _x
                                                                                                                                      in
                                                                                                                                        if Data.Vector.Generic.null
                                                                                                                                             p then
                                                                                                                                            Data.Monoid.mempty
                                                                                                                                        else
                                                                                                                                            (Data.Monoid.<>)
                                                                                                                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                 2442)
                                                                                                                                              ((\ bs
                                                                                                                                                  -> (Data.Monoid.<>)
                                                                                                                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                          (Prelude.fromIntegral
                                                                                                                                                             (Data.ByteString.length
                                                                                                                                                                bs)))
                                                                                                                                                       (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                          bs))
                                                                                                                                                 (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                                    (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                       ((Prelude..)
                                                                                                                                                          Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                          Prelude.fromIntegral)
                                                                                                                                                       p))))
                                                                                                                                     ((Data.Monoid.<>)
                                                                                                                                        (let
                                                                                                                                           p = Lens.Family2.view
                                                                                                                                                 (Data.ProtoLens.Field.field
                                                                                                                                                    @"vec'repeatedScalar06")
                                                                                                                                                 _x
                                                                                                                                         in
                                                                                                                                           if Data.Vector.Generic.null
                                                                                                                                                p then
                                                                                                                                               Data.Monoid.mempty
                                                                                                                                           else
                                                                                                                                               (Data.Monoid.<>)
                                                                                                                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                    2450)
                                                                                                                                                 ((\ bs
                                                                                                                                                     -> (Data.Monoid.<>)
                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                             (Prelude.fromIntegral
                                                                                                                                                                (Data.ByteString.length
                                                                                                                                                                   bs)))
                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                             bs))
                                                                                                                                                    (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                                       (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                          Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                          p))))
                                                                                                                                        ((Data.Monoid.<>)
                                                                                                                                           (let
                                                                                                                                              p = Lens.Family2.view
                                                                                                                                                    (Data.ProtoLens.Field.field
                                                                                                                                                       @"vec'repeatedScalar07")
                                                                                                                                                    _x
                                                                                                                                            in
                                                                                                                                              if Data.Vector.Generic.null
                                                                                                                                                   p then
                                                                                                                                                  Data.Monoid.mempty
                                                                                                                                              else
                                                                                                                                                  (Data.Monoid.<>)
                                                                                                                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                       2458)
                                                                                                                                                    ((\ bs
                                                                                                                                                        -> (Data.Monoid.<>)
                                                                                                                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                (Prelude.fromIntegral
                                                                                                                                                                   (Data.ByteString.length
                                                                                                                                                                      bs)))
                                                                                                                                                             (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                bs))
                                                                                                                                                       (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                             ((Prelude..)
                                                                                                                                                                ((Prelude..)
                                                                                                                                                                   Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                   Prelude.fromIntegral)
                                                                                                                                                                Data.ProtoLens.Encoding.Bytes.signedInt32ToWord)
                                                                                                                                                             p))))
                                                                                                                                           ((Data.Monoid.<>)
                                                                                                                                              (let
                                                                                                                                                 p = Lens.Family2.view
                                                                                                                                                       (Data.ProtoLens.Field.field
                                                                                                                                                          @"vec'repeatedScalar08")
                                                                                                                                                       _x
                                                                                                                                               in
                                                                                                                                                 if Data.Vector.Generic.null
                                                                                                                                                      p then
                                                                                                                                                     Data.Monoid.mempty
                                                                                                                                                 else
                                                                                                                                                     (Data.Monoid.<>)
                                                                                                                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                          2466)
                                                                                                                                                       ((\ bs
                                                                                                                                                           -> (Data.Monoid.<>)
                                                                                                                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                   (Prelude.fromIntegral
                                                                                                                                                                      (Data.ByteString.length
                                                                                                                                                                         bs)))
                                                                                                                                                                (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                   bs))
                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                                             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                                ((Prelude..)
                                                                                                                                                                   ((Prelude..)
                                                                                                                                                                      Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                      Prelude.fromIntegral)
                                                                                                                                                                   Data.ProtoLens.Encoding.Bytes.signedInt64ToWord)
                                                                                                                                                                p))))
                                                                                                                                              ((Data.Monoid.<>)
                                                                                                                                                 (let
                                                                                                                                                    p = Lens.Family2.view
                                                                                                                                                          (Data.ProtoLens.Field.field
                                                                                                                                                             @"vec'repeatedScalar09")
                                                                                                                                                          _x
                                                                                                                                                  in
                                                                                                                                                    if Data.Vector.Generic.null
                                                                                                                                                         p then
                                                                                                                                                        Data.Monoid.mempty
                                                                                                                                                    else
                                                                                                                                                        (Data.Monoid.<>)
                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                             2474)
                                                                                                                                                          ((\ bs
                                                                                                                                                              -> (Data.Monoid.<>)
                                                                                                                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                      (Prelude.fromIntegral
                                                                                                                                                                         (Data.ByteString.length
                                                                                                                                                                            bs)))
                                                                                                                                                                   (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                      bs))
                                                                                                                                                             (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                                                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                                   Data.ProtoLens.Encoding.Bytes.putFixed32
                                                                                                                                                                   p))))
                                                                                                                                                 ((Data.Monoid.<>)
                                                                                                                                                    (let
                                                                                                                                                       p = Lens.Family2.view
                                                                                                                                                             (Data.ProtoLens.Field.field
                                                                                                                                                                @"vec'repeatedScalar10")
                                                                                                                                                             _x
                                                                                                                                                     in
                                                                                                                                                       if Data.Vector.Generic.null
                                                                                                                                                            p then
                                                                                                                                                           Data.Monoid.mempty
                                                                                                                                                       else
                                                                                                                                                           (Data.Monoid.<>)
                                                                                                                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                2482)
                                                                                                                                                             ((\ bs
                                                                                                                                                                 -> (Data.Monoid.<>)
                                                                                                                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                         (Prelude.fromIntegral
                                                                                                                                                                            (Data.ByteString.length
                                                                                                                                                                               bs)))
                                                                                                                                                                      (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                         bs))
                                                                                                                                                                (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                                                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                                      Data.ProtoLens.Encoding.Bytes.putFixed64
                                                                                                                                                                      p))))
                                                                                                                                                    ((Data.Monoid.<>)
                                                                                                                                                       (let
                                                                                                                                                          p = Lens.Family2.view
                                                                                                                                                                (Data.ProtoLens.Field.field
                                                                                                                                                                   @"vec'repeatedScalar11")
                                                                                                                                                                _x
                                                                                                                                                        in
                                                                                                                                                          if Data.Vector.Generic.null
                                                                                                                                                               p then
                                                                                                                                                              Data.Monoid.mempty
                                                                                                                                                          else
                                                                                                                                                              (Data.Monoid.<>)
                                                                                                                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                   2490)
                                                                                                                                                                ((\ bs
                                                                                                                                                                    -> (Data.Monoid.<>)
                                                                                                                                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                            (Prelude.fromIntegral
                                                                                                                                                                               (Data.ByteString.length
                                                                                                                                                                                  bs)))
                                                                                                                                                                         (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                            bs))
                                                                                                                                                                   (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                                                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                                         ((Prelude..)
                                                                                                                                                                            Data.ProtoLens.Encoding.Bytes.putFixed32
                                                                                                                                                                            Prelude.fromIntegral)
                                                                                                                                                                         p))))
                                                                                                                                                       ((Data.Monoid.<>)
                                                                                                                                                          (let
                                                                                                                                                             p = Lens.Family2.view
                                                                                                                                                                   (Data.ProtoLens.Field.field
                                                                                                                                                                      @"vec'repeatedScalar12")
                                                                                                                                                                   _x
                                                                                                                                                           in
                                                                                                                                                             if Data.Vector.Generic.null
                                                                                                                                                                  p then
                                                                                                                                                                 Data.Monoid.mempty
                                                                                                                                                             else
                                                                                                                                                                 (Data.Monoid.<>)
                                                                                                                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                      2498)
                                                                                                                                                                   ((\ bs
                                                                                                                                                                       -> (Data.Monoid.<>)
                                                                                                                                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                               (Prelude.fromIntegral
                                                                                                                                                                                  (Data.ByteString.length
                                                                                                                                                                                     bs)))
                                                                                                                                                                            (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                               bs))
                                                                                                                                                                      (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                                                         (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                                            ((Prelude..)
                                                                                                                                                                               Data.ProtoLens.Encoding.Bytes.putFixed64
                                                                                                                                                                               Prelude.fromIntegral)
                                                                                                                                                                            p))))
                                                                                                                                                          ((Data.Monoid.<>)
                                                                                                                                                             (let
                                                                                                                                                                p = Lens.Family2.view
                                                                                                                                                                      (Data.ProtoLens.Field.field
                                                                                                                                                                         @"vec'repeatedScalar13")
                                                                                                                                                                      _x
                                                                                                                                                              in
                                                                                                                                                                if Data.Vector.Generic.null
                                                                                                                                                                     p then
                                                                                                                                                                    Data.Monoid.mempty
                                                                                                                                                                else
                                                                                                                                                                    (Data.Monoid.<>)
                                                                                                                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                         2506)
                                                                                                                                                                      ((\ bs
                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                  (Prelude.fromIntegral
                                                                                                                                                                                     (Data.ByteString.length
                                                                                                                                                                                        bs)))
                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                  bs))
                                                                                                                                                                         (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                                                            (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                  (\ b
                                                                                                                                                                                     -> if b then
                                                                                                                                                                                            1
                                                                                                                                                                                        else
                                                                                                                                                                                            0))
                                                                                                                                                                               p))))
                                                                                                                                                             ((Data.Monoid.<>)
                                                                                                                                                                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                                   (\ _v
                                                                                                                                                                      -> (Data.Monoid.<>)
                                                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                              2514)
                                                                                                                                                                           ((Prelude..)
                                                                                                                                                                              (\ bs
                                                                                                                                                                                 -> (Data.Monoid.<>)
                                                                                                                                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                         (Prelude.fromIntegral
                                                                                                                                                                                            (Data.ByteString.length
                                                                                                                                                                                               bs)))
                                                                                                                                                                                      (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                         bs))
                                                                                                                                                                              Data.Text.Encoding.encodeUtf8
                                                                                                                                                                              _v))
                                                                                                                                                                   (Lens.Family2.view
                                                                                                                                                                      (Data.ProtoLens.Field.field
                                                                                                                                                                         @"vec'repeatedScalar14")
                                                                                                                                                                      _x))
                                                                                                                                                                ((Data.Monoid.<>)
                                                                                                                                                                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                                      (\ _v
                                                                                                                                                                         -> (Data.Monoid.<>)
                                                                                                                                                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                 2522)
                                                                                                                                                                              ((\ bs
                                                                                                                                                                                  -> (Data.Monoid.<>)
                                                                                                                                                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                          (Prelude.fromIntegral
                                                                                                                                                                                             (Data.ByteString.length
                                                                                                                                                                                                bs)))
                                                                                                                                                                                       (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                          bs))
                                                                                                                                                                                 _v))
                                                                                                                                                                      (Lens.Family2.view
                                                                                                                                                                         (Data.ProtoLens.Field.field
                                                                                                                                                                            @"vec'repeatedScalar15")
                                                                                                                                                                         _x))
                                                                                                                                                                   ((Data.Monoid.<>)
                                                                                                                                                                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                                         (\ _v
                                                                                                                                                                            -> (Data.Monoid.<>)
                                                                                                                                                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                    2530)
                                                                                                                                                                                 ((Prelude..)
                                                                                                                                                                                    (\ bs
                                                                                                                                                                                       -> (Data.Monoid.<>)
                                                                                                                                                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                               (Prelude.fromIntegral
                                                                                                                                                                                                  (Data.ByteString.length
                                                                                                                                                                                                     bs)))
                                                                                                                                                                                            (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                               bs))
                                                                                                                                                                                    Data.ProtoLens.encodeMessage
                                                                                                                                                                                    _v))
                                                                                                                                                                         (Lens.Family2.view
                                                                                                                                                                            (Data.ProtoLens.Field.field
                                                                                                                                                                               @"vec'repeatedAnother")
                                                                                                                                                                            _x))
                                                                                                                                                                      ((Data.Monoid.<>)
                                                                                                                                                                         (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                                            (\ _v
                                                                                                                                                                               -> (Data.Monoid.<>)
                                                                                                                                                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                       2538)
                                                                                                                                                                                    ((Prelude..)
                                                                                                                                                                                       (\ bs
                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                  (Prelude.fromIntegral
                                                                                                                                                                                                     (Data.ByteString.length
                                                                                                                                                                                                        bs)))
                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                  bs))
                                                                                                                                                                                       Data.ProtoLens.encodeMessage
                                                                                                                                                                                       _v))
                                                                                                                                                                            (Lens.Family2.view
                                                                                                                                                                               (Data.ProtoLens.Field.field
                                                                                                                                                                                  @"vec'repeatedNested")
                                                                                                                                                                               _x))
                                                                                                                                                                         ((Data.Monoid.<>)
                                                                                                                                                                            (let
                                                                                                                                                                               p = Lens.Family2.view
                                                                                                                                                                                     (Data.ProtoLens.Field.field
                                                                                                                                                                                        @"vec'repeatedEnum")
                                                                                                                                                                                     _x
                                                                                                                                                                             in
                                                                                                                                                                               if Data.Vector.Generic.null
                                                                                                                                                                                    p then
                                                                                                                                                                                   Data.Monoid.mempty
                                                                                                                                                                               else
                                                                                                                                                                                   (Data.Monoid.<>)
                                                                                                                                                                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                        2546)
                                                                                                                                                                                     ((\ bs
                                                                                                                                                                                         -> (Data.Monoid.<>)
                                                                                                                                                                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                 (Prelude.fromIntegral
                                                                                                                                                                                                    (Data.ByteString.length
                                                                                                                                                                                                       bs)))
                                                                                                                                                                                              (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                 bs))
                                                                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                                                                                                                                              ((Prelude..)
                                                                                                                                                                                                 ((Prelude..)
                                                                                                                                                                                                    Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                    Prelude.fromIntegral)
                                                                                                                                                                                                 Prelude.fromEnum)
                                                                                                                                                                                              p))))
                                                                                                                                                                            ((Data.Monoid.<>)
                                                                                                                                                                               (Data.Monoid.mconcat
                                                                                                                                                                                  (Prelude.map
                                                                                                                                                                                     (\ _v
                                                                                                                                                                                        -> (Data.Monoid.<>)
                                                                                                                                                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                3210)
                                                                                                                                                                                             ((Prelude..)
                                                                                                                                                                                                (\ bs
                                                                                                                                                                                                   -> (Data.Monoid.<>)
                                                                                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                           (Prelude.fromIntegral
                                                                                                                                                                                                              (Data.ByteString.length
                                                                                                                                                                                                                 bs)))
                                                                                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                           bs))
                                                                                                                                                                                                Data.ProtoLens.encodeMessage
                                                                                                                                                                                                (Lens.Family2.set
                                                                                                                                                                                                   (Data.ProtoLens.Field.field
                                                                                                                                                                                                      @"key")
                                                                                                                                                                                                   (Prelude.fst
                                                                                                                                                                                                      _v)
                                                                                                                                                                                                   (Lens.Family2.set
                                                                                                                                                                                                      (Data.ProtoLens.Field.field
                                                                                                                                                                                                         @"value")
                                                                                                                                                                                                      (Prelude.snd
                                                                                                                                                                                                         _v)
                                                                                                                                                                                                      (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                         ExampleMessage'MapScalar01Entry)))))
                                                                                                                                                                                     (Data.Map.toList
                                                                                                                                                                                        (Lens.Family2.view
                                                                                                                                                                                           (Data.ProtoLens.Field.field
                                                                                                                                                                                              @"mapScalar01")
                                                                                                                                                                                           _x))))
                                                                                                                                                                               ((Data.Monoid.<>)
                                                                                                                                                                                  (Data.Monoid.mconcat
                                                                                                                                                                                     (Prelude.map
                                                                                                                                                                                        (\ _v
                                                                                                                                                                                           -> (Data.Monoid.<>)
                                                                                                                                                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                   3218)
                                                                                                                                                                                                ((Prelude..)
                                                                                                                                                                                                   (\ bs
                                                                                                                                                                                                      -> (Data.Monoid.<>)
                                                                                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                              (Prelude.fromIntegral
                                                                                                                                                                                                                 (Data.ByteString.length
                                                                                                                                                                                                                    bs)))
                                                                                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                              bs))
                                                                                                                                                                                                   Data.ProtoLens.encodeMessage
                                                                                                                                                                                                   (Lens.Family2.set
                                                                                                                                                                                                      (Data.ProtoLens.Field.field
                                                                                                                                                                                                         @"key")
                                                                                                                                                                                                      (Prelude.fst
                                                                                                                                                                                                         _v)
                                                                                                                                                                                                      (Lens.Family2.set
                                                                                                                                                                                                         (Data.ProtoLens.Field.field
                                                                                                                                                                                                            @"value")
                                                                                                                                                                                                         (Prelude.snd
                                                                                                                                                                                                            _v)
                                                                                                                                                                                                         (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                            ExampleMessage'MapScalar02Entry)))))
                                                                                                                                                                                        (Data.Map.toList
                                                                                                                                                                                           (Lens.Family2.view
                                                                                                                                                                                              (Data.ProtoLens.Field.field
                                                                                                                                                                                                 @"mapScalar02")
                                                                                                                                                                                              _x))))
                                                                                                                                                                                  ((Data.Monoid.<>)
                                                                                                                                                                                     (Data.Monoid.mconcat
                                                                                                                                                                                        (Prelude.map
                                                                                                                                                                                           (\ _v
                                                                                                                                                                                              -> (Data.Monoid.<>)
                                                                                                                                                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                      3226)
                                                                                                                                                                                                   ((Prelude..)
                                                                                                                                                                                                      (\ bs
                                                                                                                                                                                                         -> (Data.Monoid.<>)
                                                                                                                                                                                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                 (Prelude.fromIntegral
                                                                                                                                                                                                                    (Data.ByteString.length
                                                                                                                                                                                                                       bs)))
                                                                                                                                                                                                              (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                 bs))
                                                                                                                                                                                                      Data.ProtoLens.encodeMessage
                                                                                                                                                                                                      (Lens.Family2.set
                                                                                                                                                                                                         (Data.ProtoLens.Field.field
                                                                                                                                                                                                            @"key")
                                                                                                                                                                                                         (Prelude.fst
                                                                                                                                                                                                            _v)
                                                                                                                                                                                                         (Lens.Family2.set
                                                                                                                                                                                                            (Data.ProtoLens.Field.field
                                                                                                                                                                                                               @"value")
                                                                                                                                                                                                            (Prelude.snd
                                                                                                                                                                                                               _v)
                                                                                                                                                                                                            (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                               ExampleMessage'MapScalar03Entry)))))
                                                                                                                                                                                           (Data.Map.toList
                                                                                                                                                                                              (Lens.Family2.view
                                                                                                                                                                                                 (Data.ProtoLens.Field.field
                                                                                                                                                                                                    @"mapScalar03")
                                                                                                                                                                                                 _x))))
                                                                                                                                                                                     ((Data.Monoid.<>)
                                                                                                                                                                                        (Data.Monoid.mconcat
                                                                                                                                                                                           (Prelude.map
                                                                                                                                                                                              (\ _v
                                                                                                                                                                                                 -> (Data.Monoid.<>)
                                                                                                                                                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                         3234)
                                                                                                                                                                                                      ((Prelude..)
                                                                                                                                                                                                         (\ bs
                                                                                                                                                                                                            -> (Data.Monoid.<>)
                                                                                                                                                                                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                    (Prelude.fromIntegral
                                                                                                                                                                                                                       (Data.ByteString.length
                                                                                                                                                                                                                          bs)))
                                                                                                                                                                                                                 (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                    bs))
                                                                                                                                                                                                         Data.ProtoLens.encodeMessage
                                                                                                                                                                                                         (Lens.Family2.set
                                                                                                                                                                                                            (Data.ProtoLens.Field.field
                                                                                                                                                                                                               @"key")
                                                                                                                                                                                                            (Prelude.fst
                                                                                                                                                                                                               _v)
                                                                                                                                                                                                            (Lens.Family2.set
                                                                                                                                                                                                               (Data.ProtoLens.Field.field
                                                                                                                                                                                                                  @"value")
                                                                                                                                                                                                               (Prelude.snd
                                                                                                                                                                                                                  _v)
                                                                                                                                                                                                               (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                  ExampleMessage'MapScalar04Entry)))))
                                                                                                                                                                                              (Data.Map.toList
                                                                                                                                                                                                 (Lens.Family2.view
                                                                                                                                                                                                    (Data.ProtoLens.Field.field
                                                                                                                                                                                                       @"mapScalar04")
                                                                                                                                                                                                    _x))))
                                                                                                                                                                                        ((Data.Monoid.<>)
                                                                                                                                                                                           (Data.Monoid.mconcat
                                                                                                                                                                                              (Prelude.map
                                                                                                                                                                                                 (\ _v
                                                                                                                                                                                                    -> (Data.Monoid.<>)
                                                                                                                                                                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                            3242)
                                                                                                                                                                                                         ((Prelude..)
                                                                                                                                                                                                            (\ bs
                                                                                                                                                                                                               -> (Data.Monoid.<>)
                                                                                                                                                                                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                       (Prelude.fromIntegral
                                                                                                                                                                                                                          (Data.ByteString.length
                                                                                                                                                                                                                             bs)))
                                                                                                                                                                                                                    (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                       bs))
                                                                                                                                                                                                            Data.ProtoLens.encodeMessage
                                                                                                                                                                                                            (Lens.Family2.set
                                                                                                                                                                                                               (Data.ProtoLens.Field.field
                                                                                                                                                                                                                  @"key")
                                                                                                                                                                                                               (Prelude.fst
                                                                                                                                                                                                                  _v)
                                                                                                                                                                                                               (Lens.Family2.set
                                                                                                                                                                                                                  (Data.ProtoLens.Field.field
                                                                                                                                                                                                                     @"value")
                                                                                                                                                                                                                  (Prelude.snd
                                                                                                                                                                                                                     _v)
                                                                                                                                                                                                                  (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                     ExampleMessage'MapScalar05Entry)))))
                                                                                                                                                                                                 (Data.Map.toList
                                                                                                                                                                                                    (Lens.Family2.view
                                                                                                                                                                                                       (Data.ProtoLens.Field.field
                                                                                                                                                                                                          @"mapScalar05")
                                                                                                                                                                                                       _x))))
                                                                                                                                                                                           ((Data.Monoid.<>)
                                                                                                                                                                                              (Data.Monoid.mconcat
                                                                                                                                                                                                 (Prelude.map
                                                                                                                                                                                                    (\ _v
                                                                                                                                                                                                       -> (Data.Monoid.<>)
                                                                                                                                                                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                               3250)
                                                                                                                                                                                                            ((Prelude..)
                                                                                                                                                                                                               (\ bs
                                                                                                                                                                                                                  -> (Data.Monoid.<>)
                                                                                                                                                                                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                          (Prelude.fromIntegral
                                                                                                                                                                                                                             (Data.ByteString.length
                                                                                                                                                                                                                                bs)))
                                                                                                                                                                                                                       (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                          bs))
                                                                                                                                                                                                               Data.ProtoLens.encodeMessage
                                                                                                                                                                                                               (Lens.Family2.set
                                                                                                                                                                                                                  (Data.ProtoLens.Field.field
                                                                                                                                                                                                                     @"key")
                                                                                                                                                                                                                  (Prelude.fst
                                                                                                                                                                                                                     _v)
                                                                                                                                                                                                                  (Lens.Family2.set
                                                                                                                                                                                                                     (Data.ProtoLens.Field.field
                                                                                                                                                                                                                        @"value")
                                                                                                                                                                                                                     (Prelude.snd
                                                                                                                                                                                                                        _v)
                                                                                                                                                                                                                     (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                        ExampleMessage'MapScalar06Entry)))))
                                                                                                                                                                                                    (Data.Map.toList
                                                                                                                                                                                                       (Lens.Family2.view
                                                                                                                                                                                                          (Data.ProtoLens.Field.field
                                                                                                                                                                                                             @"mapScalar06")
                                                                                                                                                                                                          _x))))
                                                                                                                                                                                              ((Data.Monoid.<>)
                                                                                                                                                                                                 (Data.Monoid.mconcat
                                                                                                                                                                                                    (Prelude.map
                                                                                                                                                                                                       (\ _v
                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                  3258)
                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                  (\ bs
                                                                                                                                                                                                                     -> (Data.Monoid.<>)
                                                                                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                             (Prelude.fromIntegral
                                                                                                                                                                                                                                (Data.ByteString.length
                                                                                                                                                                                                                                   bs)))
                                                                                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                             bs))
                                                                                                                                                                                                                  Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                  (Lens.Family2.set
                                                                                                                                                                                                                     (Data.ProtoLens.Field.field
                                                                                                                                                                                                                        @"key")
                                                                                                                                                                                                                     (Prelude.fst
                                                                                                                                                                                                                        _v)
                                                                                                                                                                                                                     (Lens.Family2.set
                                                                                                                                                                                                                        (Data.ProtoLens.Field.field
                                                                                                                                                                                                                           @"value")
                                                                                                                                                                                                                        (Prelude.snd
                                                                                                                                                                                                                           _v)
                                                                                                                                                                                                                        (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                           ExampleMessage'MapScalar07Entry)))))
                                                                                                                                                                                                       (Data.Map.toList
                                                                                                                                                                                                          (Lens.Family2.view
                                                                                                                                                                                                             (Data.ProtoLens.Field.field
                                                                                                                                                                                                                @"mapScalar07")
                                                                                                                                                                                                             _x))))
                                                                                                                                                                                                 ((Data.Monoid.<>)
                                                                                                                                                                                                    (Data.Monoid.mconcat
                                                                                                                                                                                                       (Prelude.map
                                                                                                                                                                                                          (\ _v
                                                                                                                                                                                                             -> (Data.Monoid.<>)
                                                                                                                                                                                                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                     3266)
                                                                                                                                                                                                                  ((Prelude..)
                                                                                                                                                                                                                     (\ bs
                                                                                                                                                                                                                        -> (Data.Monoid.<>)
                                                                                                                                                                                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                (Prelude.fromIntegral
                                                                                                                                                                                                                                   (Data.ByteString.length
                                                                                                                                                                                                                                      bs)))
                                                                                                                                                                                                                             (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                bs))
                                                                                                                                                                                                                     Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                     (Lens.Family2.set
                                                                                                                                                                                                                        (Data.ProtoLens.Field.field
                                                                                                                                                                                                                           @"key")
                                                                                                                                                                                                                        (Prelude.fst
                                                                                                                                                                                                                           _v)
                                                                                                                                                                                                                        (Lens.Family2.set
                                                                                                                                                                                                                           (Data.ProtoLens.Field.field
                                                                                                                                                                                                                              @"value")
                                                                                                                                                                                                                           (Prelude.snd
                                                                                                                                                                                                                              _v)
                                                                                                                                                                                                                           (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                              ExampleMessage'MapScalar08Entry)))))
                                                                                                                                                                                                          (Data.Map.toList
                                                                                                                                                                                                             (Lens.Family2.view
                                                                                                                                                                                                                (Data.ProtoLens.Field.field
                                                                                                                                                                                                                   @"mapScalar08")
                                                                                                                                                                                                                _x))))
                                                                                                                                                                                                    ((Data.Monoid.<>)
                                                                                                                                                                                                       (Data.Monoid.mconcat
                                                                                                                                                                                                          (Prelude.map
                                                                                                                                                                                                             (\ _v
                                                                                                                                                                                                                -> (Data.Monoid.<>)
                                                                                                                                                                                                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                        3274)
                                                                                                                                                                                                                     ((Prelude..)
                                                                                                                                                                                                                        (\ bs
                                                                                                                                                                                                                           -> (Data.Monoid.<>)
                                                                                                                                                                                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                   (Prelude.fromIntegral
                                                                                                                                                                                                                                      (Data.ByteString.length
                                                                                                                                                                                                                                         bs)))
                                                                                                                                                                                                                                (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                   bs))
                                                                                                                                                                                                                        Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                        (Lens.Family2.set
                                                                                                                                                                                                                           (Data.ProtoLens.Field.field
                                                                                                                                                                                                                              @"key")
                                                                                                                                                                                                                           (Prelude.fst
                                                                                                                                                                                                                              _v)
                                                                                                                                                                                                                           (Lens.Family2.set
                                                                                                                                                                                                                              (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                 @"value")
                                                                                                                                                                                                                              (Prelude.snd
                                                                                                                                                                                                                                 _v)
                                                                                                                                                                                                                              (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                                 ExampleMessage'MapScalar09Entry)))))
                                                                                                                                                                                                             (Data.Map.toList
                                                                                                                                                                                                                (Lens.Family2.view
                                                                                                                                                                                                                   (Data.ProtoLens.Field.field
                                                                                                                                                                                                                      @"mapScalar09")
                                                                                                                                                                                                                   _x))))
                                                                                                                                                                                                       ((Data.Monoid.<>)
                                                                                                                                                                                                          (Data.Monoid.mconcat
                                                                                                                                                                                                             (Prelude.map
                                                                                                                                                                                                                (\ _v
                                                                                                                                                                                                                   -> (Data.Monoid.<>)
                                                                                                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                           3282)
                                                                                                                                                                                                                        ((Prelude..)
                                                                                                                                                                                                                           (\ bs
                                                                                                                                                                                                                              -> (Data.Monoid.<>)
                                                                                                                                                                                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                      (Prelude.fromIntegral
                                                                                                                                                                                                                                         (Data.ByteString.length
                                                                                                                                                                                                                                            bs)))
                                                                                                                                                                                                                                   (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                      bs))
                                                                                                                                                                                                                           Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                           (Lens.Family2.set
                                                                                                                                                                                                                              (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                 @"key")
                                                                                                                                                                                                                              (Prelude.fst
                                                                                                                                                                                                                                 _v)
                                                                                                                                                                                                                              (Lens.Family2.set
                                                                                                                                                                                                                                 (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                    @"value")
                                                                                                                                                                                                                                 (Prelude.snd
                                                                                                                                                                                                                                    _v)
                                                                                                                                                                                                                                 (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                                    ExampleMessage'MapScalar10Entry)))))
                                                                                                                                                                                                                (Data.Map.toList
                                                                                                                                                                                                                   (Lens.Family2.view
                                                                                                                                                                                                                      (Data.ProtoLens.Field.field
                                                                                                                                                                                                                         @"mapScalar10")
                                                                                                                                                                                                                      _x))))
                                                                                                                                                                                                          ((Data.Monoid.<>)
                                                                                                                                                                                                             (Data.Monoid.mconcat
                                                                                                                                                                                                                (Prelude.map
                                                                                                                                                                                                                   (\ _v
                                                                                                                                                                                                                      -> (Data.Monoid.<>)
                                                                                                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                              3290)
                                                                                                                                                                                                                           ((Prelude..)
                                                                                                                                                                                                                              (\ bs
                                                                                                                                                                                                                                 -> (Data.Monoid.<>)
                                                                                                                                                                                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                         (Prelude.fromIntegral
                                                                                                                                                                                                                                            (Data.ByteString.length
                                                                                                                                                                                                                                               bs)))
                                                                                                                                                                                                                                      (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                         bs))
                                                                                                                                                                                                                              Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                              (Lens.Family2.set
                                                                                                                                                                                                                                 (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                    @"key")
                                                                                                                                                                                                                                 (Prelude.fst
                                                                                                                                                                                                                                    _v)
                                                                                                                                                                                                                                 (Lens.Family2.set
                                                                                                                                                                                                                                    (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                       @"value")
                                                                                                                                                                                                                                    (Prelude.snd
                                                                                                                                                                                                                                       _v)
                                                                                                                                                                                                                                    (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                                       ExampleMessage'MapScalar11Entry)))))
                                                                                                                                                                                                                   (Data.Map.toList
                                                                                                                                                                                                                      (Lens.Family2.view
                                                                                                                                                                                                                         (Data.ProtoLens.Field.field
                                                                                                                                                                                                                            @"mapScalar11")
                                                                                                                                                                                                                         _x))))
                                                                                                                                                                                                             ((Data.Monoid.<>)
                                                                                                                                                                                                                (Data.Monoid.mconcat
                                                                                                                                                                                                                   (Prelude.map
                                                                                                                                                                                                                      (\ _v
                                                                                                                                                                                                                         -> (Data.Monoid.<>)
                                                                                                                                                                                                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                 3298)
                                                                                                                                                                                                                              ((Prelude..)
                                                                                                                                                                                                                                 (\ bs
                                                                                                                                                                                                                                    -> (Data.Monoid.<>)
                                                                                                                                                                                                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                            (Prelude.fromIntegral
                                                                                                                                                                                                                                               (Data.ByteString.length
                                                                                                                                                                                                                                                  bs)))
                                                                                                                                                                                                                                         (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                            bs))
                                                                                                                                                                                                                                 Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                                 (Lens.Family2.set
                                                                                                                                                                                                                                    (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                       @"key")
                                                                                                                                                                                                                                    (Prelude.fst
                                                                                                                                                                                                                                       _v)
                                                                                                                                                                                                                                    (Lens.Family2.set
                                                                                                                                                                                                                                       (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                          @"value")
                                                                                                                                                                                                                                       (Prelude.snd
                                                                                                                                                                                                                                          _v)
                                                                                                                                                                                                                                       (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                                          ExampleMessage'MapScalar12Entry)))))
                                                                                                                                                                                                                      (Data.Map.toList
                                                                                                                                                                                                                         (Lens.Family2.view
                                                                                                                                                                                                                            (Data.ProtoLens.Field.field
                                                                                                                                                                                                                               @"mapScalar12")
                                                                                                                                                                                                                            _x))))
                                                                                                                                                                                                                ((Data.Monoid.<>)
                                                                                                                                                                                                                   (Data.Monoid.mconcat
                                                                                                                                                                                                                      (Prelude.map
                                                                                                                                                                                                                         (\ _v
                                                                                                                                                                                                                            -> (Data.Monoid.<>)
                                                                                                                                                                                                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                    3306)
                                                                                                                                                                                                                                 ((Prelude..)
                                                                                                                                                                                                                                    (\ bs
                                                                                                                                                                                                                                       -> (Data.Monoid.<>)
                                                                                                                                                                                                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                               (Prelude.fromIntegral
                                                                                                                                                                                                                                                  (Data.ByteString.length
                                                                                                                                                                                                                                                     bs)))
                                                                                                                                                                                                                                            (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                               bs))
                                                                                                                                                                                                                                    Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                                    (Lens.Family2.set
                                                                                                                                                                                                                                       (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                          @"key")
                                                                                                                                                                                                                                       (Prelude.fst
                                                                                                                                                                                                                                          _v)
                                                                                                                                                                                                                                       (Lens.Family2.set
                                                                                                                                                                                                                                          (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                             @"value")
                                                                                                                                                                                                                                          (Prelude.snd
                                                                                                                                                                                                                                             _v)
                                                                                                                                                                                                                                          (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                                             ExampleMessage'MapScalar13Entry)))))
                                                                                                                                                                                                                         (Data.Map.toList
                                                                                                                                                                                                                            (Lens.Family2.view
                                                                                                                                                                                                                               (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                  @"mapScalar13")
                                                                                                                                                                                                                               _x))))
                                                                                                                                                                                                                   ((Data.Monoid.<>)
                                                                                                                                                                                                                      (Data.Monoid.mconcat
                                                                                                                                                                                                                         (Prelude.map
                                                                                                                                                                                                                            (\ _v
                                                                                                                                                                                                                               -> (Data.Monoid.<>)
                                                                                                                                                                                                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                       3314)
                                                                                                                                                                                                                                    ((Prelude..)
                                                                                                                                                                                                                                       (\ bs
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  (Prelude.fromIntegral
                                                                                                                                                                                                                                                     (Data.ByteString.length
                                                                                                                                                                                                                                                        bs)))
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                                  bs))
                                                                                                                                                                                                                                       Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                                       (Lens.Family2.set
                                                                                                                                                                                                                                          (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                             @"key")
                                                                                                                                                                                                                                          (Prelude.fst
                                                                                                                                                                                                                                             _v)
                                                                                                                                                                                                                                          (Lens.Family2.set
                                                                                                                                                                                                                                             (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                                @"value")
                                                                                                                                                                                                                                             (Prelude.snd
                                                                                                                                                                                                                                                _v)
                                                                                                                                                                                                                                             (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                                                ExampleMessage'MapScalar14Entry)))))
                                                                                                                                                                                                                            (Data.Map.toList
                                                                                                                                                                                                                               (Lens.Family2.view
                                                                                                                                                                                                                                  (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                     @"mapScalar14")
                                                                                                                                                                                                                                  _x))))
                                                                                                                                                                                                                      ((Data.Monoid.<>)
                                                                                                                                                                                                                         (Data.Monoid.mconcat
                                                                                                                                                                                                                            (Prelude.map
                                                                                                                                                                                                                               (\ _v
                                                                                                                                                                                                                                  -> (Data.Monoid.<>)
                                                                                                                                                                                                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                          3322)
                                                                                                                                                                                                                                       ((Prelude..)
                                                                                                                                                                                                                                          (\ bs
                                                                                                                                                                                                                                             -> (Data.Monoid.<>)
                                                                                                                                                                                                                                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                     (Prelude.fromIntegral
                                                                                                                                                                                                                                                        (Data.ByteString.length
                                                                                                                                                                                                                                                           bs)))
                                                                                                                                                                                                                                                  (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                                     bs))
                                                                                                                                                                                                                                          Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                                          (Lens.Family2.set
                                                                                                                                                                                                                                             (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                                @"key")
                                                                                                                                                                                                                                             (Prelude.fst
                                                                                                                                                                                                                                                _v)
                                                                                                                                                                                                                                             (Lens.Family2.set
                                                                                                                                                                                                                                                (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                                   @"value")
                                                                                                                                                                                                                                                (Prelude.snd
                                                                                                                                                                                                                                                   _v)
                                                                                                                                                                                                                                                (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                                                   ExampleMessage'MapScalar15Entry)))))
                                                                                                                                                                                                                               (Data.Map.toList
                                                                                                                                                                                                                                  (Lens.Family2.view
                                                                                                                                                                                                                                     (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                        @"mapScalar15")
                                                                                                                                                                                                                                     _x))))
                                                                                                                                                                                                                         ((Data.Monoid.<>)
                                                                                                                                                                                                                            (Data.Monoid.mconcat
                                                                                                                                                                                                                               (Prelude.map
                                                                                                                                                                                                                                  (\ _v
                                                                                                                                                                                                                                     -> (Data.Monoid.<>)
                                                                                                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                             3330)
                                                                                                                                                                                                                                          ((Prelude..)
                                                                                                                                                                                                                                             (\ bs
                                                                                                                                                                                                                                                -> (Data.Monoid.<>)
                                                                                                                                                                                                                                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                        (Prelude.fromIntegral
                                                                                                                                                                                                                                                           (Data.ByteString.length
                                                                                                                                                                                                                                                              bs)))
                                                                                                                                                                                                                                                     (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                                        bs))
                                                                                                                                                                                                                                             Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                                             (Lens.Family2.set
                                                                                                                                                                                                                                                (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                                   @"key")
                                                                                                                                                                                                                                                (Prelude.fst
                                                                                                                                                                                                                                                   _v)
                                                                                                                                                                                                                                                (Lens.Family2.set
                                                                                                                                                                                                                                                   (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                                      @"value")
                                                                                                                                                                                                                                                   (Prelude.snd
                                                                                                                                                                                                                                                      _v)
                                                                                                                                                                                                                                                   (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                                                      ExampleMessage'MapAnotherEntry)))))
                                                                                                                                                                                                                                  (Data.Map.toList
                                                                                                                                                                                                                                     (Lens.Family2.view
                                                                                                                                                                                                                                        (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                           @"mapAnother")
                                                                                                                                                                                                                                        _x))))
                                                                                                                                                                                                                            ((Data.Monoid.<>)
                                                                                                                                                                                                                               (Data.Monoid.mconcat
                                                                                                                                                                                                                                  (Prelude.map
                                                                                                                                                                                                                                     (\ _v
                                                                                                                                                                                                                                        -> (Data.Monoid.<>)
                                                                                                                                                                                                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                3338)
                                                                                                                                                                                                                                             ((Prelude..)
                                                                                                                                                                                                                                                (\ bs
                                                                                                                                                                                                                                                   -> (Data.Monoid.<>)
                                                                                                                                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                           (Prelude.fromIntegral
                                                                                                                                                                                                                                                              (Data.ByteString.length
                                                                                                                                                                                                                                                                 bs)))
                                                                                                                                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                                           bs))
                                                                                                                                                                                                                                                Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                                                (Lens.Family2.set
                                                                                                                                                                                                                                                   (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                                      @"key")
                                                                                                                                                                                                                                                   (Prelude.fst
                                                                                                                                                                                                                                                      _v)
                                                                                                                                                                                                                                                   (Lens.Family2.set
                                                                                                                                                                                                                                                      (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                                         @"value")
                                                                                                                                                                                                                                                      (Prelude.snd
                                                                                                                                                                                                                                                         _v)
                                                                                                                                                                                                                                                      (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                                                         ExampleMessage'MapNestedEntry)))))
                                                                                                                                                                                                                                     (Data.Map.toList
                                                                                                                                                                                                                                        (Lens.Family2.view
                                                                                                                                                                                                                                           (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                              @"mapNested")
                                                                                                                                                                                                                                           _x))))
                                                                                                                                                                                                                               ((Data.Monoid.<>)
                                                                                                                                                                                                                                  (Data.Monoid.mconcat
                                                                                                                                                                                                                                     (Prelude.map
                                                                                                                                                                                                                                        (\ _v
                                                                                                                                                                                                                                           -> (Data.Monoid.<>)
                                                                                                                                                                                                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                   3346)
                                                                                                                                                                                                                                                ((Prelude..)
                                                                                                                                                                                                                                                   (\ bs
                                                                                                                                                                                                                                                      -> (Data.Monoid.<>)
                                                                                                                                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                              (Prelude.fromIntegral
                                                                                                                                                                                                                                                                 (Data.ByteString.length
                                                                                                                                                                                                                                                                    bs)))
                                                                                                                                                                                                                                                           (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                                              bs))
                                                                                                                                                                                                                                                   Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                                                   (Lens.Family2.set
                                                                                                                                                                                                                                                      (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                                         @"key")
                                                                                                                                                                                                                                                      (Prelude.fst
                                                                                                                                                                                                                                                         _v)
                                                                                                                                                                                                                                                      (Lens.Family2.set
                                                                                                                                                                                                                                                         (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                                            @"value")
                                                                                                                                                                                                                                                         (Prelude.snd
                                                                                                                                                                                                                                                            _v)
                                                                                                                                                                                                                                                         (Data.ProtoLens.defMessage ::
                                                                                                                                                                                                                                                            ExampleMessage'MapEnumEntry)))))
                                                                                                                                                                                                                                        (Data.Map.toList
                                                                                                                                                                                                                                           (Lens.Family2.view
                                                                                                                                                                                                                                              (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                                 @"mapEnum")
                                                                                                                                                                                                                                              _x))))
                                                                                                                                                                                                                                  ((Data.Monoid.<>)
                                                                                                                                                                                                                                     (case
                                                                                                                                                                                                                                          Lens.Family2.view
                                                                                                                                                                                                                                            (Data.ProtoLens.Field.field
                                                                                                                                                                                                                                               @"maybe'exampleOneOf")
                                                                                                                                                                                                                                            _x
                                                                                                                                                                                                                                      of
                                                                                                                                                                                                                                        Prelude.Nothing
                                                                                                                                                                                                                                          -> Data.Monoid.mempty
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar01 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4009)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.putFixed64
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.doubleToWord
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar02 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4021)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.putFixed32
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.floatToWord
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar03 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4024)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  Prelude.fromIntegral
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar04 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4032)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  Prelude.fromIntegral
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar05 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4040)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  Prelude.fromIntegral
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar06 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4048)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar07 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4056)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  ((Prelude..)
                                                                                                                                                                                                                                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                     Prelude.fromIntegral)
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.signedInt32ToWord
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar08 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4064)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  ((Prelude..)
                                                                                                                                                                                                                                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                     Prelude.fromIntegral)
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.signedInt64ToWord
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar09 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4077)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putFixed32
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar10 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4081)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putFixed64
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar11 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4093)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.putFixed32
                                                                                                                                                                                                                                                  Prelude.fromIntegral
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar12 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4097)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.putFixed64
                                                                                                                                                                                                                                                  Prelude.fromIntegral
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar13 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4104)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  (\ b
                                                                                                                                                                                                                                                     -> if b then
                                                                                                                                                                                                                                                            1
                                                                                                                                                                                                                                                        else
                                                                                                                                                                                                                                                            0)
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar14 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4114)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  (\ bs
                                                                                                                                                                                                                                                     -> (Data.Monoid.<>)
                                                                                                                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                             (Prelude.fromIntegral
                                                                                                                                                                                                                                                                (Data.ByteString.length
                                                                                                                                                                                                                                                                   bs)))
                                                                                                                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                                             bs))
                                                                                                                                                                                                                                                  Data.Text.Encoding.encodeUtf8
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofScalar15 v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4122)
                                                                                                                                                                                                                                               ((\ bs
                                                                                                                                                                                                                                                   -> (Data.Monoid.<>)
                                                                                                                                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                           (Prelude.fromIntegral
                                                                                                                                                                                                                                                              (Data.ByteString.length
                                                                                                                                                                                                                                                                 bs)))
                                                                                                                                                                                                                                                        (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                                           bs))
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofAnother v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4130)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  (\ bs
                                                                                                                                                                                                                                                     -> (Data.Monoid.<>)
                                                                                                                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                             (Prelude.fromIntegral
                                                                                                                                                                                                                                                                (Data.ByteString.length
                                                                                                                                                                                                                                                                   bs)))
                                                                                                                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                                             bs))
                                                                                                                                                                                                                                                  Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofNested v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4138)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  (\ bs
                                                                                                                                                                                                                                                     -> (Data.Monoid.<>)
                                                                                                                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                             (Prelude.fromIntegral
                                                                                                                                                                                                                                                                (Data.ByteString.length
                                                                                                                                                                                                                                                                   bs)))
                                                                                                                                                                                                                                                          (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                                                                                                                                                                                             bs))
                                                                                                                                                                                                                                                  Data.ProtoLens.encodeMessage
                                                                                                                                                                                                                                                  v)
                                                                                                                                                                                                                                        (Prelude.Just (ExampleMessage'OneofEnum v))
                                                                                                                                                                                                                                          -> (Data.Monoid.<>)
                                                                                                                                                                                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                  4144)
                                                                                                                                                                                                                                               ((Prelude..)
                                                                                                                                                                                                                                                  ((Prelude..)
                                                                                                                                                                                                                                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                                                                                                                                                                                     Prelude.fromIntegral)
                                                                                                                                                                                                                                                  Prelude.fromEnum
                                                                                                                                                                                                                                                  v))
                                                                                                                                                                                                                                     (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                                                                                                                                                                                                                        (Lens.Family2.view
                                                                                                                                                                                                                                           Data.ProtoLens.unknownFields
                                                                                                                                                                                                                                           _x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
instance Control.DeepSeq.NFData ExampleMessage where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'defaultScalar01 x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'defaultScalar02 x__)
                   (Control.DeepSeq.deepseq
                      (_ExampleMessage'defaultScalar03 x__)
                      (Control.DeepSeq.deepseq
                         (_ExampleMessage'defaultScalar04 x__)
                         (Control.DeepSeq.deepseq
                            (_ExampleMessage'defaultScalar05 x__)
                            (Control.DeepSeq.deepseq
                               (_ExampleMessage'defaultScalar06 x__)
                               (Control.DeepSeq.deepseq
                                  (_ExampleMessage'defaultScalar07 x__)
                                  (Control.DeepSeq.deepseq
                                     (_ExampleMessage'defaultScalar08 x__)
                                     (Control.DeepSeq.deepseq
                                        (_ExampleMessage'defaultScalar09 x__)
                                        (Control.DeepSeq.deepseq
                                           (_ExampleMessage'defaultScalar10 x__)
                                           (Control.DeepSeq.deepseq
                                              (_ExampleMessage'defaultScalar11 x__)
                                              (Control.DeepSeq.deepseq
                                                 (_ExampleMessage'defaultScalar12 x__)
                                                 (Control.DeepSeq.deepseq
                                                    (_ExampleMessage'defaultScalar13 x__)
                                                    (Control.DeepSeq.deepseq
                                                       (_ExampleMessage'defaultScalar14 x__)
                                                       (Control.DeepSeq.deepseq
                                                          (_ExampleMessage'defaultScalar15 x__)
                                                          (Control.DeepSeq.deepseq
                                                             (_ExampleMessage'defaultAnother x__)
                                                             (Control.DeepSeq.deepseq
                                                                (_ExampleMessage'defaultNested x__)
                                                                (Control.DeepSeq.deepseq
                                                                   (_ExampleMessage'defaultEnum x__)
                                                                   (Control.DeepSeq.deepseq
                                                                      (_ExampleMessage'optionalScalar01
                                                                         x__)
                                                                      (Control.DeepSeq.deepseq
                                                                         (_ExampleMessage'optionalScalar02
                                                                            x__)
                                                                         (Control.DeepSeq.deepseq
                                                                            (_ExampleMessage'optionalScalar03
                                                                               x__)
                                                                            (Control.DeepSeq.deepseq
                                                                               (_ExampleMessage'optionalScalar04
                                                                                  x__)
                                                                               (Control.DeepSeq.deepseq
                                                                                  (_ExampleMessage'optionalScalar05
                                                                                     x__)
                                                                                  (Control.DeepSeq.deepseq
                                                                                     (_ExampleMessage'optionalScalar06
                                                                                        x__)
                                                                                     (Control.DeepSeq.deepseq
                                                                                        (_ExampleMessage'optionalScalar07
                                                                                           x__)
                                                                                        (Control.DeepSeq.deepseq
                                                                                           (_ExampleMessage'optionalScalar08
                                                                                              x__)
                                                                                           (Control.DeepSeq.deepseq
                                                                                              (_ExampleMessage'optionalScalar09
                                                                                                 x__)
                                                                                              (Control.DeepSeq.deepseq
                                                                                                 (_ExampleMessage'optionalScalar10
                                                                                                    x__)
                                                                                                 (Control.DeepSeq.deepseq
                                                                                                    (_ExampleMessage'optionalScalar11
                                                                                                       x__)
                                                                                                    (Control.DeepSeq.deepseq
                                                                                                       (_ExampleMessage'optionalScalar12
                                                                                                          x__)
                                                                                                       (Control.DeepSeq.deepseq
                                                                                                          (_ExampleMessage'optionalScalar13
                                                                                                             x__)
                                                                                                          (Control.DeepSeq.deepseq
                                                                                                             (_ExampleMessage'optionalScalar14
                                                                                                                x__)
                                                                                                             (Control.DeepSeq.deepseq
                                                                                                                (_ExampleMessage'optionalScalar15
                                                                                                                   x__)
                                                                                                                (Control.DeepSeq.deepseq
                                                                                                                   (_ExampleMessage'optionalAnother
                                                                                                                      x__)
                                                                                                                   (Control.DeepSeq.deepseq
                                                                                                                      (_ExampleMessage'optionalNested
                                                                                                                         x__)
                                                                                                                      (Control.DeepSeq.deepseq
                                                                                                                         (_ExampleMessage'optionalEnum
                                                                                                                            x__)
                                                                                                                         (Control.DeepSeq.deepseq
                                                                                                                            (_ExampleMessage'repeatedScalar01
                                                                                                                               x__)
                                                                                                                            (Control.DeepSeq.deepseq
                                                                                                                               (_ExampleMessage'repeatedScalar02
                                                                                                                                  x__)
                                                                                                                               (Control.DeepSeq.deepseq
                                                                                                                                  (_ExampleMessage'repeatedScalar03
                                                                                                                                     x__)
                                                                                                                                  (Control.DeepSeq.deepseq
                                                                                                                                     (_ExampleMessage'repeatedScalar04
                                                                                                                                        x__)
                                                                                                                                     (Control.DeepSeq.deepseq
                                                                                                                                        (_ExampleMessage'repeatedScalar05
                                                                                                                                           x__)
                                                                                                                                        (Control.DeepSeq.deepseq
                                                                                                                                           (_ExampleMessage'repeatedScalar06
                                                                                                                                              x__)
                                                                                                                                           (Control.DeepSeq.deepseq
                                                                                                                                              (_ExampleMessage'repeatedScalar07
                                                                                                                                                 x__)
                                                                                                                                              (Control.DeepSeq.deepseq
                                                                                                                                                 (_ExampleMessage'repeatedScalar08
                                                                                                                                                    x__)
                                                                                                                                                 (Control.DeepSeq.deepseq
                                                                                                                                                    (_ExampleMessage'repeatedScalar09
                                                                                                                                                       x__)
                                                                                                                                                    (Control.DeepSeq.deepseq
                                                                                                                                                       (_ExampleMessage'repeatedScalar10
                                                                                                                                                          x__)
                                                                                                                                                       (Control.DeepSeq.deepseq
                                                                                                                                                          (_ExampleMessage'repeatedScalar11
                                                                                                                                                             x__)
                                                                                                                                                          (Control.DeepSeq.deepseq
                                                                                                                                                             (_ExampleMessage'repeatedScalar12
                                                                                                                                                                x__)
                                                                                                                                                             (Control.DeepSeq.deepseq
                                                                                                                                                                (_ExampleMessage'repeatedScalar13
                                                                                                                                                                   x__)
                                                                                                                                                                (Control.DeepSeq.deepseq
                                                                                                                                                                   (_ExampleMessage'repeatedScalar14
                                                                                                                                                                      x__)
                                                                                                                                                                   (Control.DeepSeq.deepseq
                                                                                                                                                                      (_ExampleMessage'repeatedScalar15
                                                                                                                                                                         x__)
                                                                                                                                                                      (Control.DeepSeq.deepseq
                                                                                                                                                                         (_ExampleMessage'repeatedAnother
                                                                                                                                                                            x__)
                                                                                                                                                                         (Control.DeepSeq.deepseq
                                                                                                                                                                            (_ExampleMessage'repeatedNested
                                                                                                                                                                               x__)
                                                                                                                                                                            (Control.DeepSeq.deepseq
                                                                                                                                                                               (_ExampleMessage'repeatedEnum
                                                                                                                                                                                  x__)
                                                                                                                                                                               (Control.DeepSeq.deepseq
                                                                                                                                                                                  (_ExampleMessage'mapScalar01
                                                                                                                                                                                     x__)
                                                                                                                                                                                  (Control.DeepSeq.deepseq
                                                                                                                                                                                     (_ExampleMessage'mapScalar02
                                                                                                                                                                                        x__)
                                                                                                                                                                                     (Control.DeepSeq.deepseq
                                                                                                                                                                                        (_ExampleMessage'mapScalar03
                                                                                                                                                                                           x__)
                                                                                                                                                                                        (Control.DeepSeq.deepseq
                                                                                                                                                                                           (_ExampleMessage'mapScalar04
                                                                                                                                                                                              x__)
                                                                                                                                                                                           (Control.DeepSeq.deepseq
                                                                                                                                                                                              (_ExampleMessage'mapScalar05
                                                                                                                                                                                                 x__)
                                                                                                                                                                                              (Control.DeepSeq.deepseq
                                                                                                                                                                                                 (_ExampleMessage'mapScalar06
                                                                                                                                                                                                    x__)
                                                                                                                                                                                                 (Control.DeepSeq.deepseq
                                                                                                                                                                                                    (_ExampleMessage'mapScalar07
                                                                                                                                                                                                       x__)
                                                                                                                                                                                                    (Control.DeepSeq.deepseq
                                                                                                                                                                                                       (_ExampleMessage'mapScalar08
                                                                                                                                                                                                          x__)
                                                                                                                                                                                                       (Control.DeepSeq.deepseq
                                                                                                                                                                                                          (_ExampleMessage'mapScalar09
                                                                                                                                                                                                             x__)
                                                                                                                                                                                                          (Control.DeepSeq.deepseq
                                                                                                                                                                                                             (_ExampleMessage'mapScalar10
                                                                                                                                                                                                                x__)
                                                                                                                                                                                                             (Control.DeepSeq.deepseq
                                                                                                                                                                                                                (_ExampleMessage'mapScalar11
                                                                                                                                                                                                                   x__)
                                                                                                                                                                                                                (Control.DeepSeq.deepseq
                                                                                                                                                                                                                   (_ExampleMessage'mapScalar12
                                                                                                                                                                                                                      x__)
                                                                                                                                                                                                                   (Control.DeepSeq.deepseq
                                                                                                                                                                                                                      (_ExampleMessage'mapScalar13
                                                                                                                                                                                                                         x__)
                                                                                                                                                                                                                      (Control.DeepSeq.deepseq
                                                                                                                                                                                                                         (_ExampleMessage'mapScalar14
                                                                                                                                                                                                                            x__)
                                                                                                                                                                                                                         (Control.DeepSeq.deepseq
                                                                                                                                                                                                                            (_ExampleMessage'mapScalar15
                                                                                                                                                                                                                               x__)
                                                                                                                                                                                                                            (Control.DeepSeq.deepseq
                                                                                                                                                                                                                               (_ExampleMessage'mapAnother
                                                                                                                                                                                                                                  x__)
                                                                                                                                                                                                                               (Control.DeepSeq.deepseq
                                                                                                                                                                                                                                  (_ExampleMessage'mapNested
                                                                                                                                                                                                                                     x__)
                                                                                                                                                                                                                                  (Control.DeepSeq.deepseq
                                                                                                                                                                                                                                     (_ExampleMessage'mapEnum
                                                                                                                                                                                                                                        x__)
                                                                                                                                                                                                                                     (Control.DeepSeq.deepseq
                                                                                                                                                                                                                                        (_ExampleMessage'exampleOneOf
                                                                                                                                                                                                                                           x__)
                                                                                                                                                                                                                                        ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
instance Control.DeepSeq.NFData ExampleMessage'ExampleOneOf where
  rnf (ExampleMessage'OneofScalar01 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar02 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar03 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar04 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar05 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar06 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar07 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar08 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar09 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar10 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar11 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar12 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar13 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar14 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofScalar15 x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofAnother x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofNested x__) = Control.DeepSeq.rnf x__
  rnf (ExampleMessage'OneofEnum x__) = Control.DeepSeq.rnf x__
_ExampleMessage'OneofScalar01 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Prelude.Double
_ExampleMessage'OneofScalar01
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar01
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar01 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar02 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Prelude.Float
_ExampleMessage'OneofScalar02
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar02
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar02 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar03 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.Int.Int32
_ExampleMessage'OneofScalar03
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar03
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar03 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar04 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.Int.Int64
_ExampleMessage'OneofScalar04
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar04
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar04 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar05 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.Word.Word32
_ExampleMessage'OneofScalar05
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar05
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar05 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar06 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.Word.Word64
_ExampleMessage'OneofScalar06
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar06
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar06 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar07 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.Int.Int32
_ExampleMessage'OneofScalar07
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar07
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar07 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar08 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.Int.Int64
_ExampleMessage'OneofScalar08
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar08
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar08 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar09 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.Word.Word32
_ExampleMessage'OneofScalar09
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar09
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar09 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar10 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.Word.Word64
_ExampleMessage'OneofScalar10
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar10
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar10 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar11 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.Int.Int32
_ExampleMessage'OneofScalar11
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar11
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar11 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar12 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.Int.Int64
_ExampleMessage'OneofScalar12
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar12
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar12 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar13 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Prelude.Bool
_ExampleMessage'OneofScalar13
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar13
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar13 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar14 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.Text.Text
_ExampleMessage'OneofScalar14
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar14
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar14 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofScalar15 ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf Data.ByteString.ByteString
_ExampleMessage'OneofScalar15
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofScalar15
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofScalar15 p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofAnother ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf AnotherMessage
_ExampleMessage'OneofAnother
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofAnother
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofAnother p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofNested ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf ExampleMessage'NestedMessage
_ExampleMessage'OneofNested
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofNested
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofNested p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_ExampleMessage'OneofEnum ::
  Data.ProtoLens.Prism.Prism' ExampleMessage'ExampleOneOf ExampleEnum
_ExampleMessage'OneofEnum
  = Data.ProtoLens.Prism.prism'
      ExampleMessage'OneofEnum
      (\ p__
         -> case p__ of
              (ExampleMessage'OneofEnum p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapAnotherEntry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapAnotherEntry AnotherMessage@
         * 'Proto.Spec_Fields.maybe'value' @:: Lens' ExampleMessage'MapAnotherEntry (Prelude.Maybe AnotherMessage)@ -}
data ExampleMessage'MapAnotherEntry
  = ExampleMessage'MapAnotherEntry'_constructor {_ExampleMessage'MapAnotherEntry'key :: !Data.Text.Text,
                                                 _ExampleMessage'MapAnotherEntry'value :: !(Prelude.Maybe AnotherMessage),
                                                 _ExampleMessage'MapAnotherEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapAnotherEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapAnotherEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapAnotherEntry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapAnotherEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapAnotherEntry "value" AnotherMessage where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapAnotherEntry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapAnotherEntry'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExampleMessage'MapAnotherEntry "maybe'value" (Prelude.Maybe AnotherMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapAnotherEntry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapAnotherEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapAnotherEntry where
  messageName _ = Data.Text.pack "ExampleMessage.MapAnotherEntry"
  packedMessageDescriptor _
    = "\n\
      \\SIMapAnotherEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2%\n\
      \\ENQvalue\CAN\STX \SOH(\v2\SI.AnotherMessageR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapAnotherEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AnotherMessage)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapAnotherEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapAnotherEntry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapAnotherEntry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapAnotherEntry'_constructor
        {_ExampleMessage'MapAnotherEntry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapAnotherEntry'value = Prelude.Nothing,
         _ExampleMessage'MapAnotherEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapAnotherEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapAnotherEntry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapAnotherEntry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapAnotherEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapAnotherEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapAnotherEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapAnotherEntry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapEnumEntry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapEnumEntry ExampleEnum@ -}
data ExampleMessage'MapEnumEntry
  = ExampleMessage'MapEnumEntry'_constructor {_ExampleMessage'MapEnumEntry'key :: !Data.Text.Text,
                                              _ExampleMessage'MapEnumEntry'value :: !ExampleEnum,
                                              _ExampleMessage'MapEnumEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapEnumEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapEnumEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapEnumEntry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapEnumEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapEnumEntry "value" ExampleEnum where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapEnumEntry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapEnumEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapEnumEntry where
  messageName _ = Data.Text.pack "ExampleMessage.MapEnumEntry"
  packedMessageDescriptor _
    = "\n\
      \\fMapEnumEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\"\n\
      \\ENQvalue\CAN\STX \SOH(\SO2\f.ExampleEnumR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapEnumEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleEnum)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapEnumEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapEnumEntry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapEnumEntry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapEnumEntry'_constructor
        {_ExampleMessage'MapEnumEntry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapEnumEntry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapEnumEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapEnumEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapEnumEntry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapEnumEntry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                            Prelude.fromEnum _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapEnumEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapEnumEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapEnumEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapEnumEntry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapNestedEntry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapNestedEntry ExampleMessage'NestedMessage@
         * 'Proto.Spec_Fields.maybe'value' @:: Lens' ExampleMessage'MapNestedEntry (Prelude.Maybe ExampleMessage'NestedMessage)@ -}
data ExampleMessage'MapNestedEntry
  = ExampleMessage'MapNestedEntry'_constructor {_ExampleMessage'MapNestedEntry'key :: !Data.Text.Text,
                                                _ExampleMessage'MapNestedEntry'value :: !(Prelude.Maybe ExampleMessage'NestedMessage),
                                                _ExampleMessage'MapNestedEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapNestedEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapNestedEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapNestedEntry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapNestedEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapNestedEntry "value" ExampleMessage'NestedMessage where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapNestedEntry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapNestedEntry'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExampleMessage'MapNestedEntry "maybe'value" (Prelude.Maybe ExampleMessage'NestedMessage) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapNestedEntry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapNestedEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapNestedEntry where
  messageName _ = Data.Text.pack "ExampleMessage.MapNestedEntry"
  packedMessageDescriptor _
    = "\n\
      \\SOMapNestedEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC23\n\
      \\ENQvalue\CAN\STX \SOH(\v2\GS.ExampleMessage.NestedMessageR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapNestedEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExampleMessage'NestedMessage)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapNestedEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapNestedEntry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapNestedEntry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapNestedEntry'_constructor
        {_ExampleMessage'MapNestedEntry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapNestedEntry'value = Prelude.Nothing,
         _ExampleMessage'MapNestedEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapNestedEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapNestedEntry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapNestedEntry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapNestedEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapNestedEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapNestedEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapNestedEntry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar01Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar01Entry Prelude.Double@ -}
data ExampleMessage'MapScalar01Entry
  = ExampleMessage'MapScalar01Entry'_constructor {_ExampleMessage'MapScalar01Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar01Entry'value :: !Prelude.Double,
                                                  _ExampleMessage'MapScalar01Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar01Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar01Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar01Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar01Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar01Entry "value" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar01Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar01Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar01Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar01Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar01Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar01Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar01Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar01Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar01Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar01Entry'_constructor
        {_ExampleMessage'MapScalar01Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar01Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar01Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar01Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar01Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        17
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToDouble
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar01Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 17)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putFixed64
                            Data.ProtoLens.Encoding.Bytes.doubleToWord _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar01Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar01Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar01Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar01Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar02Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar02Entry Prelude.Float@ -}
data ExampleMessage'MapScalar02Entry
  = ExampleMessage'MapScalar02Entry'_constructor {_ExampleMessage'MapScalar02Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar02Entry'value :: !Prelude.Float,
                                                  _ExampleMessage'MapScalar02Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar02Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar02Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar02Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar02Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar02Entry "value" Prelude.Float where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar02Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar02Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar02Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar02Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar02Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\STXR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar02Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar02Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar02Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar02Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar02Entry'_constructor
        {_ExampleMessage'MapScalar02Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar02Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar02Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar02Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar02Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        21
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToFloat
                                          Data.ProtoLens.Encoding.Bytes.getFixed32)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar02Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 21)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putFixed32
                            Data.ProtoLens.Encoding.Bytes.floatToWord _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar02Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar02Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar02Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar02Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar03Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar03Entry Data.Int.Int32@ -}
data ExampleMessage'MapScalar03Entry
  = ExampleMessage'MapScalar03Entry'_constructor {_ExampleMessage'MapScalar03Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar03Entry'value :: !Data.Int.Int32,
                                                  _ExampleMessage'MapScalar03Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar03Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar03Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar03Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar03Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar03Entry "value" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar03Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar03Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar03Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar03Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar03Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar03Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar03Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar03Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar03Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar03Entry'_constructor
        {_ExampleMessage'MapScalar03Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar03Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar03Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar03Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar03Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar03Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar03Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar03Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar03Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar03Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar04Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar04Entry Data.Int.Int64@ -}
data ExampleMessage'MapScalar04Entry
  = ExampleMessage'MapScalar04Entry'_constructor {_ExampleMessage'MapScalar04Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar04Entry'value :: !Data.Int.Int64,
                                                  _ExampleMessage'MapScalar04Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar04Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar04Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar04Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar04Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar04Entry "value" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar04Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar04Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar04Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar04Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar04Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ETXR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar04Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar04Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar04Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar04Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar04Entry'_constructor
        {_ExampleMessage'MapScalar04Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar04Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar04Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar04Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar04Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar04Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar04Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar04Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar04Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar04Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar05Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar05Entry Data.Word.Word32@ -}
data ExampleMessage'MapScalar05Entry
  = ExampleMessage'MapScalar05Entry'_constructor {_ExampleMessage'MapScalar05Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar05Entry'value :: !Data.Word.Word32,
                                                  _ExampleMessage'MapScalar05Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar05Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar05Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar05Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar05Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar05Entry "value" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar05Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar05Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar05Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar05Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar05Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\rR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar05Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar05Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar05Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar05Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar05Entry'_constructor
        {_ExampleMessage'MapScalar05Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar05Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar05Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar05Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar05Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar05Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar05Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar05Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar05Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar05Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar06Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar06Entry Data.Word.Word64@ -}
data ExampleMessage'MapScalar06Entry
  = ExampleMessage'MapScalar06Entry'_constructor {_ExampleMessage'MapScalar06Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar06Entry'value :: !Data.Word.Word64,
                                                  _ExampleMessage'MapScalar06Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar06Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar06Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar06Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar06Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar06Entry "value" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar06Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar06Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar06Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar06Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar06Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\EOTR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar06Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar06Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar06Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar06Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar06Entry'_constructor
        {_ExampleMessage'MapScalar06Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar06Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar06Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar06Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar06Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar06Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar06Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar06Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar06Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar06Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar07Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar07Entry Data.Int.Int32@ -}
data ExampleMessage'MapScalar07Entry
  = ExampleMessage'MapScalar07Entry'_constructor {_ExampleMessage'MapScalar07Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar07Entry'value :: !Data.Int.Int32,
                                                  _ExampleMessage'MapScalar07Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar07Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar07Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar07Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar07Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar07Entry "value" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar07Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar07Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar07Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar07Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar07Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\DC1R\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar07Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar07Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar07Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar07Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar07Entry'_constructor
        {_ExampleMessage'MapScalar07Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar07Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar07Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar07Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar07Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToSignedInt32
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar07Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                            Data.ProtoLens.Encoding.Bytes.signedInt32ToWord _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar07Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar07Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar07Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar07Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar08Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar08Entry Data.Int.Int64@ -}
data ExampleMessage'MapScalar08Entry
  = ExampleMessage'MapScalar08Entry'_constructor {_ExampleMessage'MapScalar08Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar08Entry'value :: !Data.Int.Int64,
                                                  _ExampleMessage'MapScalar08Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar08Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar08Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar08Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar08Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar08Entry "value" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar08Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar08Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar08Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar08Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar08Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\DC2R\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar08Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar08Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar08Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar08Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar08Entry'_constructor
        {_ExampleMessage'MapScalar08Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar08Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar08Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar08Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar08Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToSignedInt64
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar08Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                            Data.ProtoLens.Encoding.Bytes.signedInt64ToWord _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar08Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar08Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar08Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar08Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar09Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar09Entry Data.Word.Word32@ -}
data ExampleMessage'MapScalar09Entry
  = ExampleMessage'MapScalar09Entry'_constructor {_ExampleMessage'MapScalar09Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar09Entry'value :: !Data.Word.Word32,
                                                  _ExampleMessage'MapScalar09Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar09Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar09Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar09Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar09Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar09Entry "value" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar09Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar09Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar09Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar09Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar09Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\aR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar09Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar09Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar09Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar09Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar09Entry'_constructor
        {_ExampleMessage'MapScalar09Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar09Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar09Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar09Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar09Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        21
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed32 "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar09Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 21)
                         (Data.ProtoLens.Encoding.Bytes.putFixed32 _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar09Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar09Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar09Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar09Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar10Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar10Entry Data.Word.Word64@ -}
data ExampleMessage'MapScalar10Entry
  = ExampleMessage'MapScalar10Entry'_constructor {_ExampleMessage'MapScalar10Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar10Entry'value :: !Data.Word.Word64,
                                                  _ExampleMessage'MapScalar10Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar10Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar10Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar10Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar10Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar10Entry "value" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar10Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar10Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar10Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar10Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar10Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ACKR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar10Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar10Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar10Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar10Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar10Entry'_constructor
        {_ExampleMessage'MapScalar10Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar10Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar10Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar10Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar10Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        17
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed64 "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar10Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 17)
                         (Data.ProtoLens.Encoding.Bytes.putFixed64 _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar10Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar10Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar10Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar10Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar11Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar11Entry Data.Int.Int32@ -}
data ExampleMessage'MapScalar11Entry
  = ExampleMessage'MapScalar11Entry'_constructor {_ExampleMessage'MapScalar11Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar11Entry'value :: !Data.Int.Int32,
                                                  _ExampleMessage'MapScalar11Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar11Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar11Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar11Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar11Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar11Entry "value" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar11Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar11Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar11Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar11Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar11Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\SIR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar11Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SFixed32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar11Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar11Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar11Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar11Entry'_constructor
        {_ExampleMessage'MapScalar11Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar11Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar11Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar11Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar11Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        21
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getFixed32)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar11Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 21)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putFixed32 Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar11Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar11Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar11Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar11Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar12Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar12Entry Data.Int.Int64@ -}
data ExampleMessage'MapScalar12Entry
  = ExampleMessage'MapScalar12Entry'_constructor {_ExampleMessage'MapScalar12Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar12Entry'value :: !Data.Int.Int64,
                                                  _ExampleMessage'MapScalar12Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar12Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar12Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar12Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar12Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar12Entry "value" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar12Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar12Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar12Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar12Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar12Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\DLER\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar12Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.SFixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar12Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar12Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar12Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar12Entry'_constructor
        {_ExampleMessage'MapScalar12Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar12Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar12Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar12Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar12Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        17
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar12Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 17)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putFixed64 Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar12Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar12Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar12Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar12Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar13Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar13Entry Prelude.Bool@ -}
data ExampleMessage'MapScalar13Entry
  = ExampleMessage'MapScalar13Entry'_constructor {_ExampleMessage'MapScalar13Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar13Entry'value :: !Prelude.Bool,
                                                  _ExampleMessage'MapScalar13Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar13Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar13Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar13Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar13Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar13Entry "value" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar13Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar13Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar13Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar13Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar13Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\bR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar13Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar13Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar13Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar13Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar13Entry'_constructor
        {_ExampleMessage'MapScalar13Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar13Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar13Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar13Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar13Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar13Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt (\ b -> if b then 1 else 0)
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar13Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar13Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar13Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar13Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar14Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar14Entry Data.Text.Text@ -}
data ExampleMessage'MapScalar14Entry
  = ExampleMessage'MapScalar14Entry'_constructor {_ExampleMessage'MapScalar14Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar14Entry'value :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar14Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar14Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar14Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar14Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar14Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar14Entry "value" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar14Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar14Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar14Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar14Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar14Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\tR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar14Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar14Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar14Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar14Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar14Entry'_constructor
        {_ExampleMessage'MapScalar14Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar14Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar14Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar14Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar14Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar14Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar14Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar14Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar14Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar14Entry'value x__) ()))
{- | Fields :
     
         * 'Proto.Spec_Fields.key' @:: Lens' ExampleMessage'MapScalar15Entry Data.Text.Text@
         * 'Proto.Spec_Fields.value' @:: Lens' ExampleMessage'MapScalar15Entry Data.ByteString.ByteString@ -}
data ExampleMessage'MapScalar15Entry
  = ExampleMessage'MapScalar15Entry'_constructor {_ExampleMessage'MapScalar15Entry'key :: !Data.Text.Text,
                                                  _ExampleMessage'MapScalar15Entry'value :: !Data.ByteString.ByteString,
                                                  _ExampleMessage'MapScalar15Entry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'MapScalar15Entry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar15Entry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar15Entry'key
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar15Entry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExampleMessage'MapScalar15Entry "value" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExampleMessage'MapScalar15Entry'value
           (\ x__ y__ -> x__ {_ExampleMessage'MapScalar15Entry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExampleMessage'MapScalar15Entry where
  messageName _ = Data.Text.pack "ExampleMessage.MapScalar15Entry"
  packedMessageDescriptor _
    = "\n\
      \\DLEMapScalar15Entry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar15Entry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExampleMessage'MapScalar15Entry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'MapScalar15Entry'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'MapScalar15Entry'_unknownFields = y__})
  defMessage
    = ExampleMessage'MapScalar15Entry'_constructor
        {_ExampleMessage'MapScalar15Entry'key = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar15Entry'value = Data.ProtoLens.fieldDefault,
         _ExampleMessage'MapScalar15Entry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'MapScalar15Entry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'MapScalar15Entry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MapScalar15Entry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExampleMessage'MapScalar15Entry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'MapScalar15Entry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExampleMessage'MapScalar15Entry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExampleMessage'MapScalar15Entry'value x__) ()))
{- | Fields :
      -}
data ExampleMessage'NestedMessage
  = ExampleMessage'NestedMessage'_constructor {_ExampleMessage'NestedMessage'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExampleMessage'NestedMessage where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message ExampleMessage'NestedMessage where
  messageName _ = Data.Text.pack "ExampleMessage.NestedMessage"
  packedMessageDescriptor _
    = "\n\
      \\rNestedMessage"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExampleMessage'NestedMessage'_unknownFields
        (\ x__ y__
           -> x__ {_ExampleMessage'NestedMessage'_unknownFields = y__})
  defMessage
    = ExampleMessage'NestedMessage'_constructor
        {_ExampleMessage'NestedMessage'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExampleMessage'NestedMessage
          -> Data.ProtoLens.Encoding.Bytes.Parser ExampleMessage'NestedMessage
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "NestedMessage"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData ExampleMessage'NestedMessage where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExampleMessage'NestedMessage'_unknownFields x__) ()
data ExampleService = ExampleService {}
instance Data.ProtoLens.Service.Types.Service ExampleService where
  type ServiceName ExampleService = "ExampleService"
  type ServicePackage ExampleService = ""
  type ServiceMethods ExampleService = '["acceptAnother",
                                         "acceptNested",
                                         "returnAnother",
                                         "returnNested"]
  packedServiceDescriptor _
    = "\n\
      \\SOExampleService\DC2(\n\
      \\rAcceptAnother\DC2\SI.AnotherMessage\SUB\ACK.Empty\DC25\n\
      \\fAcceptNested\DC2\GS.ExampleMessage.NestedMessage\SUB\ACK.Empty\DC2(\n\
      \\rReturnAnother\DC2\ACK.Empty\SUB\SI.AnotherMessage\DC25\n\
      \\fReturnNested\DC2\ACK.Empty\SUB\GS.ExampleMessage.NestedMessage"
instance Data.ProtoLens.Service.Types.HasMethodImpl ExampleService "acceptAnother" where
  type MethodName ExampleService "acceptAnother" = "AcceptAnother"
  type MethodInput ExampleService "acceptAnother" = AnotherMessage
  type MethodOutput ExampleService "acceptAnother" = Empty
  type MethodStreamingType ExampleService "acceptAnother" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl ExampleService "acceptNested" where
  type MethodName ExampleService "acceptNested" = "AcceptNested"
  type MethodInput ExampleService "acceptNested" = ExampleMessage'NestedMessage
  type MethodOutput ExampleService "acceptNested" = Empty
  type MethodStreamingType ExampleService "acceptNested" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl ExampleService "returnAnother" where
  type MethodName ExampleService "returnAnother" = "ReturnAnother"
  type MethodInput ExampleService "returnAnother" = Empty
  type MethodOutput ExampleService "returnAnother" = AnotherMessage
  type MethodStreamingType ExampleService "returnAnother" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl ExampleService "returnNested" where
  type MethodName ExampleService "returnNested" = "ReturnNested"
  type MethodInput ExampleService "returnNested" = Empty
  type MethodOutput ExampleService "returnNested" = ExampleMessage'NestedMessage
  type MethodStreamingType ExampleService "returnNested" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\n\
    \spec.proto\"\228\&0\n\
    \\SOExampleMessage\DC2(\n\
    \\SIdefaultScalar01\CANe \SOH(\SOHR\SIdefaultScalar01\DC2(\n\
    \\SIdefaultScalar02\CANf \SOH(\STXR\SIdefaultScalar02\DC2(\n\
    \\SIdefaultScalar03\CANg \SOH(\ENQR\SIdefaultScalar03\DC2(\n\
    \\SIdefaultScalar04\CANh \SOH(\ETXR\SIdefaultScalar04\DC2(\n\
    \\SIdefaultScalar05\CANi \SOH(\rR\SIdefaultScalar05\DC2(\n\
    \\SIdefaultScalar06\CANj \SOH(\EOTR\SIdefaultScalar06\DC2(\n\
    \\SIdefaultScalar07\CANk \SOH(\DC1R\SIdefaultScalar07\DC2(\n\
    \\SIdefaultScalar08\CANl \SOH(\DC2R\SIdefaultScalar08\DC2(\n\
    \\SIdefaultScalar09\CANm \SOH(\aR\SIdefaultScalar09\DC2(\n\
    \\SIdefaultScalar10\CANn \SOH(\ACKR\SIdefaultScalar10\DC2(\n\
    \\SIdefaultScalar11\CANo \SOH(\SIR\SIdefaultScalar11\DC2(\n\
    \\SIdefaultScalar12\CANp \SOH(\DLER\SIdefaultScalar12\DC2(\n\
    \\SIdefaultScalar13\CANq \SOH(\bR\SIdefaultScalar13\DC2(\n\
    \\SIdefaultScalar14\CANr \SOH(\tR\SIdefaultScalar14\DC2(\n\
    \\SIdefaultScalar15\CANs \SOH(\fR\SIdefaultScalar15\DC27\n\
    \\SOdefaultAnother\CANt \SOH(\v2\SI.AnotherMessageR\SOdefaultAnother\DC2C\n\
    \\rdefaultNested\CANu \SOH(\v2\GS.ExampleMessage.NestedMessageR\rdefaultNested\DC2.\n\
    \\vdefaultEnum\CANv \SOH(\SO2\f.ExampleEnumR\vdefaultEnum\DC20\n\
    \\DLEoptionalScalar01\CAN\201\SOH \SOH(\SOHH\SOHR\DLEoptionalScalar01\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar02\CAN\202\SOH \SOH(\STXH\STXR\DLEoptionalScalar02\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar03\CAN\203\SOH \SOH(\ENQH\ETXR\DLEoptionalScalar03\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar04\CAN\204\SOH \SOH(\ETXH\EOTR\DLEoptionalScalar04\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar05\CAN\205\SOH \SOH(\rH\ENQR\DLEoptionalScalar05\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar06\CAN\206\SOH \SOH(\EOTH\ACKR\DLEoptionalScalar06\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar07\CAN\207\SOH \SOH(\DC1H\aR\DLEoptionalScalar07\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar08\CAN\208\SOH \SOH(\DC2H\bR\DLEoptionalScalar08\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar09\CAN\209\SOH \SOH(\aH\tR\DLEoptionalScalar09\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar10\CAN\210\SOH \SOH(\ACKH\n\
    \R\DLEoptionalScalar10\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar11\CAN\211\SOH \SOH(\SIH\vR\DLEoptionalScalar11\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar12\CAN\212\SOH \SOH(\DLEH\fR\DLEoptionalScalar12\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar13\CAN\213\SOH \SOH(\bH\rR\DLEoptionalScalar13\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar14\CAN\214\SOH \SOH(\tH\SOR\DLEoptionalScalar14\136\SOH\SOH\DC20\n\
    \\DLEoptionalScalar15\CAN\215\SOH \SOH(\fH\SIR\DLEoptionalScalar15\136\SOH\SOH\DC2?\n\
    \\SIoptionalAnother\CAN\216\SOH \SOH(\v2\SI.AnotherMessageH\DLER\SIoptionalAnother\136\SOH\SOH\DC2K\n\
    \\SOoptionalNested\CAN\217\SOH \SOH(\v2\GS.ExampleMessage.NestedMessageH\DC1R\SOoptionalNested\136\SOH\SOH\DC26\n\
    \\foptionalEnum\CAN\218\SOH \SOH(\SO2\f.ExampleEnumH\DC2R\foptionalEnum\136\SOH\SOH\DC2+\n\
    \\DLErepeatedScalar01\CAN\173\STX \ETX(\SOHR\DLErepeatedScalar01\DC2+\n\
    \\DLErepeatedScalar02\CAN\174\STX \ETX(\STXR\DLErepeatedScalar02\DC2+\n\
    \\DLErepeatedScalar03\CAN\175\STX \ETX(\ENQR\DLErepeatedScalar03\DC2+\n\
    \\DLErepeatedScalar04\CAN\176\STX \ETX(\ETXR\DLErepeatedScalar04\DC2+\n\
    \\DLErepeatedScalar05\CAN\177\STX \ETX(\rR\DLErepeatedScalar05\DC2+\n\
    \\DLErepeatedScalar06\CAN\178\STX \ETX(\EOTR\DLErepeatedScalar06\DC2+\n\
    \\DLErepeatedScalar07\CAN\179\STX \ETX(\DC1R\DLErepeatedScalar07\DC2+\n\
    \\DLErepeatedScalar08\CAN\180\STX \ETX(\DC2R\DLErepeatedScalar08\DC2+\n\
    \\DLErepeatedScalar09\CAN\181\STX \ETX(\aR\DLErepeatedScalar09\DC2+\n\
    \\DLErepeatedScalar10\CAN\182\STX \ETX(\ACKR\DLErepeatedScalar10\DC2+\n\
    \\DLErepeatedScalar11\CAN\183\STX \ETX(\SIR\DLErepeatedScalar11\DC2+\n\
    \\DLErepeatedScalar12\CAN\184\STX \ETX(\DLER\DLErepeatedScalar12\DC2+\n\
    \\DLErepeatedScalar13\CAN\185\STX \ETX(\bR\DLErepeatedScalar13\DC2+\n\
    \\DLErepeatedScalar14\CAN\186\STX \ETX(\tR\DLErepeatedScalar14\DC2+\n\
    \\DLErepeatedScalar15\CAN\187\STX \ETX(\fR\DLErepeatedScalar15\DC2:\n\
    \\SIrepeatedAnother\CAN\188\STX \ETX(\v2\SI.AnotherMessageR\SIrepeatedAnother\DC2F\n\
    \\SOrepeatedNested\CAN\189\STX \ETX(\v2\GS.ExampleMessage.NestedMessageR\SOrepeatedNested\DC21\n\
    \\frepeatedEnum\CAN\190\STX \ETX(\SO2\f.ExampleEnumR\frepeatedEnum\DC2C\n\
    \\vmapScalar01\CAN\145\ETX \ETX(\v2 .ExampleMessage.MapScalar01EntryR\vmapScalar01\DC2C\n\
    \\vmapScalar02\CAN\146\ETX \ETX(\v2 .ExampleMessage.MapScalar02EntryR\vmapScalar02\DC2C\n\
    \\vmapScalar03\CAN\147\ETX \ETX(\v2 .ExampleMessage.MapScalar03EntryR\vmapScalar03\DC2C\n\
    \\vmapScalar04\CAN\148\ETX \ETX(\v2 .ExampleMessage.MapScalar04EntryR\vmapScalar04\DC2C\n\
    \\vmapScalar05\CAN\149\ETX \ETX(\v2 .ExampleMessage.MapScalar05EntryR\vmapScalar05\DC2C\n\
    \\vmapScalar06\CAN\150\ETX \ETX(\v2 .ExampleMessage.MapScalar06EntryR\vmapScalar06\DC2C\n\
    \\vmapScalar07\CAN\151\ETX \ETX(\v2 .ExampleMessage.MapScalar07EntryR\vmapScalar07\DC2C\n\
    \\vmapScalar08\CAN\152\ETX \ETX(\v2 .ExampleMessage.MapScalar08EntryR\vmapScalar08\DC2C\n\
    \\vmapScalar09\CAN\153\ETX \ETX(\v2 .ExampleMessage.MapScalar09EntryR\vmapScalar09\DC2C\n\
    \\vmapScalar10\CAN\154\ETX \ETX(\v2 .ExampleMessage.MapScalar10EntryR\vmapScalar10\DC2C\n\
    \\vmapScalar11\CAN\155\ETX \ETX(\v2 .ExampleMessage.MapScalar11EntryR\vmapScalar11\DC2C\n\
    \\vmapScalar12\CAN\156\ETX \ETX(\v2 .ExampleMessage.MapScalar12EntryR\vmapScalar12\DC2C\n\
    \\vmapScalar13\CAN\157\ETX \ETX(\v2 .ExampleMessage.MapScalar13EntryR\vmapScalar13\DC2C\n\
    \\vmapScalar14\CAN\158\ETX \ETX(\v2 .ExampleMessage.MapScalar14EntryR\vmapScalar14\DC2C\n\
    \\vmapScalar15\CAN\159\ETX \ETX(\v2 .ExampleMessage.MapScalar15EntryR\vmapScalar15\DC2@\n\
    \\n\
    \mapAnother\CAN\160\ETX \ETX(\v2\US.ExampleMessage.MapAnotherEntryR\n\
    \mapAnother\DC2=\n\
    \\tmapNested\CAN\161\ETX \ETX(\v2\RS.ExampleMessage.MapNestedEntryR\tmapNested\DC27\n\
    \\amapEnum\CAN\162\ETX \ETX(\v2\FS.ExampleMessage.MapEnumEntryR\amapEnum\DC2'\n\
    \\roneofScalar01\CAN\245\ETX \SOH(\SOHH\NULR\roneofScalar01\DC2'\n\
    \\roneofScalar02\CAN\246\ETX \SOH(\STXH\NULR\roneofScalar02\DC2'\n\
    \\roneofScalar03\CAN\247\ETX \SOH(\ENQH\NULR\roneofScalar03\DC2'\n\
    \\roneofScalar04\CAN\248\ETX \SOH(\ETXH\NULR\roneofScalar04\DC2'\n\
    \\roneofScalar05\CAN\249\ETX \SOH(\rH\NULR\roneofScalar05\DC2'\n\
    \\roneofScalar06\CAN\250\ETX \SOH(\EOTH\NULR\roneofScalar06\DC2'\n\
    \\roneofScalar07\CAN\251\ETX \SOH(\DC1H\NULR\roneofScalar07\DC2'\n\
    \\roneofScalar08\CAN\252\ETX \SOH(\DC2H\NULR\roneofScalar08\DC2'\n\
    \\roneofScalar09\CAN\253\ETX \SOH(\aH\NULR\roneofScalar09\DC2'\n\
    \\roneofScalar10\CAN\254\ETX \SOH(\ACKH\NULR\roneofScalar10\DC2'\n\
    \\roneofScalar11\CAN\255\ETX \SOH(\SIH\NULR\roneofScalar11\DC2'\n\
    \\roneofScalar12\CAN\128\EOT \SOH(\DLEH\NULR\roneofScalar12\DC2'\n\
    \\roneofScalar13\CAN\129\EOT \SOH(\bH\NULR\roneofScalar13\DC2'\n\
    \\roneofScalar14\CAN\130\EOT \SOH(\tH\NULR\roneofScalar14\DC2'\n\
    \\roneofScalar15\CAN\131\EOT \SOH(\fH\NULR\roneofScalar15\DC26\n\
    \\foneofAnother\CAN\132\EOT \SOH(\v2\SI.AnotherMessageH\NULR\foneofAnother\DC2B\n\
    \\voneofNested\CAN\133\EOT \SOH(\v2\GS.ExampleMessage.NestedMessageH\NULR\voneofNested\DC2-\n\
    \\toneofEnum\CAN\134\EOT \SOH(\SO2\f.ExampleEnumH\NULR\toneofEnum\SUB\SI\n\
    \\rNestedMessage\SUB>\n\
    \\DLEMapScalar01Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar02Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\STXR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar03Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar04Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\ETXR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar05Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\rR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar06Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\EOTR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar07Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\DC1R\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar08Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\DC2R\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar09Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\aR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar10Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\ACKR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar11Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\SIR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar12Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\DLER\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar13Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\bR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar14Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\tR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEMapScalar15Entry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue:\STX8\SOH\SUBN\n\
    \\SIMapAnotherEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2%\n\
    \\ENQvalue\CAN\STX \SOH(\v2\SI.AnotherMessageR\ENQvalue:\STX8\SOH\SUB[\n\
    \\SOMapNestedEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC23\n\
    \\ENQvalue\CAN\STX \SOH(\v2\GS.ExampleMessage.NestedMessageR\ENQvalue:\STX8\SOH\SUBH\n\
    \\fMapEnumEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\"\n\
    \\ENQvalue\CAN\STX \SOH(\SO2\f.ExampleEnumR\ENQvalue:\STX8\SOHB\SO\n\
    \\fexampleOneOfB\DC3\n\
    \\DC1_optionalScalar01B\DC3\n\
    \\DC1_optionalScalar02B\DC3\n\
    \\DC1_optionalScalar03B\DC3\n\
    \\DC1_optionalScalar04B\DC3\n\
    \\DC1_optionalScalar05B\DC3\n\
    \\DC1_optionalScalar06B\DC3\n\
    \\DC1_optionalScalar07B\DC3\n\
    \\DC1_optionalScalar08B\DC3\n\
    \\DC1_optionalScalar09B\DC3\n\
    \\DC1_optionalScalar10B\DC3\n\
    \\DC1_optionalScalar11B\DC3\n\
    \\DC1_optionalScalar12B\DC3\n\
    \\DC1_optionalScalar13B\DC3\n\
    \\DC1_optionalScalar14B\DC3\n\
    \\DC1_optionalScalar15B\DC2\n\
    \\DLE_optionalAnotherB\DC1\n\
    \\SI_optionalNestedB\SI\n\
    \\r_optionalEnum\"\DLE\n\
    \\SOAnotherMessage\"\a\n\
    \\ENQEmpty*1\n\
    \\vExampleEnum\DC2\n\
    \\n\
    \\ACKENUM_A\DLE\NUL\DC2\n\
    \\n\
    \\ACKENUM_B\DLE\SOH\DC2\n\
    \\n\
    \\ACKENUM_C\DLE\STX2\210\SOH\n\
    \\SOExampleService\DC2(\n\
    \\rAcceptAnother\DC2\SI.AnotherMessage\SUB\ACK.Empty\DC25\n\
    \\fAcceptNested\DC2\GS.ExampleMessage.NestedMessage\SUB\ACK.Empty\DC2(\n\
    \\rReturnAnother\DC2\ACK.Empty\SUB\SI.AnotherMessage\DC25\n\
    \\fReturnNested\DC2\ACK.Empty\SUB\GS.ExampleMessage.NestedMessageJ\210\&7\n\
    \\a\DC2\ENQ\NUL\NUL\152\SOH\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\193\b\n\
    \\STX\EOT\NUL\DC2\ENQ \NUL\132\SOH\SOH2\179\b\n\
    \ Definition that exercises most of the spec.\n\
    \\n\
    \ Notes:\n\
    \\n\
    \ - We combine all field labels with all field types.\n\
    \   https://protobuf.dev/programming-guides/proto3/#field-labels\n\
    \   https://protobuf.dev/programming-guides/proto3/#scalar\n\
    \\n\
    \ - It is not possible to nest labels; we can't have a list of lists, a map\n\
    \   of optional values, etc.\n\
    \\n\
    \ - Maps can only have (some) scalar types as keys. For our purposes it\n\
    \   suffices to test with _one_ scalar type.\n\
    \   https://protobuf.dev/programming-guides/proto3/#maps\n\
    \\n\
    \ - The spec says that `oneof` is allowed to contain optional fields, but\n\
    \   `protoc` disagrees (\"Fields in oneofs must not have labels (required /\n\
    \   optional / repeated)\"). It also disallows maps (\"Map fields are not allowed\n\
    \   in oneofs\"), but that actually _is_ conform the spec, albeit a bit\n\
    \   implicitly.\n\
    \   https://protobuf.dev/programming-guides/proto3/#using-oneof\n\
    \\n\
    \ - Groups are deprecated; we don't test them.\n\
    \   https://protobuf.dev/programming-guides/encoding/#groups\n\
    \\n\
    \ - Only message types can be used as rpc inputs or outputs; not scalars or\n\
    \   enums.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX \b\SYN\n\
    \\f\n\
    \\EOT\EOT\NUL\ETX\NUL\DC2\EOT!\STX\"\ETX\n\
    \\f\n\
    \\ENQ\EOT\NUL\ETX\NUL\SOH\DC2\ETX!\n\
    \\ETB\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX$\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX$\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX$\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX$#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX%\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ENQ\DC2\ETX%\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX%\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX%#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETX&\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ENQ\DC2\ETX&\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETX&\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETX&#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\ETX\DC2\ETX'\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ENQ\DC2\ETX'\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\SOH\DC2\ETX'\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ETX\DC2\ETX'#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\EOT\DC2\ETX(\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\ENQ\DC2\ETX(\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\SOH\DC2\ETX(\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\ETX\DC2\ETX(#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\ENQ\DC2\ETX)\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\ENQ\DC2\ETX)\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\SOH\DC2\ETX)\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\ETX\DC2\ETX)#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\ACK\DC2\ETX*\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ACK\ENQ\DC2\ETX*\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ACK\SOH\DC2\ETX*\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ACK\ETX\DC2\ETX*#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\a\DC2\ETX+\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\a\ENQ\DC2\ETX+\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\a\SOH\DC2\ETX+\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\a\ETX\DC2\ETX+#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\b\DC2\ETX,\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\b\ENQ\DC2\ETX,\STX\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\b\SOH\DC2\ETX,\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\b\ETX\DC2\ETX,#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\t\DC2\ETX-\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\t\ENQ\DC2\ETX-\STX\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\t\SOH\DC2\ETX-\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\t\ETX\DC2\ETX-#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\n\
    \\DC2\ETX.\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\n\
    \\ENQ\DC2\ETX.\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\n\
    \\SOH\DC2\ETX.\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\n\
    \\ETX\DC2\ETX.#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\v\DC2\ETX/\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\v\ENQ\DC2\ETX/\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\v\SOH\DC2\ETX/\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\v\ETX\DC2\ETX/#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\f\DC2\ETX0\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\f\ENQ\DC2\ETX0\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\f\SOH\DC2\ETX0\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\f\ETX\DC2\ETX0#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\r\DC2\ETX1\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\r\ENQ\DC2\ETX1\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\r\SOH\DC2\ETX1\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\r\ETX\DC2\ETX1#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SO\DC2\ETX2\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SO\ENQ\DC2\ETX2\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SO\SOH\DC2\ETX2\DC1 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SO\ETX\DC2\ETX2#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SI\DC2\ETX3\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SI\ACK\DC2\ETX3\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SI\SOH\DC2\ETX3\DC1\US\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SI\ETX\DC2\ETX3#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\DLE\DC2\ETX4\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DLE\ACK\DC2\ETX4\STX\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DLE\SOH\DC2\ETX4\DC1\RS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DLE\ETX\DC2\ETX4#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\DC1\DC2\ETX5\STX'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC1\ACK\DC2\ETX5\STX\r\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC1\SOH\DC2\ETX5\DC1\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC1\ETX\DC2\ETX5#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\DC2\DC2\ETX7\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC2\EOT\DC2\ETX7\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC2\ENQ\DC2\ETX7\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC2\SOH\DC2\ETX7\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC2\ETX\DC2\ETX7-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\DC3\DC2\ETX8\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC3\EOT\DC2\ETX8\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC3\ENQ\DC2\ETX8\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC3\SOH\DC2\ETX8\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC3\ETX\DC2\ETX8-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\DC4\DC2\ETX9\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC4\EOT\DC2\ETX9\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC4\ENQ\DC2\ETX9\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC4\SOH\DC2\ETX9\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC4\ETX\DC2\ETX9-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NAK\DC2\ETX:\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NAK\EOT\DC2\ETX:\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NAK\ENQ\DC2\ETX:\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NAK\SOH\DC2\ETX:\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NAK\ETX\DC2\ETX:-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SYN\DC2\ETX;\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SYN\EOT\DC2\ETX;\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SYN\ENQ\DC2\ETX;\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SYN\SOH\DC2\ETX;\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SYN\ETX\DC2\ETX;-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\ETB\DC2\ETX<\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETB\EOT\DC2\ETX<\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETB\ENQ\DC2\ETX<\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETB\SOH\DC2\ETX<\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETB\ETX\DC2\ETX<-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\CAN\DC2\ETX=\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\CAN\EOT\DC2\ETX=\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\CAN\ENQ\DC2\ETX=\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\CAN\SOH\DC2\ETX=\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\CAN\ETX\DC2\ETX=-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\EM\DC2\ETX>\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EM\EOT\DC2\ETX>\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EM\ENQ\DC2\ETX>\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EM\SOH\DC2\ETX>\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EM\ETX\DC2\ETX>-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SUB\DC2\ETX?\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SUB\EOT\DC2\ETX?\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SUB\ENQ\DC2\ETX?\v\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SUB\SOH\DC2\ETX?\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SUB\ETX\DC2\ETX?-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\ESC\DC2\ETX@\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ESC\EOT\DC2\ETX@\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ESC\ENQ\DC2\ETX@\v\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ESC\SOH\DC2\ETX@\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ESC\ETX\DC2\ETX@-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\FS\DC2\ETXA\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\FS\EOT\DC2\ETXA\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\FS\ENQ\DC2\ETXA\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\FS\SOH\DC2\ETXA\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\FS\ETX\DC2\ETXA-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\GS\DC2\ETXB\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\GS\EOT\DC2\ETXB\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\GS\ENQ\DC2\ETXB\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\GS\SOH\DC2\ETXB\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\GS\ETX\DC2\ETXB-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\RS\DC2\ETXC\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\RS\EOT\DC2\ETXC\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\RS\ENQ\DC2\ETXC\v\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\RS\SOH\DC2\ETXC\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\RS\ETX\DC2\ETXC-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\US\DC2\ETXD\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\US\EOT\DC2\ETXD\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\US\ENQ\DC2\ETXD\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\US\SOH\DC2\ETXD\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\US\ETX\DC2\ETXD-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX \DC2\ETXE\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX \EOT\DC2\ETXE\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX \ENQ\DC2\ETXE\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX \SOH\DC2\ETXE\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX \ETX\DC2\ETXE-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX!\DC2\ETXF\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX!\EOT\DC2\ETXF\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX!\ACK\DC2\ETXF\v\EM\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX!\SOH\DC2\ETXF\SUB)\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX!\ETX\DC2\ETXF-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\"\DC2\ETXG\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\"\EOT\DC2\ETXG\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\"\ACK\DC2\ETXG\v\CAN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\"\SOH\DC2\ETXG\SUB(\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\"\ETX\DC2\ETXG-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX#\DC2\ETXH\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX#\EOT\DC2\ETXH\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX#\ACK\DC2\ETXH\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX#\SOH\DC2\ETXH\SUB&\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX#\ETX\DC2\ETXH-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX$\DC2\ETXJ\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX$\EOT\DC2\ETXJ\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX$\ENQ\DC2\ETXJ\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX$\SOH\DC2\ETXJ\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX$\ETX\DC2\ETXJ-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX%\DC2\ETXK\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX%\EOT\DC2\ETXK\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX%\ENQ\DC2\ETXK\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX%\SOH\DC2\ETXK\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX%\ETX\DC2\ETXK-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX&\DC2\ETXL\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX&\EOT\DC2\ETXL\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX&\ENQ\DC2\ETXL\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX&\SOH\DC2\ETXL\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX&\ETX\DC2\ETXL-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX'\DC2\ETXM\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX'\EOT\DC2\ETXM\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX'\ENQ\DC2\ETXM\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX'\SOH\DC2\ETXM\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX'\ETX\DC2\ETXM-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX(\DC2\ETXN\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX(\EOT\DC2\ETXN\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX(\ENQ\DC2\ETXN\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX(\SOH\DC2\ETXN\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX(\ETX\DC2\ETXN-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX)\DC2\ETXO\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX)\EOT\DC2\ETXO\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX)\ENQ\DC2\ETXO\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX)\SOH\DC2\ETXO\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX)\ETX\DC2\ETXO-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX*\DC2\ETXP\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX*\EOT\DC2\ETXP\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX*\ENQ\DC2\ETXP\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX*\SOH\DC2\ETXP\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX*\ETX\DC2\ETXP-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX+\DC2\ETXQ\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX+\EOT\DC2\ETXQ\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX+\ENQ\DC2\ETXQ\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX+\SOH\DC2\ETXQ\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX+\ETX\DC2\ETXQ-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX,\DC2\ETXR\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX,\EOT\DC2\ETXR\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX,\ENQ\DC2\ETXR\v\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX,\SOH\DC2\ETXR\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX,\ETX\DC2\ETXR-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX-\DC2\ETXS\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX-\EOT\DC2\ETXS\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX-\ENQ\DC2\ETXS\v\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX-\SOH\DC2\ETXS\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX-\ETX\DC2\ETXS-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX.\DC2\ETXT\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX.\EOT\DC2\ETXT\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX.\ENQ\DC2\ETXT\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX.\SOH\DC2\ETXT\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX.\ETX\DC2\ETXT-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX/\DC2\ETXU\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX/\EOT\DC2\ETXU\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX/\ENQ\DC2\ETXU\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX/\SOH\DC2\ETXU\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX/\ETX\DC2\ETXU-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX0\DC2\ETXV\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX0\EOT\DC2\ETXV\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX0\ENQ\DC2\ETXV\v\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX0\SOH\DC2\ETXV\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX0\ETX\DC2\ETXV-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX1\DC2\ETXW\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX1\EOT\DC2\ETXW\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX1\ENQ\DC2\ETXW\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX1\SOH\DC2\ETXW\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX1\ETX\DC2\ETXW-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX2\DC2\ETXX\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX2\EOT\DC2\ETXX\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX2\ENQ\DC2\ETXX\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX2\SOH\DC2\ETXX\SUB*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX2\ETX\DC2\ETXX-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX3\DC2\ETXY\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX3\EOT\DC2\ETXY\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX3\ACK\DC2\ETXY\v\EM\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX3\SOH\DC2\ETXY\SUB)\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX3\ETX\DC2\ETXY-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX4\DC2\ETXZ\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX4\EOT\DC2\ETXZ\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX4\ACK\DC2\ETXZ\v\CAN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX4\SOH\DC2\ETXZ\SUB(\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX4\ETX\DC2\ETXZ-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX5\DC2\ETX[\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX5\EOT\DC2\ETX[\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX5\ACK\DC2\ETX[\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX5\SOH\DC2\ETX[\SUB&\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX5\ETX\DC2\ETX[-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX6\DC2\ETX]\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX6\ACK\DC2\ETX]\STX\SYN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX6\SOH\DC2\ETX]\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX6\ETX\DC2\ETX]-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX7\DC2\ETX^\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX7\ACK\DC2\ETX^\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX7\SOH\DC2\ETX^\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX7\ETX\DC2\ETX^-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX8\DC2\ETX_\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX8\ACK\DC2\ETX_\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX8\SOH\DC2\ETX_\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX8\ETX\DC2\ETX_-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX9\DC2\ETX`\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX9\ACK\DC2\ETX`\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX9\SOH\DC2\ETX`\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX9\ETX\DC2\ETX`-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX:\DC2\ETXa\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX:\ACK\DC2\ETXa\STX\SYN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX:\SOH\DC2\ETXa\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX:\ETX\DC2\ETXa-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX;\DC2\ETXb\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX;\ACK\DC2\ETXb\STX\SYN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX;\SOH\DC2\ETXb\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX;\ETX\DC2\ETXb-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX<\DC2\ETXc\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX<\ACK\DC2\ETXc\STX\SYN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX<\SOH\DC2\ETXc\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX<\ETX\DC2\ETXc-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX=\DC2\ETXd\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX=\ACK\DC2\ETXd\STX\SYN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX=\SOH\DC2\ETXd\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX=\ETX\DC2\ETXd-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX>\DC2\ETXe\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX>\ACK\DC2\ETXe\STX\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX>\SOH\DC2\ETXe\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX>\ETX\DC2\ETXe-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX?\DC2\ETXf\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX?\ACK\DC2\ETXf\STX\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX?\SOH\DC2\ETXf\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX?\ETX\DC2\ETXf-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STX@\DC2\ETXg\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX@\ACK\DC2\ETXg\STX\CAN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX@\SOH\DC2\ETXg\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX@\ETX\DC2\ETXg-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STXA\DC2\ETXh\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXA\ACK\DC2\ETXh\STX\CAN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXA\SOH\DC2\ETXh\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXA\ETX\DC2\ETXh-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STXB\DC2\ETXi\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXB\ACK\DC2\ETXi\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXB\SOH\DC2\ETXi\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXB\ETX\DC2\ETXi-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STXC\DC2\ETXj\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXC\ACK\DC2\ETXj\STX\SYN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXC\SOH\DC2\ETXj\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXC\ETX\DC2\ETXj-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STXD\DC2\ETXk\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXD\ACK\DC2\ETXk\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXD\SOH\DC2\ETXk\US*\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXD\ETX\DC2\ETXk-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STXE\DC2\ETXl\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXE\ACK\DC2\ETXl\STX\RS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXE\SOH\DC2\ETXl\US)\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXE\ETX\DC2\ETXl-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STXF\DC2\ETXm\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXF\ACK\DC2\ETXm\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXF\SOH\DC2\ETXm\US(\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXF\ETX\DC2\ETXm-0\n\
    \\v\n\
    \\EOT\EOT\NUL\STXG\DC2\ETXn\STX1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXG\ACK\DC2\ETXn\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXG\SOH\DC2\ETXn\US&\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXG\ETX\DC2\ETXn-0\n\
    \\r\n\
    \\EOT\EOT\NUL\b\NUL\DC2\ENQp\STX\131\SOH\ETX\n\
    \\f\n\
    \\ENQ\EOT\NUL\b\NUL\SOH\DC2\ETXp\b\DC4\n\
    \\v\n\
    \\EOT\EOT\NUL\STXH\DC2\ETXq\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXH\ENQ\DC2\ETXq\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXH\SOH\DC2\ETXq\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXH\ETX\DC2\ETXq#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXI\DC2\ETXr\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXI\ENQ\DC2\ETXr\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXI\SOH\DC2\ETXr\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXI\ETX\DC2\ETXr#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXJ\DC2\ETXs\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXJ\ENQ\DC2\ETXs\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXJ\SOH\DC2\ETXs\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXJ\ETX\DC2\ETXs#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXK\DC2\ETXt\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXK\ENQ\DC2\ETXt\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXK\SOH\DC2\ETXt\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXK\ETX\DC2\ETXt#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXL\DC2\ETXu\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXL\ENQ\DC2\ETXu\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXL\SOH\DC2\ETXu\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXL\ETX\DC2\ETXu#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXM\DC2\ETXv\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXM\ENQ\DC2\ETXv\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXM\SOH\DC2\ETXv\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXM\ETX\DC2\ETXv#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXN\DC2\ETXw\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXN\ENQ\DC2\ETXw\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXN\SOH\DC2\ETXw\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXN\ETX\DC2\ETXw#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXO\DC2\ETXx\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXO\ENQ\DC2\ETXx\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXO\SOH\DC2\ETXx\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXO\ETX\DC2\ETXx#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXP\DC2\ETXy\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXP\ENQ\DC2\ETXy\EOT\v\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXP\SOH\DC2\ETXy\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXP\ETX\DC2\ETXy#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXQ\DC2\ETXz\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXQ\ENQ\DC2\ETXz\EOT\v\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXQ\SOH\DC2\ETXz\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXQ\ETX\DC2\ETXz#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXR\DC2\ETX{\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXR\ENQ\DC2\ETX{\EOT\f\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXR\SOH\DC2\ETX{\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXR\ETX\DC2\ETX{#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXS\DC2\ETX|\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXS\ENQ\DC2\ETX|\EOT\f\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXS\SOH\DC2\ETX|\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXS\ETX\DC2\ETX|#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXT\DC2\ETX}\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXT\ENQ\DC2\ETX}\EOT\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXT\SOH\DC2\ETX}\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXT\ETX\DC2\ETX}#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXU\DC2\ETX~\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXU\ENQ\DC2\ETX~\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXU\SOH\DC2\ETX~\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXU\ETX\DC2\ETX~#&\n\
    \\v\n\
    \\EOT\EOT\NUL\STXV\DC2\ETX\DEL\EOT'\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXV\ENQ\DC2\ETX\DEL\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\STXV\SOH\DC2\ETX\DEL\DC3 \n\
    \\f\n\
    \\ENQ\EOT\NUL\STXV\ETX\DC2\ETX\DEL#&\n\
    \\f\n\
    \\EOT\EOT\NUL\STXW\DC2\EOT\128\SOH\EOT'\n\
    \\r\n\
    \\ENQ\EOT\NUL\STXW\ACK\DC2\EOT\128\SOH\EOT\DC2\n\
    \\r\n\
    \\ENQ\EOT\NUL\STXW\SOH\DC2\EOT\128\SOH\DC3\US\n\
    \\r\n\
    \\ENQ\EOT\NUL\STXW\ETX\DC2\EOT\128\SOH#&\n\
    \\f\n\
    \\EOT\EOT\NUL\STXX\DC2\EOT\129\SOH\EOT'\n\
    \\r\n\
    \\ENQ\EOT\NUL\STXX\ACK\DC2\EOT\129\SOH\EOT\DC1\n\
    \\r\n\
    \\ENQ\EOT\NUL\STXX\SOH\DC2\EOT\129\SOH\DC3\RS\n\
    \\r\n\
    \\ENQ\EOT\NUL\STXX\ETX\DC2\EOT\129\SOH#&\n\
    \\f\n\
    \\EOT\EOT\NUL\STXY\DC2\EOT\130\SOH\EOT'\n\
    \\r\n\
    \\ENQ\EOT\NUL\STXY\ACK\DC2\EOT\130\SOH\EOT\SI\n\
    \\r\n\
    \\ENQ\EOT\NUL\STXY\SOH\DC2\EOT\130\SOH\DC3\FS\n\
    \\r\n\
    \\ENQ\EOT\NUL\STXY\ETX\DC2\EOT\130\SOH#&\n\
    \\f\n\
    \\STX\ENQ\NUL\DC2\ACK\134\SOH\NUL\138\SOH\SOH\n\
    \\v\n\
    \\ETX\ENQ\NUL\SOH\DC2\EOT\134\SOH\ENQ\DLE\n\
    \\f\n\
    \\EOT\ENQ\NUL\STX\NUL\DC2\EOT\135\SOH\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\NUL\SOH\DC2\EOT\135\SOH\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\NUL\STX\DC2\EOT\135\SOH\v\f\n\
    \\f\n\
    \\EOT\ENQ\NUL\STX\SOH\DC2\EOT\136\SOH\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\SOH\SOH\DC2\EOT\136\SOH\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\SOH\STX\DC2\EOT\136\SOH\v\f\n\
    \\f\n\
    \\EOT\ENQ\NUL\STX\STX\DC2\EOT\137\SOH\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\STX\SOH\DC2\EOT\137\SOH\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\STX\STX\DC2\EOT\137\SOH\v\f\n\
    \\f\n\
    \\STX\EOT\SOH\DC2\ACK\140\SOH\NUL\141\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\SOH\SOH\DC2\EOT\140\SOH\b\SYN\n\
    \\f\n\
    \\STX\ACK\NUL\DC2\ACK\143\SOH\NUL\149\SOH\SOH\n\
    \\v\n\
    \\ETX\ACK\NUL\SOH\DC2\EOT\143\SOH\b\SYN\n\
    \\f\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\EOT\144\SOH\STX4\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\EOT\144\SOH\ACK\DC3\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\EOT\144\SOH\DC4\"\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\EOT\144\SOH-2\n\
    \\f\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\EOT\145\SOH\STXA\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\EOT\145\SOH\ACK\DC2\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\EOT\145\SOH\DC3/\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\EOT\145\SOH:?\n\
    \\f\n\
    \\EOT\ACK\NUL\STX\STX\DC2\EOT\147\SOH\STX4\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\STX\SOH\DC2\EOT\147\SOH\ACK\DC3\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\STX\STX\DC2\EOT\147\SOH\DC4\EM\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\STX\ETX\DC2\EOT\147\SOH$2\n\
    \\f\n\
    \\EOT\ACK\NUL\STX\ETX\DC2\EOT\148\SOH\STXA\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ETX\SOH\DC2\EOT\148\SOH\ACK\DC2\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ETX\STX\DC2\EOT\148\SOH\DC3\CAN\n\
    \\r\n\
    \\ENQ\ACK\NUL\STX\ETX\ETX\DC2\EOT\148\SOH#?\n\
    \\f\n\
    \\STX\EOT\STX\DC2\ACK\151\SOH\NUL\152\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\STX\SOH\DC2\EOT\151\SOH\b\rb\ACKproto3"