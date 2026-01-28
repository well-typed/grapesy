{-# OPTIONS_GHC -Wno-prepositive-qualified-module -Wno-identities #-}
{- This file was auto-generated from test-any.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.TestAny (
        TestAnyService(..), A(), B(), TestAnyMsg()
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
import qualified Proto.Google.Protobuf.Any
{- | Fields :
     
         * 'Proto.TestAny_Fields.a' @:: Lens' A Data.Int.Int32@ -}
data A
  = A'_constructor {_A'a :: !Data.Int.Int32,
                    _A'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show A where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField A "a" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens _A'a (\ x__ y__ -> x__ {_A'a = y__}))
        Prelude.id
instance Data.ProtoLens.Message A where
  messageName _ = Data.Text.pack "A"
  packedMessageDescriptor _
    = "\n\
      \\SOHA\DC2\f\n\
      \\SOHa\CAN\SOH \SOH(\ENQR\SOHa"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        a__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "a"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"a")) ::
              Data.ProtoLens.FieldDescriptor A
      in Data.Map.fromList [(Data.ProtoLens.Tag 1, a__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _A'_unknownFields (\ x__ y__ -> x__ {_A'_unknownFields = y__})
  defMessage
    = A'_constructor
        {_A'a = Data.ProtoLens.fieldDefault, _A'_unknownFields = []}
  parseMessage
    = let
        loop :: A -> Data.ProtoLens.Encoding.Bytes.Parser A
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
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "a"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"a") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "A"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"a") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData A where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_A'_unknownFields x__) (Control.DeepSeq.deepseq (_A'a x__) ())
{- | Fields :
     
         * 'Proto.TestAny_Fields.b' @:: Lens' B Data.Int.Int32@ -}
data B
  = B'_constructor {_B'b :: !Data.Int.Int32,
                    _B'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show B where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField B "b" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens _B'b (\ x__ y__ -> x__ {_B'b = y__}))
        Prelude.id
instance Data.ProtoLens.Message B where
  messageName _ = Data.Text.pack "B"
  packedMessageDescriptor _
    = "\n\
      \\SOHB\DC2\f\n\
      \\SOHb\CAN\SOH \SOH(\ENQR\SOHb"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        b__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "b"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"b")) ::
              Data.ProtoLens.FieldDescriptor B
      in Data.Map.fromList [(Data.ProtoLens.Tag 1, b__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _B'_unknownFields (\ x__ y__ -> x__ {_B'_unknownFields = y__})
  defMessage
    = B'_constructor
        {_B'b = Data.ProtoLens.fieldDefault, _B'_unknownFields = []}
  parseMessage
    = let
        loop :: B -> Data.ProtoLens.Encoding.Bytes.Parser B
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
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "b"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"b") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "B"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"b") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData B where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_B'_unknownFields x__) (Control.DeepSeq.deepseq (_B'b x__) ())
{- | Fields :
     
         * 'Proto.TestAny_Fields.message' @:: Lens' TestAnyMsg Data.Text.Text@
         * 'Proto.TestAny_Fields.details' @:: Lens' TestAnyMsg [Proto.Google.Protobuf.Any.Any]@
         * 'Proto.TestAny_Fields.vec'details' @:: Lens' TestAnyMsg (Data.Vector.Vector Proto.Google.Protobuf.Any.Any)@ -}
data TestAnyMsg
  = TestAnyMsg'_constructor {_TestAnyMsg'message :: !Data.Text.Text,
                             _TestAnyMsg'details :: !(Data.Vector.Vector Proto.Google.Protobuf.Any.Any),
                             _TestAnyMsg'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TestAnyMsg where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TestAnyMsg "message" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TestAnyMsg'message (\ x__ y__ -> x__ {_TestAnyMsg'message = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TestAnyMsg "details" [Proto.Google.Protobuf.Any.Any] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TestAnyMsg'details (\ x__ y__ -> x__ {_TestAnyMsg'details = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField TestAnyMsg "vec'details" (Data.Vector.Vector Proto.Google.Protobuf.Any.Any) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TestAnyMsg'details (\ x__ y__ -> x__ {_TestAnyMsg'details = y__}))
        Prelude.id
instance Data.ProtoLens.Message TestAnyMsg where
  messageName _ = Data.Text.pack "TestAnyMsg"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \TestAnyMsg\DC2\CAN\n\
      \\amessage\CAN\SOH \SOH(\tR\amessage\DC2.\n\
      \\adetails\CAN\STX \ETX(\v2\DC4.google.protobuf.AnyR\adetails"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        message__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"message")) ::
              Data.ProtoLens.FieldDescriptor TestAnyMsg
        details__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "details"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.Any.Any)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"details")) ::
              Data.ProtoLens.FieldDescriptor TestAnyMsg
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, message__field_descriptor),
           (Data.ProtoLens.Tag 2, details__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TestAnyMsg'_unknownFields
        (\ x__ y__ -> x__ {_TestAnyMsg'_unknownFields = y__})
  defMessage
    = TestAnyMsg'_constructor
        {_TestAnyMsg'message = Data.ProtoLens.fieldDefault,
         _TestAnyMsg'details = Data.Vector.Generic.empty,
         _TestAnyMsg'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TestAnyMsg
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.Google.Protobuf.Any.Any
             -> Data.ProtoLens.Encoding.Bytes.Parser TestAnyMsg
        loop x mutable'details
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'details <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'details)
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
                              (Data.ProtoLens.Field.field @"vec'details") frozen'details x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "message"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"message") y x)
                                  mutable'details
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "details"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'details y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'details
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'details <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'details)
          "TestAnyMsg"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"message") _x
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
                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage _v))
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'details") _x))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData TestAnyMsg where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TestAnyMsg'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TestAnyMsg'message x__)
                (Control.DeepSeq.deepseq (_TestAnyMsg'details x__) ()))
data TestAnyService = TestAnyService {}
instance Data.ProtoLens.Service.Types.Service TestAnyService where
  type ServiceName TestAnyService = "TestAnyService"
  type ServicePackage TestAnyService = ""
  type ServiceMethods TestAnyService = '["reverse"]
  packedServiceDescriptor _
    = "\n\
      \\SOTestAnyService\DC2#\n\
      \\aReverse\DC2\v.TestAnyMsg\SUB\v.TestAnyMsg"
instance Data.ProtoLens.Service.Types.HasMethodImpl TestAnyService "reverse" where
  type MethodName TestAnyService "reverse" = "Reverse"
  type MethodInput TestAnyService "reverse" = TestAnyMsg
  type MethodOutput TestAnyService "reverse" = TestAnyMsg
  type MethodStreamingType TestAnyService "reverse" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\SOtest-any.proto\SUB\EMgoogle/protobuf/any.proto\"V\n\
    \\n\
    \TestAnyMsg\DC2\CAN\n\
    \\amessage\CAN\SOH \SOH(\tR\amessage\DC2.\n\
    \\adetails\CAN\STX \ETX(\v2\DC4.google.protobuf.AnyR\adetails\"\DC1\n\
    \\SOHA\DC2\f\n\
    \\SOHa\CAN\SOH \SOH(\ENQR\SOHa\"\DC1\n\
    \\SOHB\DC2\f\n\
    \\SOHb\CAN\SOH \SOH(\ENQR\SOHb25\n\
    \\SOTestAnyService\DC2#\n\
    \\aReverse\DC2\v.TestAnyMsg\SUB\v.TestAnyMsgJ\158\ETX\n\
    \\ACK\DC2\EOT\NUL\NUL\DC3\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\STX\NUL#\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\EOT\NUL\ACK\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\EOT\b\SYN\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\ENQ\STX0\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\ENQ\ACK\r\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\ENQ\SI\EM\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\ENQ$.\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\b\NUL\v\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\b\b\DC2\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\t\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\t\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\t\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\t\DC3\DC4\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\n\
    \\STX+\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\EOT\DC2\ETX\n\
    \\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ACK\DC2\ETX\n\
    \\v\RS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\n\
    \\US&\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\n\
    \)*\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\r\NUL\SI\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\r\b\t\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\SO\STX\SO\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\SO\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\SO\b\t\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\SO\f\r\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\DC1\NUL\DC3\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\DC1\b\t\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\DC2\STX\SO\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\DC2\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\DC2\b\t\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\DC2\f\rb\ACKproto3"