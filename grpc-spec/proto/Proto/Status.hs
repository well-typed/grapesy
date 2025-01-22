{-# OPTIONS_GHC -Wno-prepositive-qualified-module -Wno-identities #-}
{- This file was auto-generated from status.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Status (
        Status()
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
     
         * 'Proto.Status_Fields.code' @:: Lens' Status Data.Int.Int32@
         * 'Proto.Status_Fields.message' @:: Lens' Status Data.Text.Text@
         * 'Proto.Status_Fields.details' @:: Lens' Status [Proto.Google.Protobuf.Any.Any]@
         * 'Proto.Status_Fields.vec'details' @:: Lens' Status (Data.Vector.Vector Proto.Google.Protobuf.Any.Any)@ -}
data Status
  = Status'_constructor {_Status'code :: !Data.Int.Int32,
                         _Status'message :: !Data.Text.Text,
                         _Status'details :: !(Data.Vector.Vector Proto.Google.Protobuf.Any.Any),
                         _Status'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Status where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Status "code" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Status'code (\ x__ y__ -> x__ {_Status'code = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Status "message" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Status'message (\ x__ y__ -> x__ {_Status'message = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Status "details" [Proto.Google.Protobuf.Any.Any] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Status'details (\ x__ y__ -> x__ {_Status'details = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Status "vec'details" (Data.Vector.Vector Proto.Google.Protobuf.Any.Any) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Status'details (\ x__ y__ -> x__ {_Status'details = y__}))
        Prelude.id
instance Data.ProtoLens.Message Status where
  messageName _ = Data.Text.pack "google.rpc.Status"
  packedMessageDescriptor _
    = "\n\
      \\ACKStatus\DC2\DC2\n\
      \\EOTcode\CAN\SOH \SOH(\ENQR\EOTcode\DC2\CAN\n\
      \\amessage\CAN\STX \SOH(\tR\amessage\DC2.\n\
      \\adetails\CAN\ETX \ETX(\v2\DC4.google.protobuf.AnyR\adetails"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        code__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "code"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"code")) ::
              Data.ProtoLens.FieldDescriptor Status
        message__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"message")) ::
              Data.ProtoLens.FieldDescriptor Status
        details__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "details"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Google.Protobuf.Any.Any)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"details")) ::
              Data.ProtoLens.FieldDescriptor Status
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, code__field_descriptor),
           (Data.ProtoLens.Tag 2, message__field_descriptor),
           (Data.ProtoLens.Tag 3, details__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Status'_unknownFields
        (\ x__ y__ -> x__ {_Status'_unknownFields = y__})
  defMessage
    = Status'_constructor
        {_Status'code = Data.ProtoLens.fieldDefault,
         _Status'message = Data.ProtoLens.fieldDefault,
         _Status'details = Data.Vector.Generic.empty,
         _Status'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Status
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.Google.Protobuf.Any.Any
             -> Data.ProtoLens.Encoding.Bytes.Parser Status
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
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "code"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"code") y x)
                                  mutable'details
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "message"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"message") y x)
                                  mutable'details
                        26
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
          "Status"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"code") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"message") _x
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
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'details") _x))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Status where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Status'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Status'code x__)
                (Control.DeepSeq.deepseq
                   (_Status'message x__)
                   (Control.DeepSeq.deepseq (_Status'details x__) ())))
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\fstatus.proto\DC2\n\
    \google.rpc\SUB\EMgoogle/protobuf/any.proto\"f\n\
    \\ACKStatus\DC2\DC2\n\
    \\EOTcode\CAN\SOH \SOH(\ENQR\EOTcode\DC2\CAN\n\
    \\amessage\CAN\STX \SOH(\tR\amessage\DC2.\n\
    \\adetails\CAN\ETX \ETX(\v2\DC4.google.protobuf.AnyR\adetailsB^\n\
    \\SOcom.google.rpcB\vStatusProtoP\SOHZ7google.golang.org/genproto/googleapis/rpc/status;status\162\STX\ETXRPCJ\139\RS\n\
    \\ACK\DC2\EOT\SO\NUL[\SOH\n\
    \\189\EOT\n\
    \\SOH\f\DC2\ETX\SO\NUL\DC22\178\EOT Copyright 2016 Google Inc.\n\
    \\n\
    \ Licensed under the Apache License, Version 2.0 (the \"License\");\n\
    \ you may not use this file except in compliance with the License.\n\
    \ You may obtain a copy of the License at\n\
    \\n\
    \     http://www.apache.org/licenses/LICENSE-2.0\n\
    \\n\
    \ Unless required by applicable law or agreed to in writing, software\n\
    \ distributed under the License is distributed on an \"AS IS\" BASIS,\n\
    \ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n\
    \ See the License for the specific language governing permissions and\n\
    \ limitations under the License.\n\
    \\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\DLE\NUL\DC3\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\DC2\NUL#\n\
    \\b\n\
    \\SOH\b\DC2\ETX\DC4\NULN\n\
    \\t\n\
    \\STX\b\v\DC2\ETX\DC4\NULN\n\
    \\b\n\
    \\SOH\b\DC2\ETX\NAK\NUL\"\n\
    \\t\n\
    \\STX\b\n\
    \\DC2\ETX\NAK\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX\SYN\NUL,\n\
    \\t\n\
    \\STX\b\b\DC2\ETX\SYN\NUL,\n\
    \\b\n\
    \\SOH\b\DC2\ETX\ETB\NUL'\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\ETB\NUL'\n\
    \\b\n\
    \\SOH\b\DC2\ETX\CAN\NUL!\n\
    \\t\n\
    \\STX\b$\DC2\ETX\CAN\NUL!\n\
    \\214\DC3\n\
    \\STX\EOT\NUL\DC2\EOTO\NUL[\SOH\SUB\201\DC3 The `Status` type defines a logical error model that is suitable for different\n\
    \ programming environments, including REST APIs and RPC APIs. It is used by\n\
    \ [gRPC](https://github.com/grpc). The error model is designed to be:\n\
    \\n\
    \ - Simple to use and understand for most users\n\
    \ - Flexible enough to meet unexpected needs\n\
    \\n\
    \ # Overview\n\
    \\n\
    \ The `Status` message contains three pieces of data: error code, error message,\n\
    \ and error details. The error code should be an enum value of\n\
    \ [google.rpc.Code][google.rpc.Code], but it may accept additional error codes if needed.  The\n\
    \ error message should be a developer-facing English message that helps\n\
    \ developers *understand* and *resolve* the error. If a localized user-facing\n\
    \ error message is needed, put the localized message in the error details or\n\
    \ localize it in the client. The optional error details may contain arbitrary\n\
    \ information about the error. There is a predefined set of error detail types\n\
    \ in the package `google.rpc` which can be used for common error conditions.\n\
    \\n\
    \ # Language mapping\n\
    \\n\
    \ The `Status` message is the logical representation of the error model, but it\n\
    \ is not necessarily the actual wire format. When the `Status` message is\n\
    \ exposed in different client libraries and different wire protocols, it can be\n\
    \ mapped differently. For example, it will likely be mapped to some exceptions\n\
    \ in Java, but more likely mapped to some error codes in C.\n\
    \\n\
    \ # Other uses\n\
    \\n\
    \ The error model and the `Status` message can be used in a variety of\n\
    \ environments, either with or without APIs, to provide a\n\
    \ consistent developer experience across different environments.\n\
    \\n\
    \ Example uses of this error model include:\n\
    \\n\
    \ - Partial errors. If a service needs to return partial errors to the client,\n\
    \     it may embed the `Status` in the normal response to indicate the partial\n\
    \     errors.\n\
    \\n\
    \ - Workflow errors. A typical workflow has multiple steps. Each step may\n\
    \     have a `Status` message for error reporting purpose.\n\
    \\n\
    \ - Batch operations. If a client uses batch request and batch response, the\n\
    \     `Status` message should be used directly inside batch response, one for\n\
    \     each error sub-response.\n\
    \\n\
    \ - Asynchronous operations. If an API call embeds asynchronous operation\n\
    \     results in its response, the status of those operations should be\n\
    \     represented directly using the `Status` message.\n\
    \\n\
    \ - Logging. If some API errors are stored in logs, the message `Status` could\n\
    \     be used directly after any stripping needed for security/privacy reasons.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETXO\b\SO\n\
    \d\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETXQ\STX\DC1\SUBW The status code, which should be an enum value of [google.rpc.Code][google.rpc.Code].\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETXQ\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETXQ\b\f\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETXQ\SI\DLE\n\
    \\235\SOH\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETXV\STX\NAK\SUB\221\SOH A developer-facing error message, which should be in English. Any\n\
    \ user-facing error message should be localized and sent in the\n\
    \ [google.rpc.Status.details][google.rpc.Status.details] field, or localized by the client.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ENQ\DC2\ETXV\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETXV\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETXV\DC3\DC4\n\
    \~\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETXZ\STX+\SUBq A list of messages that carry the error details.  There will be a\n\
    \ common set of message types for APIs to use.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\EOT\DC2\ETXZ\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ACK\DC2\ETXZ\v\RS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETXZ\US&\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETXZ)*b\ACKproto3"