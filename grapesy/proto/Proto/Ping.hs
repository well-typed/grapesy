{- This file was auto-generated from ping.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
module Proto.Ping (
        PingService(..), PingMessage(), PongMessage()
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

         * 'Proto.Ping_Fields.id' @:: Lens' PingMessage Data.Word.Word32@ -}
data PingMessage
  = PingMessage'_constructor {_PingMessage'id :: !Data.Word.Word32,
                              _PingMessage'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PingMessage where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField PingMessage "id" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PingMessage'id (\ x__ y__ -> x__ {_PingMessage'id = y__}))
        Prelude.id
instance Data.ProtoLens.Message PingMessage where
  messageName _ = Data.Text.pack "PingMessage"
  packedMessageDescriptor _
    = "\n\
      \\vPingMessage\DC2\SO\n\
      \\STXid\CAN\SOH \SOH(\rR\STXid"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        id__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"id")) ::
              Data.ProtoLens.FieldDescriptor PingMessage
      in Data.Map.fromList [(Data.ProtoLens.Tag 1, id__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PingMessage'_unknownFields
        (\ x__ y__ -> x__ {_PingMessage'_unknownFields = y__})
  defMessage
    = PingMessage'_constructor
        {_PingMessage'id = Data.ProtoLens.fieldDefault,
         _PingMessage'_unknownFields = []}
  parseMessage
    = let
        loop ::
          PingMessage -> Data.ProtoLens.Encoding.Bytes.Parser PingMessage
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
                                       "id"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"id") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "PingMessage"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"id") _x
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
instance Control.DeepSeq.NFData PingMessage where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PingMessage'_unknownFields x__)
             (Control.DeepSeq.deepseq (_PingMessage'id x__) ())
{- | Fields :

         * 'Proto.Ping_Fields.id' @:: Lens' PongMessage Data.Word.Word32@ -}
data PongMessage
  = PongMessage'_constructor {_PongMessage'id :: !Data.Word.Word32,
                              _PongMessage'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PongMessage where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField PongMessage "id" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PongMessage'id (\ x__ y__ -> x__ {_PongMessage'id = y__}))
        Prelude.id
instance Data.ProtoLens.Message PongMessage where
  messageName _ = Data.Text.pack "PongMessage"
  packedMessageDescriptor _
    = "\n\
      \\vPongMessage\DC2\SO\n\
      \\STXid\CAN\SOH \SOH(\rR\STXid"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        id__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"id")) ::
              Data.ProtoLens.FieldDescriptor PongMessage
      in Data.Map.fromList [(Data.ProtoLens.Tag 1, id__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PongMessage'_unknownFields
        (\ x__ y__ -> x__ {_PongMessage'_unknownFields = y__})
  defMessage
    = PongMessage'_constructor
        {_PongMessage'id = Data.ProtoLens.fieldDefault,
         _PongMessage'_unknownFields = []}
  parseMessage
    = let
        loop ::
          PongMessage -> Data.ProtoLens.Encoding.Bytes.Parser PongMessage
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
                                       "id"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"id") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "PongMessage"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"id") _x
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
instance Control.DeepSeq.NFData PongMessage where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PongMessage'_unknownFields x__)
             (Control.DeepSeq.deepseq (_PongMessage'id x__) ())
data PingService = PingService {}
instance Data.ProtoLens.Service.Types.Service PingService where
  type ServiceName PingService = "PingService"
  type ServicePackage PingService = ""
  type ServiceMethods PingService = '["ping"]
  packedServiceDescriptor _
    = "\n\
      \\vPingService\DC2\"\n\
      \\EOTPing\DC2\f.PingMessage\SUB\f.PongMessage"
instance Data.ProtoLens.Service.Types.HasMethodImpl PingService "ping" where
  type MethodName PingService "ping" = "Ping"
  type MethodInput PingService "ping" = PingMessage
  type MethodOutput PingService "ping" = PongMessage
  type MethodStreamingType PingService "ping" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\n\
    \ping.proto\"\GS\n\
    \\vPingMessage\DC2\SO\n\
    \\STXid\CAN\SOH \SOH(\rR\STXid\"\GS\n\
    \\vPongMessage\DC2\SO\n\
    \\STXid\CAN\SOH \SOH(\rR\STXid21\n\
    \\vPingService\DC2\"\n\
    \\EOTPing\DC2\f.PingMessage\SUB\f.PongMessageJ\255\SOH\n\
    \\ACK\DC2\EOT\NUL\NUL\f\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\STX\NUL\EOT\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\STX\b\DC3\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\ETX\STX/\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\ETX\ACK\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\ETX\f\ETB\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\ETX\"-\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\ACK\NUL\b\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\ACK\b\DC3\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\a\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\a\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\a\t\v\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\a\SO\SI\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\n\
    \\NUL\f\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\n\
    \\b\DC3\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\v\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\v\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\v\t\v\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\v\SO\SIb\ACKproto3"