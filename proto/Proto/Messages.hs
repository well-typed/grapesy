{- This file was auto-generated from messages.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
module Proto.Messages (
        BoolValue(), ClientConfigureRequest(),
        ClientConfigureRequest'Metadata(),
        ClientConfigureRequest'RpcType(..),
        ClientConfigureRequest'RpcType(),
        ClientConfigureRequest'RpcType'UnrecognizedValue,
        ClientConfigureResponse(), EchoStatus(), GrpclbRouteType(..),
        GrpclbRouteType(), GrpclbRouteType'UnrecognizedValue,
        HookRequest(), HookRequest'HookRequestCommand(..),
        HookRequest'HookRequestCommand(),
        HookRequest'HookRequestCommand'UnrecognizedValue, HookResponse(),
        LoadBalancerAccumulatedStatsRequest(),
        LoadBalancerAccumulatedStatsResponse(),
        LoadBalancerAccumulatedStatsResponse'MethodStats(),
        LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry(),
        LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry(),
        LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry(),
        LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry(),
        LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry(),
        LoadBalancerStatsRequest(), LoadBalancerStatsResponse(),
        LoadBalancerStatsResponse'MetadataByPeer(),
        LoadBalancerStatsResponse'MetadataEntry(),
        LoadBalancerStatsResponse'MetadataType(..),
        LoadBalancerStatsResponse'MetadataType(),
        LoadBalancerStatsResponse'MetadataType'UnrecognizedValue,
        LoadBalancerStatsResponse'MetadatasByPeerEntry(),
        LoadBalancerStatsResponse'RpcMetadata(),
        LoadBalancerStatsResponse'RpcsByMethodEntry(),
        LoadBalancerStatsResponse'RpcsByPeer(),
        LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry(),
        LoadBalancerStatsResponse'RpcsByPeerEntry(), MemorySize(),
        Payload(), PayloadType(..), PayloadType(),
        PayloadType'UnrecognizedValue, ReconnectInfo(), ReconnectParams(),
        ResponseParameters(), SetReturnStatusRequest(), SimpleRequest(),
        SimpleResponse(), StreamingInputCallRequest(),
        StreamingInputCallResponse(), StreamingOutputCallRequest(),
        StreamingOutputCallResponse(), TestOrcaReport(),
        TestOrcaReport'RequestCostEntry(),
        TestOrcaReport'UtilizationEntry()
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

         * 'Proto.Messages_Fields.value' @:: Lens' BoolValue Prelude.Bool@ -}
data BoolValue
  = BoolValue'_constructor {_BoolValue'value :: !Prelude.Bool,
                            _BoolValue'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show BoolValue where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField BoolValue "value" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BoolValue'value (\ x__ y__ -> x__ {_BoolValue'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message BoolValue where
  messageName _ = Data.Text.pack "grpc.testing.BoolValue"
  packedMessageDescriptor _
    = "\n\
      \\tBoolValue\DC2\DC4\n\
      \\ENQvalue\CAN\SOH \SOH(\bR\ENQvalue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor BoolValue
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _BoolValue'_unknownFields
        (\ x__ y__ -> x__ {_BoolValue'_unknownFields = y__})
  defMessage
    = BoolValue'_constructor
        {_BoolValue'value = Data.ProtoLens.fieldDefault,
         _BoolValue'_unknownFields = []}
  parseMessage
    = let
        loop :: BoolValue -> Data.ProtoLens.Encoding.Bytes.Parser BoolValue
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
          (do loop Data.ProtoLens.defMessage) "BoolValue"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt (\ b -> if b then 1 else 0)
                         _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData BoolValue where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_BoolValue'_unknownFields x__)
             (Control.DeepSeq.deepseq (_BoolValue'value x__) ())
{- | Fields :

         * 'Proto.Messages_Fields.types' @:: Lens' ClientConfigureRequest [ClientConfigureRequest'RpcType]@
         * 'Proto.Messages_Fields.vec'types' @:: Lens' ClientConfigureRequest (Data.Vector.Vector ClientConfigureRequest'RpcType)@
         * 'Proto.Messages_Fields.metadata' @:: Lens' ClientConfigureRequest [ClientConfigureRequest'Metadata]@
         * 'Proto.Messages_Fields.vec'metadata' @:: Lens' ClientConfigureRequest (Data.Vector.Vector ClientConfigureRequest'Metadata)@
         * 'Proto.Messages_Fields.timeoutSec' @:: Lens' ClientConfigureRequest Data.Int.Int32@ -}
data ClientConfigureRequest
  = ClientConfigureRequest'_constructor {_ClientConfigureRequest'types :: !(Data.Vector.Vector ClientConfigureRequest'RpcType),
                                         _ClientConfigureRequest'metadata :: !(Data.Vector.Vector ClientConfigureRequest'Metadata),
                                         _ClientConfigureRequest'timeoutSec :: !Data.Int.Int32,
                                         _ClientConfigureRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ClientConfigureRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ClientConfigureRequest "types" [ClientConfigureRequest'RpcType] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ClientConfigureRequest'types
           (\ x__ y__ -> x__ {_ClientConfigureRequest'types = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ClientConfigureRequest "vec'types" (Data.Vector.Vector ClientConfigureRequest'RpcType) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ClientConfigureRequest'types
           (\ x__ y__ -> x__ {_ClientConfigureRequest'types = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ClientConfigureRequest "metadata" [ClientConfigureRequest'Metadata] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ClientConfigureRequest'metadata
           (\ x__ y__ -> x__ {_ClientConfigureRequest'metadata = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ClientConfigureRequest "vec'metadata" (Data.Vector.Vector ClientConfigureRequest'Metadata) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ClientConfigureRequest'metadata
           (\ x__ y__ -> x__ {_ClientConfigureRequest'metadata = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ClientConfigureRequest "timeoutSec" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ClientConfigureRequest'timeoutSec
           (\ x__ y__ -> x__ {_ClientConfigureRequest'timeoutSec = y__}))
        Prelude.id
instance Data.ProtoLens.Message ClientConfigureRequest where
  messageName _
    = Data.Text.pack "grpc.testing.ClientConfigureRequest"
  packedMessageDescriptor _
    = "\n\
      \\SYNClientConfigureRequest\DC2B\n\
      \\ENQtypes\CAN\SOH \ETX(\SO2,.grpc.testing.ClientConfigureRequest.RpcTypeR\ENQtypes\DC2I\n\
      \\bmetadata\CAN\STX \ETX(\v2-.grpc.testing.ClientConfigureRequest.MetadataR\bmetadata\DC2\US\n\
      \\vtimeout_sec\CAN\ETX \SOH(\ENQR\n\
      \timeoutSec\SUBt\n\
      \\bMetadata\DC2@\n\
      \\EOTtype\CAN\SOH \SOH(\SO2,.grpc.testing.ClientConfigureRequest.RpcTypeR\EOTtype\DC2\DLE\n\
      \\ETXkey\CAN\STX \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\ETX \SOH(\tR\ENQvalue\")\n\
      \\aRpcType\DC2\SO\n\
      \\n\
      \EMPTY_CALL\DLE\NUL\DC2\SO\n\
      \\n\
      \UNARY_CALL\DLE\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        types__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "types"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor ClientConfigureRequest'RpcType)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed (Data.ProtoLens.Field.field @"types")) ::
              Data.ProtoLens.FieldDescriptor ClientConfigureRequest
        metadata__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "metadata"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ClientConfigureRequest'Metadata)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"metadata")) ::
              Data.ProtoLens.FieldDescriptor ClientConfigureRequest
        timeoutSec__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "timeout_sec"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"timeoutSec")) ::
              Data.ProtoLens.FieldDescriptor ClientConfigureRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, types__field_descriptor),
           (Data.ProtoLens.Tag 2, metadata__field_descriptor),
           (Data.ProtoLens.Tag 3, timeoutSec__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ClientConfigureRequest'_unknownFields
        (\ x__ y__ -> x__ {_ClientConfigureRequest'_unknownFields = y__})
  defMessage
    = ClientConfigureRequest'_constructor
        {_ClientConfigureRequest'types = Data.Vector.Generic.empty,
         _ClientConfigureRequest'metadata = Data.Vector.Generic.empty,
         _ClientConfigureRequest'timeoutSec = Data.ProtoLens.fieldDefault,
         _ClientConfigureRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ClientConfigureRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ClientConfigureRequest'Metadata
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ClientConfigureRequest'RpcType
                -> Data.ProtoLens.Encoding.Bytes.Parser ClientConfigureRequest
        loop x mutable'metadata mutable'types
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'metadata <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                              mutable'metadata)
                      frozen'types <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'types)
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
                              (Data.ProtoLens.Field.field @"vec'metadata") frozen'metadata
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'types") frozen'types x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.toEnum
                                           (Prelude.fmap
                                              Prelude.fromIntegral
                                              Data.ProtoLens.Encoding.Bytes.getVarInt))
                                        "types"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'types y)
                                loop x mutable'metadata v
                        10
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
                                                                    "types"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'types)
                                loop x mutable'metadata y
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "metadata"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'metadata y)
                                loop x v mutable'types
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "timeout_sec"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"timeoutSec") y x)
                                  mutable'metadata mutable'types
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'metadata mutable'types
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'metadata <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
              mutable'types <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'metadata mutable'types)
          "ClientConfigureRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                p = Lens.Family2.view (Data.ProtoLens.Field.field @"vec'types") _x
              in
                if Data.Vector.Generic.null p then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         (Data.ProtoLens.Encoding.Bytes.runBuilder
                            (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                               ((Prelude..)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                                  Prelude.fromEnum)
                               p))))
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
                   (Lens.Family2.view
                      (Data.ProtoLens.Field.field @"vec'metadata") _x))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"timeoutSec") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ClientConfigureRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ClientConfigureRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ClientConfigureRequest'types x__)
                (Control.DeepSeq.deepseq
                   (_ClientConfigureRequest'metadata x__)
                   (Control.DeepSeq.deepseq
                      (_ClientConfigureRequest'timeoutSec x__) ())))
{- | Fields :

         * 'Proto.Messages_Fields.type'' @:: Lens' ClientConfigureRequest'Metadata ClientConfigureRequest'RpcType@
         * 'Proto.Messages_Fields.key' @:: Lens' ClientConfigureRequest'Metadata Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' ClientConfigureRequest'Metadata Data.Text.Text@ -}
data ClientConfigureRequest'Metadata
  = ClientConfigureRequest'Metadata'_constructor {_ClientConfigureRequest'Metadata'type' :: !ClientConfigureRequest'RpcType,
                                                  _ClientConfigureRequest'Metadata'key :: !Data.Text.Text,
                                                  _ClientConfigureRequest'Metadata'value :: !Data.Text.Text,
                                                  _ClientConfigureRequest'Metadata'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ClientConfigureRequest'Metadata where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ClientConfigureRequest'Metadata "type'" ClientConfigureRequest'RpcType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ClientConfigureRequest'Metadata'type'
           (\ x__ y__ -> x__ {_ClientConfigureRequest'Metadata'type' = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ClientConfigureRequest'Metadata "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ClientConfigureRequest'Metadata'key
           (\ x__ y__ -> x__ {_ClientConfigureRequest'Metadata'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ClientConfigureRequest'Metadata "value" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ClientConfigureRequest'Metadata'value
           (\ x__ y__ -> x__ {_ClientConfigureRequest'Metadata'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ClientConfigureRequest'Metadata where
  messageName _
    = Data.Text.pack "grpc.testing.ClientConfigureRequest.Metadata"
  packedMessageDescriptor _
    = "\n\
      \\bMetadata\DC2@\n\
      \\EOTtype\CAN\SOH \SOH(\SO2,.grpc.testing.ClientConfigureRequest.RpcTypeR\EOTtype\DC2\DLE\n\
      \\ETXkey\CAN\STX \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\ETX \SOH(\tR\ENQvalue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        type'__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor ClientConfigureRequest'RpcType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"type'")) ::
              Data.ProtoLens.FieldDescriptor ClientConfigureRequest'Metadata
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ClientConfigureRequest'Metadata
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ClientConfigureRequest'Metadata
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, type'__field_descriptor),
           (Data.ProtoLens.Tag 2, key__field_descriptor),
           (Data.ProtoLens.Tag 3, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ClientConfigureRequest'Metadata'_unknownFields
        (\ x__ y__
           -> x__ {_ClientConfigureRequest'Metadata'_unknownFields = y__})
  defMessage
    = ClientConfigureRequest'Metadata'_constructor
        {_ClientConfigureRequest'Metadata'type' = Data.ProtoLens.fieldDefault,
         _ClientConfigureRequest'Metadata'key = Data.ProtoLens.fieldDefault,
         _ClientConfigureRequest'Metadata'value = Data.ProtoLens.fieldDefault,
         _ClientConfigureRequest'Metadata'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ClientConfigureRequest'Metadata
          -> Data.ProtoLens.Encoding.Bytes.Parser ClientConfigureRequest'Metadata
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
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "type"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"type'") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        26
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
          (do loop Data.ProtoLens.defMessage) "Metadata"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"type'") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                         Prelude.fromEnum _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
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
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8 _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ClientConfigureRequest'Metadata where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ClientConfigureRequest'Metadata'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ClientConfigureRequest'Metadata'type' x__)
                (Control.DeepSeq.deepseq
                   (_ClientConfigureRequest'Metadata'key x__)
                   (Control.DeepSeq.deepseq
                      (_ClientConfigureRequest'Metadata'value x__) ())))
newtype ClientConfigureRequest'RpcType'UnrecognizedValue
  = ClientConfigureRequest'RpcType'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data ClientConfigureRequest'RpcType
  = ClientConfigureRequest'EMPTY_CALL |
    ClientConfigureRequest'UNARY_CALL |
    ClientConfigureRequest'RpcType'Unrecognized !ClientConfigureRequest'RpcType'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum ClientConfigureRequest'RpcType where
  maybeToEnum 0 = Prelude.Just ClientConfigureRequest'EMPTY_CALL
  maybeToEnum 1 = Prelude.Just ClientConfigureRequest'UNARY_CALL
  maybeToEnum k
    = Prelude.Just
        (ClientConfigureRequest'RpcType'Unrecognized
           (ClientConfigureRequest'RpcType'UnrecognizedValue
              (Prelude.fromIntegral k)))
  showEnum ClientConfigureRequest'EMPTY_CALL = "EMPTY_CALL"
  showEnum ClientConfigureRequest'UNARY_CALL = "UNARY_CALL"
  showEnum
    (ClientConfigureRequest'RpcType'Unrecognized (ClientConfigureRequest'RpcType'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "EMPTY_CALL"
    = Prelude.Just ClientConfigureRequest'EMPTY_CALL
    | (Prelude.==) k "UNARY_CALL"
    = Prelude.Just ClientConfigureRequest'UNARY_CALL
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded ClientConfigureRequest'RpcType where
  minBound = ClientConfigureRequest'EMPTY_CALL
  maxBound = ClientConfigureRequest'UNARY_CALL
instance Prelude.Enum ClientConfigureRequest'RpcType where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum RpcType: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum ClientConfigureRequest'EMPTY_CALL = 0
  fromEnum ClientConfigureRequest'UNARY_CALL = 1
  fromEnum
    (ClientConfigureRequest'RpcType'Unrecognized (ClientConfigureRequest'RpcType'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ ClientConfigureRequest'UNARY_CALL
    = Prelude.error
        "ClientConfigureRequest'RpcType.succ: bad argument ClientConfigureRequest'UNARY_CALL. This value would be out of bounds."
  succ ClientConfigureRequest'EMPTY_CALL
    = ClientConfigureRequest'UNARY_CALL
  succ (ClientConfigureRequest'RpcType'Unrecognized _)
    = Prelude.error
        "ClientConfigureRequest'RpcType.succ: bad argument: unrecognized value"
  pred ClientConfigureRequest'EMPTY_CALL
    = Prelude.error
        "ClientConfigureRequest'RpcType.pred: bad argument ClientConfigureRequest'EMPTY_CALL. This value would be out of bounds."
  pred ClientConfigureRequest'UNARY_CALL
    = ClientConfigureRequest'EMPTY_CALL
  pred (ClientConfigureRequest'RpcType'Unrecognized _)
    = Prelude.error
        "ClientConfigureRequest'RpcType.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault ClientConfigureRequest'RpcType where
  fieldDefault = ClientConfigureRequest'EMPTY_CALL
instance Control.DeepSeq.NFData ClientConfigureRequest'RpcType where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
      -}
data ClientConfigureResponse
  = ClientConfigureResponse'_constructor {_ClientConfigureResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ClientConfigureResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message ClientConfigureResponse where
  messageName _
    = Data.Text.pack "grpc.testing.ClientConfigureResponse"
  packedMessageDescriptor _
    = "\n\
      \\ETBClientConfigureResponse"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ClientConfigureResponse'_unknownFields
        (\ x__ y__ -> x__ {_ClientConfigureResponse'_unknownFields = y__})
  defMessage
    = ClientConfigureResponse'_constructor
        {_ClientConfigureResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ClientConfigureResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ClientConfigureResponse
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
          (do loop Data.ProtoLens.defMessage) "ClientConfigureResponse"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData ClientConfigureResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ClientConfigureResponse'_unknownFields x__) ()
{- | Fields :

         * 'Proto.Messages_Fields.code' @:: Lens' EchoStatus Data.Int.Int32@
         * 'Proto.Messages_Fields.message' @:: Lens' EchoStatus Data.Text.Text@ -}
data EchoStatus
  = EchoStatus'_constructor {_EchoStatus'code :: !Data.Int.Int32,
                             _EchoStatus'message :: !Data.Text.Text,
                             _EchoStatus'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EchoStatus where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField EchoStatus "code" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EchoStatus'code (\ x__ y__ -> x__ {_EchoStatus'code = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField EchoStatus "message" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EchoStatus'message (\ x__ y__ -> x__ {_EchoStatus'message = y__}))
        Prelude.id
instance Data.ProtoLens.Message EchoStatus where
  messageName _ = Data.Text.pack "grpc.testing.EchoStatus"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \EchoStatus\DC2\DC2\n\
      \\EOTcode\CAN\SOH \SOH(\ENQR\EOTcode\DC2\CAN\n\
      \\amessage\CAN\STX \SOH(\tR\amessage"
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
              Data.ProtoLens.FieldDescriptor EchoStatus
        message__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"message")) ::
              Data.ProtoLens.FieldDescriptor EchoStatus
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, code__field_descriptor),
           (Data.ProtoLens.Tag 2, message__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _EchoStatus'_unknownFields
        (\ x__ y__ -> x__ {_EchoStatus'_unknownFields = y__})
  defMessage
    = EchoStatus'_constructor
        {_EchoStatus'code = Data.ProtoLens.fieldDefault,
         _EchoStatus'message = Data.ProtoLens.fieldDefault,
         _EchoStatus'_unknownFields = []}
  parseMessage
    = let
        loop ::
          EchoStatus -> Data.ProtoLens.Encoding.Bytes.Parser EchoStatus
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
                                       "code"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"code") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "message"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"message") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "EchoStatus"
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
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData EchoStatus where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_EchoStatus'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_EchoStatus'code x__)
                (Control.DeepSeq.deepseq (_EchoStatus'message x__) ()))
newtype GrpclbRouteType'UnrecognizedValue
  = GrpclbRouteType'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data GrpclbRouteType
  = GRPCLB_ROUTE_TYPE_UNKNOWN |
    GRPCLB_ROUTE_TYPE_FALLBACK |
    GRPCLB_ROUTE_TYPE_BACKEND |
    GrpclbRouteType'Unrecognized !GrpclbRouteType'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum GrpclbRouteType where
  maybeToEnum 0 = Prelude.Just GRPCLB_ROUTE_TYPE_UNKNOWN
  maybeToEnum 1 = Prelude.Just GRPCLB_ROUTE_TYPE_FALLBACK
  maybeToEnum 2 = Prelude.Just GRPCLB_ROUTE_TYPE_BACKEND
  maybeToEnum k
    = Prelude.Just
        (GrpclbRouteType'Unrecognized
           (GrpclbRouteType'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum GRPCLB_ROUTE_TYPE_UNKNOWN = "GRPCLB_ROUTE_TYPE_UNKNOWN"
  showEnum GRPCLB_ROUTE_TYPE_FALLBACK = "GRPCLB_ROUTE_TYPE_FALLBACK"
  showEnum GRPCLB_ROUTE_TYPE_BACKEND = "GRPCLB_ROUTE_TYPE_BACKEND"
  showEnum
    (GrpclbRouteType'Unrecognized (GrpclbRouteType'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "GRPCLB_ROUTE_TYPE_UNKNOWN"
    = Prelude.Just GRPCLB_ROUTE_TYPE_UNKNOWN
    | (Prelude.==) k "GRPCLB_ROUTE_TYPE_FALLBACK"
    = Prelude.Just GRPCLB_ROUTE_TYPE_FALLBACK
    | (Prelude.==) k "GRPCLB_ROUTE_TYPE_BACKEND"
    = Prelude.Just GRPCLB_ROUTE_TYPE_BACKEND
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded GrpclbRouteType where
  minBound = GRPCLB_ROUTE_TYPE_UNKNOWN
  maxBound = GRPCLB_ROUTE_TYPE_BACKEND
instance Prelude.Enum GrpclbRouteType where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum GrpclbRouteType: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum GRPCLB_ROUTE_TYPE_UNKNOWN = 0
  fromEnum GRPCLB_ROUTE_TYPE_FALLBACK = 1
  fromEnum GRPCLB_ROUTE_TYPE_BACKEND = 2
  fromEnum
    (GrpclbRouteType'Unrecognized (GrpclbRouteType'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ GRPCLB_ROUTE_TYPE_BACKEND
    = Prelude.error
        "GrpclbRouteType.succ: bad argument GRPCLB_ROUTE_TYPE_BACKEND. This value would be out of bounds."
  succ GRPCLB_ROUTE_TYPE_UNKNOWN = GRPCLB_ROUTE_TYPE_FALLBACK
  succ GRPCLB_ROUTE_TYPE_FALLBACK = GRPCLB_ROUTE_TYPE_BACKEND
  succ (GrpclbRouteType'Unrecognized _)
    = Prelude.error
        "GrpclbRouteType.succ: bad argument: unrecognized value"
  pred GRPCLB_ROUTE_TYPE_UNKNOWN
    = Prelude.error
        "GrpclbRouteType.pred: bad argument GRPCLB_ROUTE_TYPE_UNKNOWN. This value would be out of bounds."
  pred GRPCLB_ROUTE_TYPE_FALLBACK = GRPCLB_ROUTE_TYPE_UNKNOWN
  pred GRPCLB_ROUTE_TYPE_BACKEND = GRPCLB_ROUTE_TYPE_FALLBACK
  pred (GrpclbRouteType'Unrecognized _)
    = Prelude.error
        "GrpclbRouteType.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault GrpclbRouteType where
  fieldDefault = GRPCLB_ROUTE_TYPE_UNKNOWN
instance Control.DeepSeq.NFData GrpclbRouteType where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :

         * 'Proto.Messages_Fields.command' @:: Lens' HookRequest HookRequest'HookRequestCommand@
         * 'Proto.Messages_Fields.grpcCodeToReturn' @:: Lens' HookRequest Data.Int.Int32@
         * 'Proto.Messages_Fields.grpcStatusDescription' @:: Lens' HookRequest Data.Text.Text@
         * 'Proto.Messages_Fields.serverPort' @:: Lens' HookRequest Data.Int.Int32@ -}
data HookRequest
  = HookRequest'_constructor {_HookRequest'command :: !HookRequest'HookRequestCommand,
                              _HookRequest'grpcCodeToReturn :: !Data.Int.Int32,
                              _HookRequest'grpcStatusDescription :: !Data.Text.Text,
                              _HookRequest'serverPort :: !Data.Int.Int32,
                              _HookRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show HookRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField HookRequest "command" HookRequest'HookRequestCommand where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _HookRequest'command
           (\ x__ y__ -> x__ {_HookRequest'command = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField HookRequest "grpcCodeToReturn" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _HookRequest'grpcCodeToReturn
           (\ x__ y__ -> x__ {_HookRequest'grpcCodeToReturn = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField HookRequest "grpcStatusDescription" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _HookRequest'grpcStatusDescription
           (\ x__ y__ -> x__ {_HookRequest'grpcStatusDescription = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField HookRequest "serverPort" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _HookRequest'serverPort
           (\ x__ y__ -> x__ {_HookRequest'serverPort = y__}))
        Prelude.id
instance Data.ProtoLens.Message HookRequest where
  messageName _ = Data.Text.pack "grpc.testing.HookRequest"
  packedMessageDescriptor _
    = "\n\
      \\vHookRequest\DC2F\n\
      \\acommand\CAN\SOH \SOH(\SO2,.grpc.testing.HookRequest.HookRequestCommandR\acommand\DC2-\n\
      \\DC3grpc_code_to_return\CAN\STX \SOH(\ENQR\DLEgrpcCodeToReturn\DC26\n\
      \\ETBgrpc_status_description\CAN\ETX \SOH(\tR\NAKgrpcStatusDescription\DC2\US\n\
      \\vserver_port\CAN\EOT \SOH(\ENQR\n\
      \serverPort\"F\n\
      \\DC2HookRequestCommand\DC2\SI\n\
      \\vUNSPECIFIED\DLE\NUL\DC2\t\n\
      \\ENQSTART\DLE\SOH\DC2\b\n\
      \\EOTSTOP\DLE\STX\DC2\n\
      \\n\
      \\ACKRETURN\DLE\ETX"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        command__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "command"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor HookRequest'HookRequestCommand)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"command")) ::
              Data.ProtoLens.FieldDescriptor HookRequest
        grpcCodeToReturn__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "grpc_code_to_return"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"grpcCodeToReturn")) ::
              Data.ProtoLens.FieldDescriptor HookRequest
        grpcStatusDescription__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "grpc_status_description"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"grpcStatusDescription")) ::
              Data.ProtoLens.FieldDescriptor HookRequest
        serverPort__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_port"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"serverPort")) ::
              Data.ProtoLens.FieldDescriptor HookRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, command__field_descriptor),
           (Data.ProtoLens.Tag 2, grpcCodeToReturn__field_descriptor),
           (Data.ProtoLens.Tag 3, grpcStatusDescription__field_descriptor),
           (Data.ProtoLens.Tag 4, serverPort__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _HookRequest'_unknownFields
        (\ x__ y__ -> x__ {_HookRequest'_unknownFields = y__})
  defMessage
    = HookRequest'_constructor
        {_HookRequest'command = Data.ProtoLens.fieldDefault,
         _HookRequest'grpcCodeToReturn = Data.ProtoLens.fieldDefault,
         _HookRequest'grpcStatusDescription = Data.ProtoLens.fieldDefault,
         _HookRequest'serverPort = Data.ProtoLens.fieldDefault,
         _HookRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          HookRequest -> Data.ProtoLens.Encoding.Bytes.Parser HookRequest
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
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "command"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"command") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "grpc_code_to_return"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"grpcCodeToReturn") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "grpc_status_description"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"grpcStatusDescription") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "server_port"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverPort") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "HookRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"command") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                         Prelude.fromEnum _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"grpcCodeToReturn") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"grpcStatusDescription") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8 _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view (Data.ProtoLens.Field.field @"serverPort") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData HookRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_HookRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_HookRequest'command x__)
                (Control.DeepSeq.deepseq
                   (_HookRequest'grpcCodeToReturn x__)
                   (Control.DeepSeq.deepseq
                      (_HookRequest'grpcStatusDescription x__)
                      (Control.DeepSeq.deepseq (_HookRequest'serverPort x__) ()))))
newtype HookRequest'HookRequestCommand'UnrecognizedValue
  = HookRequest'HookRequestCommand'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data HookRequest'HookRequestCommand
  = HookRequest'UNSPECIFIED |
    HookRequest'START |
    HookRequest'STOP |
    HookRequest'RETURN |
    HookRequest'HookRequestCommand'Unrecognized !HookRequest'HookRequestCommand'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum HookRequest'HookRequestCommand where
  maybeToEnum 0 = Prelude.Just HookRequest'UNSPECIFIED
  maybeToEnum 1 = Prelude.Just HookRequest'START
  maybeToEnum 2 = Prelude.Just HookRequest'STOP
  maybeToEnum 3 = Prelude.Just HookRequest'RETURN
  maybeToEnum k
    = Prelude.Just
        (HookRequest'HookRequestCommand'Unrecognized
           (HookRequest'HookRequestCommand'UnrecognizedValue
              (Prelude.fromIntegral k)))
  showEnum HookRequest'UNSPECIFIED = "UNSPECIFIED"
  showEnum HookRequest'START = "START"
  showEnum HookRequest'STOP = "STOP"
  showEnum HookRequest'RETURN = "RETURN"
  showEnum
    (HookRequest'HookRequestCommand'Unrecognized (HookRequest'HookRequestCommand'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UNSPECIFIED"
    = Prelude.Just HookRequest'UNSPECIFIED
    | (Prelude.==) k "START" = Prelude.Just HookRequest'START
    | (Prelude.==) k "STOP" = Prelude.Just HookRequest'STOP
    | (Prelude.==) k "RETURN" = Prelude.Just HookRequest'RETURN
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded HookRequest'HookRequestCommand where
  minBound = HookRequest'UNSPECIFIED
  maxBound = HookRequest'RETURN
instance Prelude.Enum HookRequest'HookRequestCommand where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum HookRequestCommand: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum HookRequest'UNSPECIFIED = 0
  fromEnum HookRequest'START = 1
  fromEnum HookRequest'STOP = 2
  fromEnum HookRequest'RETURN = 3
  fromEnum
    (HookRequest'HookRequestCommand'Unrecognized (HookRequest'HookRequestCommand'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ HookRequest'RETURN
    = Prelude.error
        "HookRequest'HookRequestCommand.succ: bad argument HookRequest'RETURN. This value would be out of bounds."
  succ HookRequest'UNSPECIFIED = HookRequest'START
  succ HookRequest'START = HookRequest'STOP
  succ HookRequest'STOP = HookRequest'RETURN
  succ (HookRequest'HookRequestCommand'Unrecognized _)
    = Prelude.error
        "HookRequest'HookRequestCommand.succ: bad argument: unrecognized value"
  pred HookRequest'UNSPECIFIED
    = Prelude.error
        "HookRequest'HookRequestCommand.pred: bad argument HookRequest'UNSPECIFIED. This value would be out of bounds."
  pred HookRequest'START = HookRequest'UNSPECIFIED
  pred HookRequest'STOP = HookRequest'START
  pred HookRequest'RETURN = HookRequest'STOP
  pred (HookRequest'HookRequestCommand'Unrecognized _)
    = Prelude.error
        "HookRequest'HookRequestCommand.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault HookRequest'HookRequestCommand where
  fieldDefault = HookRequest'UNSPECIFIED
instance Control.DeepSeq.NFData HookRequest'HookRequestCommand where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
      -}
data HookResponse
  = HookResponse'_constructor {_HookResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show HookResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message HookResponse where
  messageName _ = Data.Text.pack "grpc.testing.HookResponse"
  packedMessageDescriptor _
    = "\n\
      \\fHookResponse"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _HookResponse'_unknownFields
        (\ x__ y__ -> x__ {_HookResponse'_unknownFields = y__})
  defMessage
    = HookResponse'_constructor {_HookResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          HookResponse -> Data.ProtoLens.Encoding.Bytes.Parser HookResponse
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
          (do loop Data.ProtoLens.defMessage) "HookResponse"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData HookResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_HookResponse'_unknownFields x__) ()
{- | Fields :
      -}
data LoadBalancerAccumulatedStatsRequest
  = LoadBalancerAccumulatedStatsRequest'_constructor {_LoadBalancerAccumulatedStatsRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerAccumulatedStatsRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message LoadBalancerAccumulatedStatsRequest where
  messageName _
    = Data.Text.pack "grpc.testing.LoadBalancerAccumulatedStatsRequest"
  packedMessageDescriptor _
    = "\n\
      \#LoadBalancerAccumulatedStatsRequest"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerAccumulatedStatsRequest'_unknownFields
        (\ x__ y__
           -> x__ {_LoadBalancerAccumulatedStatsRequest'_unknownFields = y__})
  defMessage
    = LoadBalancerAccumulatedStatsRequest'_constructor
        {_LoadBalancerAccumulatedStatsRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerAccumulatedStatsRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerAccumulatedStatsRequest
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
          (do loop Data.ProtoLens.defMessage)
          "LoadBalancerAccumulatedStatsRequest"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData LoadBalancerAccumulatedStatsRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerAccumulatedStatsRequest'_unknownFields x__) ()
{- | Fields :

         * 'Proto.Messages_Fields.numRpcsStartedByMethod' @:: Lens' LoadBalancerAccumulatedStatsResponse (Data.Map.Map Data.Text.Text Data.Int.Int32)@
         * 'Proto.Messages_Fields.numRpcsSucceededByMethod' @:: Lens' LoadBalancerAccumulatedStatsResponse (Data.Map.Map Data.Text.Text Data.Int.Int32)@
         * 'Proto.Messages_Fields.numRpcsFailedByMethod' @:: Lens' LoadBalancerAccumulatedStatsResponse (Data.Map.Map Data.Text.Text Data.Int.Int32)@
         * 'Proto.Messages_Fields.statsPerMethod' @:: Lens' LoadBalancerAccumulatedStatsResponse (Data.Map.Map Data.Text.Text LoadBalancerAccumulatedStatsResponse'MethodStats)@ -}
data LoadBalancerAccumulatedStatsResponse
  = LoadBalancerAccumulatedStatsResponse'_constructor {_LoadBalancerAccumulatedStatsResponse'numRpcsStartedByMethod :: !(Data.Map.Map Data.Text.Text Data.Int.Int32),
                                                       _LoadBalancerAccumulatedStatsResponse'numRpcsSucceededByMethod :: !(Data.Map.Map Data.Text.Text Data.Int.Int32),
                                                       _LoadBalancerAccumulatedStatsResponse'numRpcsFailedByMethod :: !(Data.Map.Map Data.Text.Text Data.Int.Int32),
                                                       _LoadBalancerAccumulatedStatsResponse'statsPerMethod :: !(Data.Map.Map Data.Text.Text LoadBalancerAccumulatedStatsResponse'MethodStats),
                                                       _LoadBalancerAccumulatedStatsResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerAccumulatedStatsResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse "numRpcsStartedByMethod" (Data.Map.Map Data.Text.Text Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'numRpcsStartedByMethod
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'numRpcsStartedByMethod = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse "numRpcsSucceededByMethod" (Data.Map.Map Data.Text.Text Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'numRpcsSucceededByMethod
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'numRpcsSucceededByMethod = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse "numRpcsFailedByMethod" (Data.Map.Map Data.Text.Text Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'numRpcsFailedByMethod
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'numRpcsFailedByMethod = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse "statsPerMethod" (Data.Map.Map Data.Text.Text LoadBalancerAccumulatedStatsResponse'MethodStats) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'statsPerMethod
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'statsPerMethod = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerAccumulatedStatsResponse where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerAccumulatedStatsResponse"
  packedMessageDescriptor _
    = "\n\
      \$LoadBalancerAccumulatedStatsResponse\DC2\142\SOH\n\
      \\SUBnum_rpcs_started_by_method\CAN\SOH \ETX(\v2N.grpc.testing.LoadBalancerAccumulatedStatsResponse.NumRpcsStartedByMethodEntryR\SYNnumRpcsStartedByMethodB\STX\CAN\SOH\DC2\148\SOH\n\
      \\FSnum_rpcs_succeeded_by_method\CAN\STX \ETX(\v2P.grpc.testing.LoadBalancerAccumulatedStatsResponse.NumRpcsSucceededByMethodEntryR\CANnumRpcsSucceededByMethodB\STX\CAN\SOH\DC2\139\SOH\n\
      \\EMnum_rpcs_failed_by_method\CAN\ETX \ETX(\v2M.grpc.testing.LoadBalancerAccumulatedStatsResponse.NumRpcsFailedByMethodEntryR\NAKnumRpcsFailedByMethodB\STX\CAN\SOH\DC2p\n\
      \\DLEstats_per_method\CAN\EOT \ETX(\v2F.grpc.testing.LoadBalancerAccumulatedStatsResponse.StatsPerMethodEntryR\SOstatsPerMethod\SUBI\n\
      \\ESCNumRpcsStartedByMethodEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUBK\n\
      \\GSNumRpcsSucceededByMethodEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUBH\n\
      \\SUBNumRpcsFailedByMethodEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUB\207\SOH\n\
      \\vMethodStats\DC2!\n\
      \\frpcs_started\CAN\SOH \SOH(\ENQR\vrpcsStarted\DC2b\n\
      \\ACKresult\CAN\STX \ETX(\v2J.grpc.testing.LoadBalancerAccumulatedStatsResponse.MethodStats.ResultEntryR\ACKresult\SUB9\n\
      \\vResultEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\ENQR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUB\129\SOH\n\
      \\DC3StatsPerMethodEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2T\n\
      \\ENQvalue\CAN\STX \SOH(\v2>.grpc.testing.LoadBalancerAccumulatedStatsResponse.MethodStatsR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        numRpcsStartedByMethod__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "num_rpcs_started_by_method"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"numRpcsStartedByMethod")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse
        numRpcsSucceededByMethod__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "num_rpcs_succeeded_by_method"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"numRpcsSucceededByMethod")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse
        numRpcsFailedByMethod__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "num_rpcs_failed_by_method"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"numRpcsFailedByMethod")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse
        statsPerMethod__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "stats_per_method"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"statsPerMethod")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, numRpcsStartedByMethod__field_descriptor),
           (Data.ProtoLens.Tag 2, numRpcsSucceededByMethod__field_descriptor),
           (Data.ProtoLens.Tag 3, numRpcsFailedByMethod__field_descriptor),
           (Data.ProtoLens.Tag 4, statsPerMethod__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerAccumulatedStatsResponse'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerAccumulatedStatsResponse'_unknownFields = y__})
  defMessage
    = LoadBalancerAccumulatedStatsResponse'_constructor
        {_LoadBalancerAccumulatedStatsResponse'numRpcsStartedByMethod = Data.Map.empty,
         _LoadBalancerAccumulatedStatsResponse'numRpcsSucceededByMethod = Data.Map.empty,
         _LoadBalancerAccumulatedStatsResponse'numRpcsFailedByMethod = Data.Map.empty,
         _LoadBalancerAccumulatedStatsResponse'statsPerMethod = Data.Map.empty,
         _LoadBalancerAccumulatedStatsResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerAccumulatedStatsResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerAccumulatedStatsResponse
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
                          -> do !(entry :: LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                                                  (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                                                      Data.ProtoLens.Encoding.Bytes.isolate
                                                                                                                        (Prelude.fromIntegral
                                                                                                                           len)
                                                                                                                        Data.ProtoLens.parseMessage)
                                                                                                                  "num_rpcs_started_by_method"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"numRpcsStartedByMethod")
                                        (\ !t -> Data.Map.insert key value t) x))
                        18
                          -> do !(entry :: LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                                                    (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                                                        Data.ProtoLens.Encoding.Bytes.isolate
                                                                                                                          (Prelude.fromIntegral
                                                                                                                             len)
                                                                                                                          Data.ProtoLens.parseMessage)
                                                                                                                    "num_rpcs_succeeded_by_method"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"numRpcsSucceededByMethod")
                                        (\ !t -> Data.Map.insert key value t) x))
                        26
                          -> do !(entry :: LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                                                       (Prelude.fromIntegral
                                                                                                                          len)
                                                                                                                       Data.ProtoLens.parseMessage)
                                                                                                                 "num_rpcs_failed_by_method"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"numRpcsFailedByMethod")
                                        (\ !t -> Data.Map.insert key value t) x))
                        34
                          -> do !(entry :: LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                                          (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                                              Data.ProtoLens.Encoding.Bytes.isolate
                                                                                                                (Prelude.fromIntegral
                                                                                                                   len)
                                                                                                                Data.ProtoLens.parseMessage)
                                                                                                          "stats_per_method"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"statsPerMethod")
                                        (\ !t -> Data.Map.insert key value t) x))
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage)
          "LoadBalancerAccumulatedStatsResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.Monoid.mconcat
                (Prelude.map
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                    (Data.ProtoLens.defMessage ::
                                       LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry)))))
                   (Data.Map.toList
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"numRpcsStartedByMethod") _x))))
             ((Data.Monoid.<>)
                (Data.Monoid.mconcat
                   (Prelude.map
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                    (Lens.Family2.set
                                       (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                       (Data.ProtoLens.defMessage ::
                                          LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry)))))
                      (Data.Map.toList
                         (Lens.Family2.view
                            (Data.ProtoLens.Field.field @"numRpcsSucceededByMethod") _x))))
                ((Data.Monoid.<>)
                   (Data.Monoid.mconcat
                      (Prelude.map
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage
                                    (Lens.Family2.set
                                       (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                       (Lens.Family2.set
                                          (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                          (Data.ProtoLens.defMessage ::
                                             LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry)))))
                         (Data.Map.toList
                            (Lens.Family2.view
                               (Data.ProtoLens.Field.field @"numRpcsFailedByMethod") _x))))
                   ((Data.Monoid.<>)
                      (Data.Monoid.mconcat
                         (Prelude.map
                            (\ _v
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                    ((Prelude..)
                                       (\ bs
                                          -> (Data.Monoid.<>)
                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                  (Prelude.fromIntegral
                                                     (Data.ByteString.length bs)))
                                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                       Data.ProtoLens.encodeMessage
                                       (Lens.Family2.set
                                          (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                             (Data.ProtoLens.defMessage ::
                                                LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry)))))
                            (Data.Map.toList
                               (Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"statsPerMethod") _x))))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData LoadBalancerAccumulatedStatsResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerAccumulatedStatsResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerAccumulatedStatsResponse'numRpcsStartedByMethod x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerAccumulatedStatsResponse'numRpcsSucceededByMethod
                      x__)
                   (Control.DeepSeq.deepseq
                      (_LoadBalancerAccumulatedStatsResponse'numRpcsFailedByMethod x__)
                      (Control.DeepSeq.deepseq
                         (_LoadBalancerAccumulatedStatsResponse'statsPerMethod x__) ()))))
{- | Fields :

         * 'Proto.Messages_Fields.rpcsStarted' @:: Lens' LoadBalancerAccumulatedStatsResponse'MethodStats Data.Int.Int32@
         * 'Proto.Messages_Fields.result' @:: Lens' LoadBalancerAccumulatedStatsResponse'MethodStats (Data.Map.Map Data.Int.Int32 Data.Int.Int32)@ -}
data LoadBalancerAccumulatedStatsResponse'MethodStats
  = LoadBalancerAccumulatedStatsResponse'MethodStats'_constructor {_LoadBalancerAccumulatedStatsResponse'MethodStats'rpcsStarted :: !Data.Int.Int32,
                                                                   _LoadBalancerAccumulatedStatsResponse'MethodStats'result :: !(Data.Map.Map Data.Int.Int32 Data.Int.Int32),
                                                                   _LoadBalancerAccumulatedStatsResponse'MethodStats'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerAccumulatedStatsResponse'MethodStats where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'MethodStats "rpcsStarted" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'MethodStats'rpcsStarted
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'MethodStats'rpcsStarted = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'MethodStats "result" (Data.Map.Map Data.Int.Int32 Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'MethodStats'result
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'MethodStats'result = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerAccumulatedStatsResponse'MethodStats where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerAccumulatedStatsResponse.MethodStats"
  packedMessageDescriptor _
    = "\n\
      \\vMethodStats\DC2!\n\
      \\frpcs_started\CAN\SOH \SOH(\ENQR\vrpcsStarted\DC2b\n\
      \\ACKresult\CAN\STX \ETX(\v2J.grpc.testing.LoadBalancerAccumulatedStatsResponse.MethodStats.ResultEntryR\ACKresult\SUB9\n\
      \\vResultEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\ENQR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        rpcsStarted__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rpcs_started"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"rpcsStarted")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'MethodStats
        result__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "result"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"result")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'MethodStats
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, rpcsStarted__field_descriptor),
           (Data.ProtoLens.Tag 2, result__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerAccumulatedStatsResponse'MethodStats'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerAccumulatedStatsResponse'MethodStats'_unknownFields = y__})
  defMessage
    = LoadBalancerAccumulatedStatsResponse'MethodStats'_constructor
        {_LoadBalancerAccumulatedStatsResponse'MethodStats'rpcsStarted = Data.ProtoLens.fieldDefault,
         _LoadBalancerAccumulatedStatsResponse'MethodStats'result = Data.Map.empty,
         _LoadBalancerAccumulatedStatsResponse'MethodStats'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerAccumulatedStatsResponse'MethodStats
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerAccumulatedStatsResponse'MethodStats
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
                                       "rpcs_started"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"rpcsStarted") y x)
                        18
                          -> do !(entry :: LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                                              (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                                                                                    (Prelude.fromIntegral
                                                                                                                       len)
                                                                                                                    Data.ProtoLens.parseMessage)
                                                                                                              "result"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"result")
                                        (\ !t -> Data.Map.insert key value t) x))
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MethodStats"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"rpcsStarted") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (Data.Monoid.mconcat
                   (Prelude.map
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                    (Lens.Family2.set
                                       (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                       (Data.ProtoLens.defMessage ::
                                          LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry)))))
                      (Data.Map.toList
                         (Lens.Family2.view (Data.ProtoLens.Field.field @"result") _x))))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData LoadBalancerAccumulatedStatsResponse'MethodStats where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerAccumulatedStatsResponse'MethodStats'_unknownFields
                x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerAccumulatedStatsResponse'MethodStats'rpcsStarted x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerAccumulatedStatsResponse'MethodStats'result x__) ()))
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry Data.Int.Int32@
         * 'Proto.Messages_Fields.value' @:: Lens' LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry Data.Int.Int32@ -}
data LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry
  = LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'_constructor {_LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'key :: !Data.Int.Int32,
                                                                               _LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'value :: !Data.Int.Int32,
                                                                               _LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry "key" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'key
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry "value" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'value
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerAccumulatedStatsResponse.MethodStats.ResultEntry"
  packedMessageDescriptor _
    = "\n\
      \\vResultEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\ENQR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'_unknownFields = y__})
  defMessage
    = LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'_constructor
        {_LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'key = Data.ProtoLens.fieldDefault,
         _LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'value = Data.ProtoLens.fieldDefault,
         _LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry
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
          (do loop Data.ProtoLens.defMessage) "ResultEntry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
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
instance Control.DeepSeq.NFData LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'_unknownFields
                x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'key
                   x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerAccumulatedStatsResponse'MethodStats'ResultEntry'value
                      x__)
                   ()))
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry Data.Int.Int32@ -}
data LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry
  = LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'_constructor {_LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'key :: !Data.Text.Text,
                                                                                  _LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'value :: !Data.Int.Int32,
                                                                                  _LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'key
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry "value" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'value
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerAccumulatedStatsResponse.NumRpcsFailedByMethodEntry"
  packedMessageDescriptor _
    = "\n\
      \\SUBNumRpcsFailedByMethodEntry\DC2\DLE\n\
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
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'_unknownFields = y__})
  defMessage
    = LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'_constructor
        {_LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'key = Data.ProtoLens.fieldDefault,
         _LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'value = Data.ProtoLens.fieldDefault,
         _LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry
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
          (do loop Data.ProtoLens.defMessage) "NumRpcsFailedByMethodEntry"
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
instance Control.DeepSeq.NFData LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'_unknownFields
                x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'key
                   x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerAccumulatedStatsResponse'NumRpcsFailedByMethodEntry'value
                      x__)
                   ()))
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry Data.Int.Int32@ -}
data LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry
  = LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'_constructor {_LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'key :: !Data.Text.Text,
                                                                                   _LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'value :: !Data.Int.Int32,
                                                                                   _LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'key
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry "value" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'value
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerAccumulatedStatsResponse.NumRpcsStartedByMethodEntry"
  packedMessageDescriptor _
    = "\n\
      \\ESCNumRpcsStartedByMethodEntry\DC2\DLE\n\
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
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'_unknownFields = y__})
  defMessage
    = LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'_constructor
        {_LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'key = Data.ProtoLens.fieldDefault,
         _LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'value = Data.ProtoLens.fieldDefault,
         _LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry
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
          (do loop Data.ProtoLens.defMessage) "NumRpcsStartedByMethodEntry"
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
instance Control.DeepSeq.NFData LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'_unknownFields
                x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'key
                   x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerAccumulatedStatsResponse'NumRpcsStartedByMethodEntry'value
                      x__)
                   ()))
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry Data.Int.Int32@ -}
data LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry
  = LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'_constructor {_LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'key :: !Data.Text.Text,
                                                                                     _LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'value :: !Data.Int.Int32,
                                                                                     _LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'key
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry "value" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'value
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerAccumulatedStatsResponse.NumRpcsSucceededByMethodEntry"
  packedMessageDescriptor _
    = "\n\
      \\GSNumRpcsSucceededByMethodEntry\DC2\DLE\n\
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
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'_unknownFields = y__})
  defMessage
    = LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'_constructor
        {_LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'key = Data.ProtoLens.fieldDefault,
         _LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'value = Data.ProtoLens.fieldDefault,
         _LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry
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
          (do loop Data.ProtoLens.defMessage) "NumRpcsSucceededByMethodEntry"
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
instance Control.DeepSeq.NFData LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'_unknownFields
                x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'key
                   x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerAccumulatedStatsResponse'NumRpcsSucceededByMethodEntry'value
                      x__)
                   ()))
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry LoadBalancerAccumulatedStatsResponse'MethodStats@
         * 'Proto.Messages_Fields.maybe'value' @:: Lens' LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry (Prelude.Maybe LoadBalancerAccumulatedStatsResponse'MethodStats)@ -}
data LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry
  = LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'_constructor {_LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'key :: !Data.Text.Text,
                                                                           _LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'value :: !(Prelude.Maybe LoadBalancerAccumulatedStatsResponse'MethodStats),
                                                                           _LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'key
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry "value" LoadBalancerAccumulatedStatsResponse'MethodStats where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'value
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry "maybe'value" (Prelude.Maybe LoadBalancerAccumulatedStatsResponse'MethodStats) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'value
           (\ x__ y__
              -> x__
                   {_LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerAccumulatedStatsResponse.StatsPerMethodEntry"
  packedMessageDescriptor _
    = "\n\
      \\DC3StatsPerMethodEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2T\n\
      \\ENQvalue\CAN\STX \SOH(\v2>.grpc.testing.LoadBalancerAccumulatedStatsResponse.MethodStatsR\ENQvalue:\STX8\SOH"
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
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerAccumulatedStatsResponse'MethodStats)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'_unknownFields = y__})
  defMessage
    = LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'_constructor
        {_LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'key = Data.ProtoLens.fieldDefault,
         _LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'value = Prelude.Nothing,
         _LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry
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
          (do loop Data.ProtoLens.defMessage) "StatsPerMethodEntry"
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
instance Control.DeepSeq.NFData LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'_unknownFields
                x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerAccumulatedStatsResponse'StatsPerMethodEntry'value
                      x__)
                   ()))
{- | Fields :

         * 'Proto.Messages_Fields.numRpcs' @:: Lens' LoadBalancerStatsRequest Data.Int.Int32@
         * 'Proto.Messages_Fields.timeoutSec' @:: Lens' LoadBalancerStatsRequest Data.Int.Int32@
         * 'Proto.Messages_Fields.metadataKeys' @:: Lens' LoadBalancerStatsRequest [Data.Text.Text]@
         * 'Proto.Messages_Fields.vec'metadataKeys' @:: Lens' LoadBalancerStatsRequest (Data.Vector.Vector Data.Text.Text)@ -}
data LoadBalancerStatsRequest
  = LoadBalancerStatsRequest'_constructor {_LoadBalancerStatsRequest'numRpcs :: !Data.Int.Int32,
                                           _LoadBalancerStatsRequest'timeoutSec :: !Data.Int.Int32,
                                           _LoadBalancerStatsRequest'metadataKeys :: !(Data.Vector.Vector Data.Text.Text),
                                           _LoadBalancerStatsRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerStatsRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsRequest "numRpcs" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsRequest'numRpcs
           (\ x__ y__ -> x__ {_LoadBalancerStatsRequest'numRpcs = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerStatsRequest "timeoutSec" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsRequest'timeoutSec
           (\ x__ y__ -> x__ {_LoadBalancerStatsRequest'timeoutSec = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerStatsRequest "metadataKeys" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsRequest'metadataKeys
           (\ x__ y__ -> x__ {_LoadBalancerStatsRequest'metadataKeys = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsRequest "vec'metadataKeys" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsRequest'metadataKeys
           (\ x__ y__ -> x__ {_LoadBalancerStatsRequest'metadataKeys = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerStatsRequest where
  messageName _
    = Data.Text.pack "grpc.testing.LoadBalancerStatsRequest"
  packedMessageDescriptor _
    = "\n\
      \\CANLoadBalancerStatsRequest\DC2\EM\n\
      \\bnum_rpcs\CAN\SOH \SOH(\ENQR\anumRpcs\DC2\US\n\
      \\vtimeout_sec\CAN\STX \SOH(\ENQR\n\
      \timeoutSec\DC2#\n\
      \\rmetadata_keys\CAN\ETX \ETX(\tR\fmetadataKeys"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        numRpcs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "num_rpcs"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"numRpcs")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsRequest
        timeoutSec__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "timeout_sec"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"timeoutSec")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsRequest
        metadataKeys__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "metadata_keys"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"metadataKeys")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, numRpcs__field_descriptor),
           (Data.ProtoLens.Tag 2, timeoutSec__field_descriptor),
           (Data.ProtoLens.Tag 3, metadataKeys__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerStatsRequest'_unknownFields
        (\ x__ y__ -> x__ {_LoadBalancerStatsRequest'_unknownFields = y__})
  defMessage
    = LoadBalancerStatsRequest'_constructor
        {_LoadBalancerStatsRequest'numRpcs = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsRequest'timeoutSec = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsRequest'metadataKeys = Data.Vector.Generic.empty,
         _LoadBalancerStatsRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerStatsRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
             -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerStatsRequest
        loop x mutable'metadataKeys
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'metadataKeys <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                  mutable'metadataKeys)
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
                              (Data.ProtoLens.Field.field @"vec'metadataKeys")
                              frozen'metadataKeys x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "num_rpcs"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"numRpcs") y x)
                                  mutable'metadataKeys
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "timeout_sec"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"timeoutSec") y x)
                                  mutable'metadataKeys
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.getText
                                              (Prelude.fromIntegral len))
                                        "metadata_keys"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'metadataKeys y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'metadataKeys
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'metadataKeys <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'metadataKeys)
          "LoadBalancerStatsRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"numRpcs") _x
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
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"timeoutSec") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
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
                                 Data.Text.Encoding.encodeUtf8 _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'metadataKeys") _x))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData LoadBalancerStatsRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerStatsRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerStatsRequest'numRpcs x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerStatsRequest'timeoutSec x__)
                   (Control.DeepSeq.deepseq
                      (_LoadBalancerStatsRequest'metadataKeys x__) ())))
{- | Fields :

         * 'Proto.Messages_Fields.rpcsByPeer' @:: Lens' LoadBalancerStatsResponse (Data.Map.Map Data.Text.Text Data.Int.Int32)@
         * 'Proto.Messages_Fields.numFailures' @:: Lens' LoadBalancerStatsResponse Data.Int.Int32@
         * 'Proto.Messages_Fields.rpcsByMethod' @:: Lens' LoadBalancerStatsResponse (Data.Map.Map Data.Text.Text LoadBalancerStatsResponse'RpcsByPeer)@
         * 'Proto.Messages_Fields.metadatasByPeer' @:: Lens' LoadBalancerStatsResponse (Data.Map.Map Data.Text.Text LoadBalancerStatsResponse'MetadataByPeer)@ -}
data LoadBalancerStatsResponse
  = LoadBalancerStatsResponse'_constructor {_LoadBalancerStatsResponse'rpcsByPeer :: !(Data.Map.Map Data.Text.Text Data.Int.Int32),
                                            _LoadBalancerStatsResponse'numFailures :: !Data.Int.Int32,
                                            _LoadBalancerStatsResponse'rpcsByMethod :: !(Data.Map.Map Data.Text.Text LoadBalancerStatsResponse'RpcsByPeer),
                                            _LoadBalancerStatsResponse'metadatasByPeer :: !(Data.Map.Map Data.Text.Text LoadBalancerStatsResponse'MetadataByPeer),
                                            _LoadBalancerStatsResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerStatsResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse "rpcsByPeer" (Data.Map.Map Data.Text.Text Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'rpcsByPeer
           (\ x__ y__ -> x__ {_LoadBalancerStatsResponse'rpcsByPeer = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse "numFailures" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'numFailures
           (\ x__ y__ -> x__ {_LoadBalancerStatsResponse'numFailures = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse "rpcsByMethod" (Data.Map.Map Data.Text.Text LoadBalancerStatsResponse'RpcsByPeer) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'rpcsByMethod
           (\ x__ y__ -> x__ {_LoadBalancerStatsResponse'rpcsByMethod = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse "metadatasByPeer" (Data.Map.Map Data.Text.Text LoadBalancerStatsResponse'MetadataByPeer) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'metadatasByPeer
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'metadatasByPeer = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerStatsResponse where
  messageName _
    = Data.Text.pack "grpc.testing.LoadBalancerStatsResponse"
  packedMessageDescriptor _
    = "\n\
      \\EMLoadBalancerStatsResponse\DC2Y\n\
      \\frpcs_by_peer\CAN\SOH \ETX(\v27.grpc.testing.LoadBalancerStatsResponse.RpcsByPeerEntryR\n\
      \rpcsByPeer\DC2!\n\
      \\fnum_failures\CAN\STX \SOH(\ENQR\vnumFailures\DC2_\n\
      \\SOrpcs_by_method\CAN\ETX \ETX(\v29.grpc.testing.LoadBalancerStatsResponse.RpcsByMethodEntryR\frpcsByMethod\DC2h\n\
      \\DC1metadatas_by_peer\CAN\EOT \ETX(\v2<.grpc.testing.LoadBalancerStatsResponse.MetadatasByPeerEntryR\SImetadatasByPeer\SUB\129\SOH\n\
      \\rMetadataEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\tR\ENQvalue\DC2H\n\
      \\EOTtype\CAN\ETX \SOH(\SO24.grpc.testing.LoadBalancerStatsResponse.MetadataTypeR\EOTtype\SUB`\n\
      \\vRpcMetadata\DC2Q\n\
      \\bmetadata\CAN\SOH \ETX(\v25.grpc.testing.LoadBalancerStatsResponse.MetadataEntryR\bmetadata\SUBh\n\
      \\SOMetadataByPeer\DC2V\n\
      \\frpc_metadata\CAN\SOH \ETX(\v23.grpc.testing.LoadBalancerStatsResponse.RpcMetadataR\vrpcMetadata\SUB\177\SOH\n\
      \\n\
      \RpcsByPeer\DC2d\n\
      \\frpcs_by_peer\CAN\SOH \ETX(\v2B.grpc.testing.LoadBalancerStatsResponse.RpcsByPeer.RpcsByPeerEntryR\n\
      \rpcsByPeer\SUB=\n\
      \\SIRpcsByPeerEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUB=\n\
      \\SIRpcsByPeerEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUBs\n\
      \\DC1RpcsByMethodEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2H\n\
      \\ENQvalue\CAN\STX \SOH(\v22.grpc.testing.LoadBalancerStatsResponse.RpcsByPeerR\ENQvalue:\STX8\SOH\SUBz\n\
      \\DC4MetadatasByPeerEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2L\n\
      \\ENQvalue\CAN\STX \SOH(\v26.grpc.testing.LoadBalancerStatsResponse.MetadataByPeerR\ENQvalue:\STX8\SOH\"6\n\
      \\fMetadataType\DC2\v\n\
      \\aUNKNOWN\DLE\NUL\DC2\v\n\
      \\aINITIAL\DLE\SOH\DC2\f\n\
      \\bTRAILING\DLE\STX"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        rpcsByPeer__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rpcs_by_peer"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerStatsResponse'RpcsByPeerEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"rpcsByPeer")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse
        numFailures__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "num_failures"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"numFailures")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse
        rpcsByMethod__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rpcs_by_method"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerStatsResponse'RpcsByMethodEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"rpcsByMethod")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse
        metadatasByPeer__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "metadatas_by_peer"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerStatsResponse'MetadatasByPeerEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"metadatasByPeer")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, rpcsByPeer__field_descriptor),
           (Data.ProtoLens.Tag 2, numFailures__field_descriptor),
           (Data.ProtoLens.Tag 3, rpcsByMethod__field_descriptor),
           (Data.ProtoLens.Tag 4, metadatasByPeer__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerStatsResponse'_unknownFields
        (\ x__ y__
           -> x__ {_LoadBalancerStatsResponse'_unknownFields = y__})
  defMessage
    = LoadBalancerStatsResponse'_constructor
        {_LoadBalancerStatsResponse'rpcsByPeer = Data.Map.empty,
         _LoadBalancerStatsResponse'numFailures = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsResponse'rpcsByMethod = Data.Map.empty,
         _LoadBalancerStatsResponse'metadatasByPeer = Data.Map.empty,
         _LoadBalancerStatsResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerStatsResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerStatsResponse
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
                          -> do !(entry :: LoadBalancerStatsResponse'RpcsByPeerEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                           (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                               Data.ProtoLens.Encoding.Bytes.isolate
                                                                                                 (Prelude.fromIntegral
                                                                                                    len)
                                                                                                 Data.ProtoLens.parseMessage)
                                                                                           "rpcs_by_peer"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"rpcsByPeer")
                                        (\ !t -> Data.Map.insert key value t) x))
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "num_failures"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"numFailures") y x)
                        26
                          -> do !(entry :: LoadBalancerStatsResponse'RpcsByMethodEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                             (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                                 Data.ProtoLens.Encoding.Bytes.isolate
                                                                                                   (Prelude.fromIntegral
                                                                                                      len)
                                                                                                   Data.ProtoLens.parseMessage)
                                                                                             "rpcs_by_method"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"rpcsByMethod")
                                        (\ !t -> Data.Map.insert key value t) x))
                        34
                          -> do !(entry :: LoadBalancerStatsResponse'MetadatasByPeerEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                                (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                                    Data.ProtoLens.Encoding.Bytes.isolate
                                                                                                      (Prelude.fromIntegral
                                                                                                         len)
                                                                                                      Data.ProtoLens.parseMessage)
                                                                                                "metadatas_by_peer"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"metadatasByPeer")
                                        (\ !t -> Data.Map.insert key value t) x))
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "LoadBalancerStatsResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.Monoid.mconcat
                (Prelude.map
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                    (Data.ProtoLens.defMessage ::
                                       LoadBalancerStatsResponse'RpcsByPeerEntry)))))
                   (Data.Map.toList
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"rpcsByPeer") _x))))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"numFailures") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (Data.Monoid.mconcat
                      (Prelude.map
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage
                                    (Lens.Family2.set
                                       (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                       (Lens.Family2.set
                                          (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                          (Data.ProtoLens.defMessage ::
                                             LoadBalancerStatsResponse'RpcsByMethodEntry)))))
                         (Data.Map.toList
                            (Lens.Family2.view
                               (Data.ProtoLens.Field.field @"rpcsByMethod") _x))))
                   ((Data.Monoid.<>)
                      (Data.Monoid.mconcat
                         (Prelude.map
                            (\ _v
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                    ((Prelude..)
                                       (\ bs
                                          -> (Data.Monoid.<>)
                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                  (Prelude.fromIntegral
                                                     (Data.ByteString.length bs)))
                                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                       Data.ProtoLens.encodeMessage
                                       (Lens.Family2.set
                                          (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                             (Data.ProtoLens.defMessage ::
                                                LoadBalancerStatsResponse'MetadatasByPeerEntry)))))
                            (Data.Map.toList
                               (Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"metadatasByPeer") _x))))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData LoadBalancerStatsResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerStatsResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerStatsResponse'rpcsByPeer x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerStatsResponse'numFailures x__)
                   (Control.DeepSeq.deepseq
                      (_LoadBalancerStatsResponse'rpcsByMethod x__)
                      (Control.DeepSeq.deepseq
                         (_LoadBalancerStatsResponse'metadatasByPeer x__) ()))))
{- | Fields :

         * 'Proto.Messages_Fields.rpcMetadata' @:: Lens' LoadBalancerStatsResponse'MetadataByPeer [LoadBalancerStatsResponse'RpcMetadata]@
         * 'Proto.Messages_Fields.vec'rpcMetadata' @:: Lens' LoadBalancerStatsResponse'MetadataByPeer (Data.Vector.Vector LoadBalancerStatsResponse'RpcMetadata)@ -}
data LoadBalancerStatsResponse'MetadataByPeer
  = LoadBalancerStatsResponse'MetadataByPeer'_constructor {_LoadBalancerStatsResponse'MetadataByPeer'rpcMetadata :: !(Data.Vector.Vector LoadBalancerStatsResponse'RpcMetadata),
                                                           _LoadBalancerStatsResponse'MetadataByPeer'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerStatsResponse'MetadataByPeer where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'MetadataByPeer "rpcMetadata" [LoadBalancerStatsResponse'RpcMetadata] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'MetadataByPeer'rpcMetadata
           (\ x__ y__
              -> x__
                   {_LoadBalancerStatsResponse'MetadataByPeer'rpcMetadata = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'MetadataByPeer "vec'rpcMetadata" (Data.Vector.Vector LoadBalancerStatsResponse'RpcMetadata) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'MetadataByPeer'rpcMetadata
           (\ x__ y__
              -> x__
                   {_LoadBalancerStatsResponse'MetadataByPeer'rpcMetadata = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerStatsResponse'MetadataByPeer where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerStatsResponse.MetadataByPeer"
  packedMessageDescriptor _
    = "\n\
      \\SOMetadataByPeer\DC2V\n\
      \\frpc_metadata\CAN\SOH \ETX(\v23.grpc.testing.LoadBalancerStatsResponse.RpcMetadataR\vrpcMetadata"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        rpcMetadata__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rpc_metadata"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerStatsResponse'RpcMetadata)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"rpcMetadata")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'MetadataByPeer
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, rpcMetadata__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerStatsResponse'MetadataByPeer'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerStatsResponse'MetadataByPeer'_unknownFields = y__})
  defMessage
    = LoadBalancerStatsResponse'MetadataByPeer'_constructor
        {_LoadBalancerStatsResponse'MetadataByPeer'rpcMetadata = Data.Vector.Generic.empty,
         _LoadBalancerStatsResponse'MetadataByPeer'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerStatsResponse'MetadataByPeer
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld LoadBalancerStatsResponse'RpcMetadata
             -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerStatsResponse'MetadataByPeer
        loop x mutable'rpcMetadata
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'rpcMetadata <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                 mutable'rpcMetadata)
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
                              (Data.ProtoLens.Field.field @"vec'rpcMetadata") frozen'rpcMetadata
                              x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "rpc_metadata"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'rpcMetadata y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'rpcMetadata
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'rpcMetadata <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'rpcMetadata)
          "MetadataByPeer"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view
                   (Data.ProtoLens.Field.field @"vec'rpcMetadata") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData LoadBalancerStatsResponse'MetadataByPeer where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerStatsResponse'MetadataByPeer'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerStatsResponse'MetadataByPeer'rpcMetadata x__) ())
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' LoadBalancerStatsResponse'MetadataEntry Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' LoadBalancerStatsResponse'MetadataEntry Data.Text.Text@
         * 'Proto.Messages_Fields.type'' @:: Lens' LoadBalancerStatsResponse'MetadataEntry LoadBalancerStatsResponse'MetadataType@ -}
data LoadBalancerStatsResponse'MetadataEntry
  = LoadBalancerStatsResponse'MetadataEntry'_constructor {_LoadBalancerStatsResponse'MetadataEntry'key :: !Data.Text.Text,
                                                          _LoadBalancerStatsResponse'MetadataEntry'value :: !Data.Text.Text,
                                                          _LoadBalancerStatsResponse'MetadataEntry'type' :: !LoadBalancerStatsResponse'MetadataType,
                                                          _LoadBalancerStatsResponse'MetadataEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerStatsResponse'MetadataEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'MetadataEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'MetadataEntry'key
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'MetadataEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'MetadataEntry "value" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'MetadataEntry'value
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'MetadataEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'MetadataEntry "type'" LoadBalancerStatsResponse'MetadataType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'MetadataEntry'type'
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'MetadataEntry'type' = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerStatsResponse'MetadataEntry where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerStatsResponse.MetadataEntry"
  packedMessageDescriptor _
    = "\n\
      \\rMetadataEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\tR\ENQvalue\DC2H\n\
      \\EOTtype\CAN\ETX \SOH(\SO24.grpc.testing.LoadBalancerStatsResponse.MetadataTypeR\EOTtype"
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
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'MetadataEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'MetadataEntry
        type'__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerStatsResponse'MetadataType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"type'")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'MetadataEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor),
           (Data.ProtoLens.Tag 3, type'__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerStatsResponse'MetadataEntry'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerStatsResponse'MetadataEntry'_unknownFields = y__})
  defMessage
    = LoadBalancerStatsResponse'MetadataEntry'_constructor
        {_LoadBalancerStatsResponse'MetadataEntry'key = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsResponse'MetadataEntry'value = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsResponse'MetadataEntry'type' = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsResponse'MetadataEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerStatsResponse'MetadataEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerStatsResponse'MetadataEntry
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
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "type"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"type'") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MetadataEntry"
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
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"type'") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                               Prelude.fromEnum _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData LoadBalancerStatsResponse'MetadataEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerStatsResponse'MetadataEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerStatsResponse'MetadataEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerStatsResponse'MetadataEntry'value x__)
                   (Control.DeepSeq.deepseq
                      (_LoadBalancerStatsResponse'MetadataEntry'type' x__) ())))
newtype LoadBalancerStatsResponse'MetadataType'UnrecognizedValue
  = LoadBalancerStatsResponse'MetadataType'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data LoadBalancerStatsResponse'MetadataType
  = LoadBalancerStatsResponse'UNKNOWN |
    LoadBalancerStatsResponse'INITIAL |
    LoadBalancerStatsResponse'TRAILING |
    LoadBalancerStatsResponse'MetadataType'Unrecognized !LoadBalancerStatsResponse'MetadataType'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum LoadBalancerStatsResponse'MetadataType where
  maybeToEnum 0 = Prelude.Just LoadBalancerStatsResponse'UNKNOWN
  maybeToEnum 1 = Prelude.Just LoadBalancerStatsResponse'INITIAL
  maybeToEnum 2 = Prelude.Just LoadBalancerStatsResponse'TRAILING
  maybeToEnum k
    = Prelude.Just
        (LoadBalancerStatsResponse'MetadataType'Unrecognized
           (LoadBalancerStatsResponse'MetadataType'UnrecognizedValue
              (Prelude.fromIntegral k)))
  showEnum LoadBalancerStatsResponse'UNKNOWN = "UNKNOWN"
  showEnum LoadBalancerStatsResponse'INITIAL = "INITIAL"
  showEnum LoadBalancerStatsResponse'TRAILING = "TRAILING"
  showEnum
    (LoadBalancerStatsResponse'MetadataType'Unrecognized (LoadBalancerStatsResponse'MetadataType'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UNKNOWN"
    = Prelude.Just LoadBalancerStatsResponse'UNKNOWN
    | (Prelude.==) k "INITIAL"
    = Prelude.Just LoadBalancerStatsResponse'INITIAL
    | (Prelude.==) k "TRAILING"
    = Prelude.Just LoadBalancerStatsResponse'TRAILING
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded LoadBalancerStatsResponse'MetadataType where
  minBound = LoadBalancerStatsResponse'UNKNOWN
  maxBound = LoadBalancerStatsResponse'TRAILING
instance Prelude.Enum LoadBalancerStatsResponse'MetadataType where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum MetadataType: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum LoadBalancerStatsResponse'UNKNOWN = 0
  fromEnum LoadBalancerStatsResponse'INITIAL = 1
  fromEnum LoadBalancerStatsResponse'TRAILING = 2
  fromEnum
    (LoadBalancerStatsResponse'MetadataType'Unrecognized (LoadBalancerStatsResponse'MetadataType'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ LoadBalancerStatsResponse'TRAILING
    = Prelude.error
        "LoadBalancerStatsResponse'MetadataType.succ: bad argument LoadBalancerStatsResponse'TRAILING. This value would be out of bounds."
  succ LoadBalancerStatsResponse'UNKNOWN
    = LoadBalancerStatsResponse'INITIAL
  succ LoadBalancerStatsResponse'INITIAL
    = LoadBalancerStatsResponse'TRAILING
  succ (LoadBalancerStatsResponse'MetadataType'Unrecognized _)
    = Prelude.error
        "LoadBalancerStatsResponse'MetadataType.succ: bad argument: unrecognized value"
  pred LoadBalancerStatsResponse'UNKNOWN
    = Prelude.error
        "LoadBalancerStatsResponse'MetadataType.pred: bad argument LoadBalancerStatsResponse'UNKNOWN. This value would be out of bounds."
  pred LoadBalancerStatsResponse'INITIAL
    = LoadBalancerStatsResponse'UNKNOWN
  pred LoadBalancerStatsResponse'TRAILING
    = LoadBalancerStatsResponse'INITIAL
  pred (LoadBalancerStatsResponse'MetadataType'Unrecognized _)
    = Prelude.error
        "LoadBalancerStatsResponse'MetadataType.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault LoadBalancerStatsResponse'MetadataType where
  fieldDefault = LoadBalancerStatsResponse'UNKNOWN
instance Control.DeepSeq.NFData LoadBalancerStatsResponse'MetadataType where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' LoadBalancerStatsResponse'MetadatasByPeerEntry Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' LoadBalancerStatsResponse'MetadatasByPeerEntry LoadBalancerStatsResponse'MetadataByPeer@
         * 'Proto.Messages_Fields.maybe'value' @:: Lens' LoadBalancerStatsResponse'MetadatasByPeerEntry (Prelude.Maybe LoadBalancerStatsResponse'MetadataByPeer)@ -}
data LoadBalancerStatsResponse'MetadatasByPeerEntry
  = LoadBalancerStatsResponse'MetadatasByPeerEntry'_constructor {_LoadBalancerStatsResponse'MetadatasByPeerEntry'key :: !Data.Text.Text,
                                                                 _LoadBalancerStatsResponse'MetadatasByPeerEntry'value :: !(Prelude.Maybe LoadBalancerStatsResponse'MetadataByPeer),
                                                                 _LoadBalancerStatsResponse'MetadatasByPeerEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerStatsResponse'MetadatasByPeerEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'MetadatasByPeerEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'MetadatasByPeerEntry'key
           (\ x__ y__
              -> x__
                   {_LoadBalancerStatsResponse'MetadatasByPeerEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'MetadatasByPeerEntry "value" LoadBalancerStatsResponse'MetadataByPeer where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'MetadatasByPeerEntry'value
           (\ x__ y__
              -> x__
                   {_LoadBalancerStatsResponse'MetadatasByPeerEntry'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'MetadatasByPeerEntry "maybe'value" (Prelude.Maybe LoadBalancerStatsResponse'MetadataByPeer) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'MetadatasByPeerEntry'value
           (\ x__ y__
              -> x__
                   {_LoadBalancerStatsResponse'MetadatasByPeerEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerStatsResponse'MetadatasByPeerEntry where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerStatsResponse.MetadatasByPeerEntry"
  packedMessageDescriptor _
    = "\n\
      \\DC4MetadatasByPeerEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2L\n\
      \\ENQvalue\CAN\STX \SOH(\v26.grpc.testing.LoadBalancerStatsResponse.MetadataByPeerR\ENQvalue:\STX8\SOH"
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
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'MetadatasByPeerEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerStatsResponse'MetadataByPeer)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'MetadatasByPeerEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerStatsResponse'MetadatasByPeerEntry'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerStatsResponse'MetadatasByPeerEntry'_unknownFields = y__})
  defMessage
    = LoadBalancerStatsResponse'MetadatasByPeerEntry'_constructor
        {_LoadBalancerStatsResponse'MetadatasByPeerEntry'key = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsResponse'MetadatasByPeerEntry'value = Prelude.Nothing,
         _LoadBalancerStatsResponse'MetadatasByPeerEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerStatsResponse'MetadatasByPeerEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerStatsResponse'MetadatasByPeerEntry
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
          (do loop Data.ProtoLens.defMessage) "MetadatasByPeerEntry"
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
instance Control.DeepSeq.NFData LoadBalancerStatsResponse'MetadatasByPeerEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerStatsResponse'MetadatasByPeerEntry'_unknownFields
                x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerStatsResponse'MetadatasByPeerEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerStatsResponse'MetadatasByPeerEntry'value x__) ()))
{- | Fields :

         * 'Proto.Messages_Fields.metadata' @:: Lens' LoadBalancerStatsResponse'RpcMetadata [LoadBalancerStatsResponse'MetadataEntry]@
         * 'Proto.Messages_Fields.vec'metadata' @:: Lens' LoadBalancerStatsResponse'RpcMetadata (Data.Vector.Vector LoadBalancerStatsResponse'MetadataEntry)@ -}
data LoadBalancerStatsResponse'RpcMetadata
  = LoadBalancerStatsResponse'RpcMetadata'_constructor {_LoadBalancerStatsResponse'RpcMetadata'metadata :: !(Data.Vector.Vector LoadBalancerStatsResponse'MetadataEntry),
                                                        _LoadBalancerStatsResponse'RpcMetadata'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerStatsResponse'RpcMetadata where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'RpcMetadata "metadata" [LoadBalancerStatsResponse'MetadataEntry] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'RpcMetadata'metadata
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'RpcMetadata'metadata = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'RpcMetadata "vec'metadata" (Data.Vector.Vector LoadBalancerStatsResponse'MetadataEntry) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'RpcMetadata'metadata
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'RpcMetadata'metadata = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerStatsResponse'RpcMetadata where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerStatsResponse.RpcMetadata"
  packedMessageDescriptor _
    = "\n\
      \\vRpcMetadata\DC2Q\n\
      \\bmetadata\CAN\SOH \ETX(\v25.grpc.testing.LoadBalancerStatsResponse.MetadataEntryR\bmetadata"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        metadata__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "metadata"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerStatsResponse'MetadataEntry)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"metadata")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'RpcMetadata
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, metadata__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerStatsResponse'RpcMetadata'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerStatsResponse'RpcMetadata'_unknownFields = y__})
  defMessage
    = LoadBalancerStatsResponse'RpcMetadata'_constructor
        {_LoadBalancerStatsResponse'RpcMetadata'metadata = Data.Vector.Generic.empty,
         _LoadBalancerStatsResponse'RpcMetadata'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerStatsResponse'RpcMetadata
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld LoadBalancerStatsResponse'MetadataEntry
             -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerStatsResponse'RpcMetadata
        loop x mutable'metadata
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'metadata <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                              mutable'metadata)
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
                              (Data.ProtoLens.Field.field @"vec'metadata") frozen'metadata x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "metadata"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'metadata y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'metadata
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'metadata <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'metadata)
          "RpcMetadata"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view
                   (Data.ProtoLens.Field.field @"vec'metadata") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData LoadBalancerStatsResponse'RpcMetadata where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerStatsResponse'RpcMetadata'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerStatsResponse'RpcMetadata'metadata x__) ())
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' LoadBalancerStatsResponse'RpcsByMethodEntry Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' LoadBalancerStatsResponse'RpcsByMethodEntry LoadBalancerStatsResponse'RpcsByPeer@
         * 'Proto.Messages_Fields.maybe'value' @:: Lens' LoadBalancerStatsResponse'RpcsByMethodEntry (Prelude.Maybe LoadBalancerStatsResponse'RpcsByPeer)@ -}
data LoadBalancerStatsResponse'RpcsByMethodEntry
  = LoadBalancerStatsResponse'RpcsByMethodEntry'_constructor {_LoadBalancerStatsResponse'RpcsByMethodEntry'key :: !Data.Text.Text,
                                                              _LoadBalancerStatsResponse'RpcsByMethodEntry'value :: !(Prelude.Maybe LoadBalancerStatsResponse'RpcsByPeer),
                                                              _LoadBalancerStatsResponse'RpcsByMethodEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerStatsResponse'RpcsByMethodEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'RpcsByMethodEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'RpcsByMethodEntry'key
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'RpcsByMethodEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'RpcsByMethodEntry "value" LoadBalancerStatsResponse'RpcsByPeer where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'RpcsByMethodEntry'value
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'RpcsByMethodEntry'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'RpcsByMethodEntry "maybe'value" (Prelude.Maybe LoadBalancerStatsResponse'RpcsByPeer) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'RpcsByMethodEntry'value
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'RpcsByMethodEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerStatsResponse'RpcsByMethodEntry where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerStatsResponse.RpcsByMethodEntry"
  packedMessageDescriptor _
    = "\n\
      \\DC1RpcsByMethodEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2H\n\
      \\ENQvalue\CAN\STX \SOH(\v22.grpc.testing.LoadBalancerStatsResponse.RpcsByPeerR\ENQvalue:\STX8\SOH"
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
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'RpcsByMethodEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerStatsResponse'RpcsByPeer)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'RpcsByMethodEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerStatsResponse'RpcsByMethodEntry'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerStatsResponse'RpcsByMethodEntry'_unknownFields = y__})
  defMessage
    = LoadBalancerStatsResponse'RpcsByMethodEntry'_constructor
        {_LoadBalancerStatsResponse'RpcsByMethodEntry'key = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsResponse'RpcsByMethodEntry'value = Prelude.Nothing,
         _LoadBalancerStatsResponse'RpcsByMethodEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerStatsResponse'RpcsByMethodEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerStatsResponse'RpcsByMethodEntry
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
          (do loop Data.ProtoLens.defMessage) "RpcsByMethodEntry"
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
instance Control.DeepSeq.NFData LoadBalancerStatsResponse'RpcsByMethodEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerStatsResponse'RpcsByMethodEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerStatsResponse'RpcsByMethodEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerStatsResponse'RpcsByMethodEntry'value x__) ()))
{- | Fields :

         * 'Proto.Messages_Fields.rpcsByPeer' @:: Lens' LoadBalancerStatsResponse'RpcsByPeer (Data.Map.Map Data.Text.Text Data.Int.Int32)@ -}
data LoadBalancerStatsResponse'RpcsByPeer
  = LoadBalancerStatsResponse'RpcsByPeer'_constructor {_LoadBalancerStatsResponse'RpcsByPeer'rpcsByPeer :: !(Data.Map.Map Data.Text.Text Data.Int.Int32),
                                                       _LoadBalancerStatsResponse'RpcsByPeer'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerStatsResponse'RpcsByPeer where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'RpcsByPeer "rpcsByPeer" (Data.Map.Map Data.Text.Text Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'RpcsByPeer'rpcsByPeer
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'RpcsByPeer'rpcsByPeer = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerStatsResponse'RpcsByPeer where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerStatsResponse.RpcsByPeer"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \RpcsByPeer\DC2d\n\
      \\frpcs_by_peer\CAN\SOH \ETX(\v2B.grpc.testing.LoadBalancerStatsResponse.RpcsByPeer.RpcsByPeerEntryR\n\
      \rpcsByPeer\SUB=\n\
      \\SIRpcsByPeerEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        rpcsByPeer__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rpcs_by_peer"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"rpcsByPeer")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'RpcsByPeer
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, rpcsByPeer__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerStatsResponse'RpcsByPeer'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerStatsResponse'RpcsByPeer'_unknownFields = y__})
  defMessage
    = LoadBalancerStatsResponse'RpcsByPeer'_constructor
        {_LoadBalancerStatsResponse'RpcsByPeer'rpcsByPeer = Data.Map.empty,
         _LoadBalancerStatsResponse'RpcsByPeer'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerStatsResponse'RpcsByPeer
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerStatsResponse'RpcsByPeer
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
                          -> do !(entry :: LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                                      (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                                          Data.ProtoLens.Encoding.Bytes.isolate
                                                                                                            (Prelude.fromIntegral
                                                                                                               len)
                                                                                                            Data.ProtoLens.parseMessage)
                                                                                                      "rpcs_by_peer"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"rpcsByPeer")
                                        (\ !t -> Data.Map.insert key value t) x))
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "RpcsByPeer"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.Monoid.mconcat
                (Prelude.map
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                    (Data.ProtoLens.defMessage ::
                                       LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry)))))
                   (Data.Map.toList
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"rpcsByPeer") _x))))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData LoadBalancerStatsResponse'RpcsByPeer where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerStatsResponse'RpcsByPeer'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerStatsResponse'RpcsByPeer'rpcsByPeer x__) ())
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry Data.Int.Int32@ -}
data LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry
  = LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'_constructor {_LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'key :: !Data.Text.Text,
                                                                       _LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'value :: !Data.Int.Int32,
                                                                       _LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'key
           (\ x__ y__
              -> x__
                   {_LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry "value" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'value
           (\ x__ y__
              -> x__
                   {_LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerStatsResponse.RpcsByPeer.RpcsByPeerEntry"
  packedMessageDescriptor _
    = "\n\
      \\SIRpcsByPeerEntry\DC2\DLE\n\
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
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'_unknownFields = y__})
  defMessage
    = LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'_constructor
        {_LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'key = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'value = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry
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
          (do loop Data.ProtoLens.defMessage) "RpcsByPeerEntry"
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
instance Control.DeepSeq.NFData LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'_unknownFields
                x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerStatsResponse'RpcsByPeer'RpcsByPeerEntry'value x__)
                   ()))
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' LoadBalancerStatsResponse'RpcsByPeerEntry Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' LoadBalancerStatsResponse'RpcsByPeerEntry Data.Int.Int32@ -}
data LoadBalancerStatsResponse'RpcsByPeerEntry
  = LoadBalancerStatsResponse'RpcsByPeerEntry'_constructor {_LoadBalancerStatsResponse'RpcsByPeerEntry'key :: !Data.Text.Text,
                                                            _LoadBalancerStatsResponse'RpcsByPeerEntry'value :: !Data.Int.Int32,
                                                            _LoadBalancerStatsResponse'RpcsByPeerEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LoadBalancerStatsResponse'RpcsByPeerEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'RpcsByPeerEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'RpcsByPeerEntry'key
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'RpcsByPeerEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LoadBalancerStatsResponse'RpcsByPeerEntry "value" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LoadBalancerStatsResponse'RpcsByPeerEntry'value
           (\ x__ y__
              -> x__ {_LoadBalancerStatsResponse'RpcsByPeerEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message LoadBalancerStatsResponse'RpcsByPeerEntry where
  messageName _
    = Data.Text.pack
        "grpc.testing.LoadBalancerStatsResponse.RpcsByPeerEntry"
  packedMessageDescriptor _
    = "\n\
      \\SIRpcsByPeerEntry\DC2\DLE\n\
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
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'RpcsByPeerEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor LoadBalancerStatsResponse'RpcsByPeerEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LoadBalancerStatsResponse'RpcsByPeerEntry'_unknownFields
        (\ x__ y__
           -> x__
                {_LoadBalancerStatsResponse'RpcsByPeerEntry'_unknownFields = y__})
  defMessage
    = LoadBalancerStatsResponse'RpcsByPeerEntry'_constructor
        {_LoadBalancerStatsResponse'RpcsByPeerEntry'key = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsResponse'RpcsByPeerEntry'value = Data.ProtoLens.fieldDefault,
         _LoadBalancerStatsResponse'RpcsByPeerEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LoadBalancerStatsResponse'RpcsByPeerEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser LoadBalancerStatsResponse'RpcsByPeerEntry
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
          (do loop Data.ProtoLens.defMessage) "RpcsByPeerEntry"
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
instance Control.DeepSeq.NFData LoadBalancerStatsResponse'RpcsByPeerEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LoadBalancerStatsResponse'RpcsByPeerEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LoadBalancerStatsResponse'RpcsByPeerEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_LoadBalancerStatsResponse'RpcsByPeerEntry'value x__) ()))
{- | Fields :

         * 'Proto.Messages_Fields.rss' @:: Lens' MemorySize Data.Int.Int64@ -}
data MemorySize
  = MemorySize'_constructor {_MemorySize'rss :: !Data.Int.Int64,
                             _MemorySize'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MemorySize where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField MemorySize "rss" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MemorySize'rss (\ x__ y__ -> x__ {_MemorySize'rss = y__}))
        Prelude.id
instance Data.ProtoLens.Message MemorySize where
  messageName _ = Data.Text.pack "grpc.testing.MemorySize"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \MemorySize\DC2\DLE\n\
      \\ETXrss\CAN\SOH \SOH(\ETXR\ETXrss"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        rss__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rss"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"rss")) ::
              Data.ProtoLens.FieldDescriptor MemorySize
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, rss__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _MemorySize'_unknownFields
        (\ x__ y__ -> x__ {_MemorySize'_unknownFields = y__})
  defMessage
    = MemorySize'_constructor
        {_MemorySize'rss = Data.ProtoLens.fieldDefault,
         _MemorySize'_unknownFields = []}
  parseMessage
    = let
        loop ::
          MemorySize -> Data.ProtoLens.Encoding.Bytes.Parser MemorySize
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
                                       "rss"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"rss") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MemorySize"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"rss") _x
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
instance Control.DeepSeq.NFData MemorySize where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_MemorySize'_unknownFields x__)
             (Control.DeepSeq.deepseq (_MemorySize'rss x__) ())
{- | Fields :

         * 'Proto.Messages_Fields.type'' @:: Lens' Payload PayloadType@
         * 'Proto.Messages_Fields.body' @:: Lens' Payload Data.ByteString.ByteString@ -}
data Payload
  = Payload'_constructor {_Payload'type' :: !PayloadType,
                          _Payload'body :: !Data.ByteString.ByteString,
                          _Payload'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Payload where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Payload "type'" PayloadType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Payload'type' (\ x__ y__ -> x__ {_Payload'type' = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Payload "body" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Payload'body (\ x__ y__ -> x__ {_Payload'body = y__}))
        Prelude.id
instance Data.ProtoLens.Message Payload where
  messageName _ = Data.Text.pack "grpc.testing.Payload"
  packedMessageDescriptor _
    = "\n\
      \\aPayload\DC2-\n\
      \\EOTtype\CAN\SOH \SOH(\SO2\EM.grpc.testing.PayloadTypeR\EOTtype\DC2\DC2\n\
      \\EOTbody\CAN\STX \SOH(\fR\EOTbody"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        type'__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor PayloadType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"type'")) ::
              Data.ProtoLens.FieldDescriptor Payload
        body__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "body"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"body")) ::
              Data.ProtoLens.FieldDescriptor Payload
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, type'__field_descriptor),
           (Data.ProtoLens.Tag 2, body__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Payload'_unknownFields
        (\ x__ y__ -> x__ {_Payload'_unknownFields = y__})
  defMessage
    = Payload'_constructor
        {_Payload'type' = Data.ProtoLens.fieldDefault,
         _Payload'body = Data.ProtoLens.fieldDefault,
         _Payload'_unknownFields = []}
  parseMessage
    = let
        loop :: Payload -> Data.ProtoLens.Encoding.Bytes.Parser Payload
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
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "type"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"type'") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "body"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"body") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Payload"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"type'") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                         Prelude.fromEnum _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"body") _x
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
instance Control.DeepSeq.NFData Payload where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Payload'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Payload'type' x__)
                (Control.DeepSeq.deepseq (_Payload'body x__) ()))
newtype PayloadType'UnrecognizedValue
  = PayloadType'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data PayloadType
  = COMPRESSABLE |
    PayloadType'Unrecognized !PayloadType'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum PayloadType where
  maybeToEnum 0 = Prelude.Just COMPRESSABLE
  maybeToEnum k
    = Prelude.Just
        (PayloadType'Unrecognized
           (PayloadType'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum COMPRESSABLE = "COMPRESSABLE"
  showEnum
    (PayloadType'Unrecognized (PayloadType'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "COMPRESSABLE" = Prelude.Just COMPRESSABLE
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded PayloadType where
  minBound = COMPRESSABLE
  maxBound = COMPRESSABLE
instance Prelude.Enum PayloadType where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum PayloadType: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum COMPRESSABLE = 0
  fromEnum
    (PayloadType'Unrecognized (PayloadType'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ COMPRESSABLE
    = Prelude.error
        "PayloadType.succ: bad argument COMPRESSABLE. This value would be out of bounds."
  succ (PayloadType'Unrecognized _)
    = Prelude.error
        "PayloadType.succ: bad argument: unrecognized value"
  pred COMPRESSABLE
    = Prelude.error
        "PayloadType.pred: bad argument COMPRESSABLE. This value would be out of bounds."
  pred (PayloadType'Unrecognized _)
    = Prelude.error
        "PayloadType.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault PayloadType where
  fieldDefault = COMPRESSABLE
instance Control.DeepSeq.NFData PayloadType where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :

         * 'Proto.Messages_Fields.passed' @:: Lens' ReconnectInfo Prelude.Bool@
         * 'Proto.Messages_Fields.backoffMs' @:: Lens' ReconnectInfo [Data.Int.Int32]@
         * 'Proto.Messages_Fields.vec'backoffMs' @:: Lens' ReconnectInfo (Data.Vector.Unboxed.Vector Data.Int.Int32)@ -}
data ReconnectInfo
  = ReconnectInfo'_constructor {_ReconnectInfo'passed :: !Prelude.Bool,
                                _ReconnectInfo'backoffMs :: !(Data.Vector.Unboxed.Vector Data.Int.Int32),
                                _ReconnectInfo'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReconnectInfo where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReconnectInfo "passed" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReconnectInfo'passed
           (\ x__ y__ -> x__ {_ReconnectInfo'passed = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReconnectInfo "backoffMs" [Data.Int.Int32] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReconnectInfo'backoffMs
           (\ x__ y__ -> x__ {_ReconnectInfo'backoffMs = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ReconnectInfo "vec'backoffMs" (Data.Vector.Unboxed.Vector Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReconnectInfo'backoffMs
           (\ x__ y__ -> x__ {_ReconnectInfo'backoffMs = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReconnectInfo where
  messageName _ = Data.Text.pack "grpc.testing.ReconnectInfo"
  packedMessageDescriptor _
    = "\n\
      \\rReconnectInfo\DC2\SYN\n\
      \\ACKpassed\CAN\SOH \SOH(\bR\ACKpassed\DC2\GS\n\
      \\n\
      \backoff_ms\CAN\STX \ETX(\ENQR\tbackoffMs"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        passed__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "passed"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"passed")) ::
              Data.ProtoLens.FieldDescriptor ReconnectInfo
        backoffMs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "backoff_ms"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed (Data.ProtoLens.Field.field @"backoffMs")) ::
              Data.ProtoLens.FieldDescriptor ReconnectInfo
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, passed__field_descriptor),
           (Data.ProtoLens.Tag 2, backoffMs__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReconnectInfo'_unknownFields
        (\ x__ y__ -> x__ {_ReconnectInfo'_unknownFields = y__})
  defMessage
    = ReconnectInfo'_constructor
        {_ReconnectInfo'passed = Data.ProtoLens.fieldDefault,
         _ReconnectInfo'backoffMs = Data.Vector.Generic.empty,
         _ReconnectInfo'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReconnectInfo
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int32
             -> Data.ProtoLens.Encoding.Bytes.Parser ReconnectInfo
        loop x mutable'backoffMs
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'backoffMs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                               mutable'backoffMs)
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
                              (Data.ProtoLens.Field.field @"vec'backoffMs") frozen'backoffMs x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "passed"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"passed") y x)
                                  mutable'backoffMs
                        16
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "backoff_ms"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'backoffMs y)
                                loop x v
                        18
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
                                                                    "backoff_ms"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'backoffMs)
                                loop x y
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'backoffMs
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'backoffMs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'backoffMs)
          "ReconnectInfo"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"passed") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt (\ b -> if b then 1 else 0)
                         _v))
             ((Data.Monoid.<>)
                (let
                   p = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'backoffMs") _x
                 in
                   if Data.Vector.Generic.null p then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            (Data.ProtoLens.Encoding.Bytes.runBuilder
                               (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                                  p))))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ReconnectInfo where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReconnectInfo'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReconnectInfo'passed x__)
                (Control.DeepSeq.deepseq (_ReconnectInfo'backoffMs x__) ()))
{- | Fields :

         * 'Proto.Messages_Fields.maxReconnectBackoffMs' @:: Lens' ReconnectParams Data.Int.Int32@ -}
data ReconnectParams
  = ReconnectParams'_constructor {_ReconnectParams'maxReconnectBackoffMs :: !Data.Int.Int32,
                                  _ReconnectParams'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReconnectParams where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReconnectParams "maxReconnectBackoffMs" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReconnectParams'maxReconnectBackoffMs
           (\ x__ y__ -> x__ {_ReconnectParams'maxReconnectBackoffMs = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReconnectParams where
  messageName _ = Data.Text.pack "grpc.testing.ReconnectParams"
  packedMessageDescriptor _
    = "\n\
      \\SIReconnectParams\DC27\n\
      \\CANmax_reconnect_backoff_ms\CAN\SOH \SOH(\ENQR\NAKmaxReconnectBackoffMs"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        maxReconnectBackoffMs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "max_reconnect_backoff_ms"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"maxReconnectBackoffMs")) ::
              Data.ProtoLens.FieldDescriptor ReconnectParams
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, maxReconnectBackoffMs__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReconnectParams'_unknownFields
        (\ x__ y__ -> x__ {_ReconnectParams'_unknownFields = y__})
  defMessage
    = ReconnectParams'_constructor
        {_ReconnectParams'maxReconnectBackoffMs = Data.ProtoLens.fieldDefault,
         _ReconnectParams'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReconnectParams
          -> Data.ProtoLens.Encoding.Bytes.Parser ReconnectParams
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
                                       "max_reconnect_backoff_ms"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"maxReconnectBackoffMs") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ReconnectParams"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"maxReconnectBackoffMs") _x
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
instance Control.DeepSeq.NFData ReconnectParams where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReconnectParams'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReconnectParams'maxReconnectBackoffMs x__) ())
{- | Fields :

         * 'Proto.Messages_Fields.size' @:: Lens' ResponseParameters Data.Int.Int32@
         * 'Proto.Messages_Fields.intervalUs' @:: Lens' ResponseParameters Data.Int.Int32@
         * 'Proto.Messages_Fields.compressed' @:: Lens' ResponseParameters BoolValue@
         * 'Proto.Messages_Fields.maybe'compressed' @:: Lens' ResponseParameters (Prelude.Maybe BoolValue)@ -}
data ResponseParameters
  = ResponseParameters'_constructor {_ResponseParameters'size :: !Data.Int.Int32,
                                     _ResponseParameters'intervalUs :: !Data.Int.Int32,
                                     _ResponseParameters'compressed :: !(Prelude.Maybe BoolValue),
                                     _ResponseParameters'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResponseParameters where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ResponseParameters "size" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResponseParameters'size
           (\ x__ y__ -> x__ {_ResponseParameters'size = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ResponseParameters "intervalUs" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResponseParameters'intervalUs
           (\ x__ y__ -> x__ {_ResponseParameters'intervalUs = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ResponseParameters "compressed" BoolValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResponseParameters'compressed
           (\ x__ y__ -> x__ {_ResponseParameters'compressed = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ResponseParameters "maybe'compressed" (Prelude.Maybe BoolValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResponseParameters'compressed
           (\ x__ y__ -> x__ {_ResponseParameters'compressed = y__}))
        Prelude.id
instance Data.ProtoLens.Message ResponseParameters where
  messageName _ = Data.Text.pack "grpc.testing.ResponseParameters"
  packedMessageDescriptor _
    = "\n\
      \\DC2ResponseParameters\DC2\DC2\n\
      \\EOTsize\CAN\SOH \SOH(\ENQR\EOTsize\DC2\US\n\
      \\vinterval_us\CAN\STX \SOH(\ENQR\n\
      \intervalUs\DC27\n\
      \\n\
      \compressed\CAN\ETX \SOH(\v2\ETB.grpc.testing.BoolValueR\n\
      \compressed"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        size__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"size")) ::
              Data.ProtoLens.FieldDescriptor ResponseParameters
        intervalUs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "interval_us"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"intervalUs")) ::
              Data.ProtoLens.FieldDescriptor ResponseParameters
        compressed__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "compressed"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BoolValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'compressed")) ::
              Data.ProtoLens.FieldDescriptor ResponseParameters
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, size__field_descriptor),
           (Data.ProtoLens.Tag 2, intervalUs__field_descriptor),
           (Data.ProtoLens.Tag 3, compressed__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResponseParameters'_unknownFields
        (\ x__ y__ -> x__ {_ResponseParameters'_unknownFields = y__})
  defMessage
    = ResponseParameters'_constructor
        {_ResponseParameters'size = Data.ProtoLens.fieldDefault,
         _ResponseParameters'intervalUs = Data.ProtoLens.fieldDefault,
         _ResponseParameters'compressed = Prelude.Nothing,
         _ResponseParameters'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ResponseParameters
          -> Data.ProtoLens.Encoding.Bytes.Parser ResponseParameters
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
                                       "size"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"size") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "interval_us"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"intervalUs") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "compressed"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"compressed") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ResponseParameters"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"size") _x
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
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"intervalUs") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'compressed") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ResponseParameters where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ResponseParameters'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ResponseParameters'size x__)
                (Control.DeepSeq.deepseq
                   (_ResponseParameters'intervalUs x__)
                   (Control.DeepSeq.deepseq (_ResponseParameters'compressed x__) ())))
{- | Fields :

         * 'Proto.Messages_Fields.grpcCodeToReturn' @:: Lens' SetReturnStatusRequest Data.Int.Int32@
         * 'Proto.Messages_Fields.grpcStatusDescription' @:: Lens' SetReturnStatusRequest Data.Text.Text@ -}
data SetReturnStatusRequest
  = SetReturnStatusRequest'_constructor {_SetReturnStatusRequest'grpcCodeToReturn :: !Data.Int.Int32,
                                         _SetReturnStatusRequest'grpcStatusDescription :: !Data.Text.Text,
                                         _SetReturnStatusRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SetReturnStatusRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SetReturnStatusRequest "grpcCodeToReturn" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SetReturnStatusRequest'grpcCodeToReturn
           (\ x__ y__
              -> x__ {_SetReturnStatusRequest'grpcCodeToReturn = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SetReturnStatusRequest "grpcStatusDescription" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SetReturnStatusRequest'grpcStatusDescription
           (\ x__ y__
              -> x__ {_SetReturnStatusRequest'grpcStatusDescription = y__}))
        Prelude.id
instance Data.ProtoLens.Message SetReturnStatusRequest where
  messageName _
    = Data.Text.pack "grpc.testing.SetReturnStatusRequest"
  packedMessageDescriptor _
    = "\n\
      \\SYNSetReturnStatusRequest\DC2-\n\
      \\DC3grpc_code_to_return\CAN\SOH \SOH(\ENQR\DLEgrpcCodeToReturn\DC26\n\
      \\ETBgrpc_status_description\CAN\STX \SOH(\tR\NAKgrpcStatusDescription"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        grpcCodeToReturn__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "grpc_code_to_return"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"grpcCodeToReturn")) ::
              Data.ProtoLens.FieldDescriptor SetReturnStatusRequest
        grpcStatusDescription__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "grpc_status_description"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"grpcStatusDescription")) ::
              Data.ProtoLens.FieldDescriptor SetReturnStatusRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, grpcCodeToReturn__field_descriptor),
           (Data.ProtoLens.Tag 2, grpcStatusDescription__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SetReturnStatusRequest'_unknownFields
        (\ x__ y__ -> x__ {_SetReturnStatusRequest'_unknownFields = y__})
  defMessage
    = SetReturnStatusRequest'_constructor
        {_SetReturnStatusRequest'grpcCodeToReturn = Data.ProtoLens.fieldDefault,
         _SetReturnStatusRequest'grpcStatusDescription = Data.ProtoLens.fieldDefault,
         _SetReturnStatusRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SetReturnStatusRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser SetReturnStatusRequest
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
                                       "grpc_code_to_return"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"grpcCodeToReturn") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "grpc_status_description"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"grpcStatusDescription") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SetReturnStatusRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"grpcCodeToReturn") _x
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
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"grpcStatusDescription") _x
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
instance Control.DeepSeq.NFData SetReturnStatusRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SetReturnStatusRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SetReturnStatusRequest'grpcCodeToReturn x__)
                (Control.DeepSeq.deepseq
                   (_SetReturnStatusRequest'grpcStatusDescription x__) ()))
{- | Fields :

         * 'Proto.Messages_Fields.responseType' @:: Lens' SimpleRequest PayloadType@
         * 'Proto.Messages_Fields.responseSize' @:: Lens' SimpleRequest Data.Int.Int32@
         * 'Proto.Messages_Fields.payload' @:: Lens' SimpleRequest Payload@
         * 'Proto.Messages_Fields.maybe'payload' @:: Lens' SimpleRequest (Prelude.Maybe Payload)@
         * 'Proto.Messages_Fields.fillUsername' @:: Lens' SimpleRequest Prelude.Bool@
         * 'Proto.Messages_Fields.fillOauthScope' @:: Lens' SimpleRequest Prelude.Bool@
         * 'Proto.Messages_Fields.responseCompressed' @:: Lens' SimpleRequest BoolValue@
         * 'Proto.Messages_Fields.maybe'responseCompressed' @:: Lens' SimpleRequest (Prelude.Maybe BoolValue)@
         * 'Proto.Messages_Fields.responseStatus' @:: Lens' SimpleRequest EchoStatus@
         * 'Proto.Messages_Fields.maybe'responseStatus' @:: Lens' SimpleRequest (Prelude.Maybe EchoStatus)@
         * 'Proto.Messages_Fields.expectCompressed' @:: Lens' SimpleRequest BoolValue@
         * 'Proto.Messages_Fields.maybe'expectCompressed' @:: Lens' SimpleRequest (Prelude.Maybe BoolValue)@
         * 'Proto.Messages_Fields.fillServerId' @:: Lens' SimpleRequest Prelude.Bool@
         * 'Proto.Messages_Fields.fillGrpclbRouteType' @:: Lens' SimpleRequest Prelude.Bool@
         * 'Proto.Messages_Fields.orcaPerQueryReport' @:: Lens' SimpleRequest TestOrcaReport@
         * 'Proto.Messages_Fields.maybe'orcaPerQueryReport' @:: Lens' SimpleRequest (Prelude.Maybe TestOrcaReport)@ -}
data SimpleRequest
  = SimpleRequest'_constructor {_SimpleRequest'responseType :: !PayloadType,
                                _SimpleRequest'responseSize :: !Data.Int.Int32,
                                _SimpleRequest'payload :: !(Prelude.Maybe Payload),
                                _SimpleRequest'fillUsername :: !Prelude.Bool,
                                _SimpleRequest'fillOauthScope :: !Prelude.Bool,
                                _SimpleRequest'responseCompressed :: !(Prelude.Maybe BoolValue),
                                _SimpleRequest'responseStatus :: !(Prelude.Maybe EchoStatus),
                                _SimpleRequest'expectCompressed :: !(Prelude.Maybe BoolValue),
                                _SimpleRequest'fillServerId :: !Prelude.Bool,
                                _SimpleRequest'fillGrpclbRouteType :: !Prelude.Bool,
                                _SimpleRequest'orcaPerQueryReport :: !(Prelude.Maybe TestOrcaReport),
                                _SimpleRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SimpleRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SimpleRequest "responseType" PayloadType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'responseType
           (\ x__ y__ -> x__ {_SimpleRequest'responseType = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleRequest "responseSize" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'responseSize
           (\ x__ y__ -> x__ {_SimpleRequest'responseSize = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleRequest "payload" Payload where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'payload
           (\ x__ y__ -> x__ {_SimpleRequest'payload = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SimpleRequest "maybe'payload" (Prelude.Maybe Payload) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'payload
           (\ x__ y__ -> x__ {_SimpleRequest'payload = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleRequest "fillUsername" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'fillUsername
           (\ x__ y__ -> x__ {_SimpleRequest'fillUsername = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleRequest "fillOauthScope" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'fillOauthScope
           (\ x__ y__ -> x__ {_SimpleRequest'fillOauthScope = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleRequest "responseCompressed" BoolValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'responseCompressed
           (\ x__ y__ -> x__ {_SimpleRequest'responseCompressed = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SimpleRequest "maybe'responseCompressed" (Prelude.Maybe BoolValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'responseCompressed
           (\ x__ y__ -> x__ {_SimpleRequest'responseCompressed = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleRequest "responseStatus" EchoStatus where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'responseStatus
           (\ x__ y__ -> x__ {_SimpleRequest'responseStatus = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SimpleRequest "maybe'responseStatus" (Prelude.Maybe EchoStatus) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'responseStatus
           (\ x__ y__ -> x__ {_SimpleRequest'responseStatus = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleRequest "expectCompressed" BoolValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'expectCompressed
           (\ x__ y__ -> x__ {_SimpleRequest'expectCompressed = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SimpleRequest "maybe'expectCompressed" (Prelude.Maybe BoolValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'expectCompressed
           (\ x__ y__ -> x__ {_SimpleRequest'expectCompressed = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleRequest "fillServerId" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'fillServerId
           (\ x__ y__ -> x__ {_SimpleRequest'fillServerId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleRequest "fillGrpclbRouteType" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'fillGrpclbRouteType
           (\ x__ y__ -> x__ {_SimpleRequest'fillGrpclbRouteType = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleRequest "orcaPerQueryReport" TestOrcaReport where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'orcaPerQueryReport
           (\ x__ y__ -> x__ {_SimpleRequest'orcaPerQueryReport = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SimpleRequest "maybe'orcaPerQueryReport" (Prelude.Maybe TestOrcaReport) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleRequest'orcaPerQueryReport
           (\ x__ y__ -> x__ {_SimpleRequest'orcaPerQueryReport = y__}))
        Prelude.id
instance Data.ProtoLens.Message SimpleRequest where
  messageName _ = Data.Text.pack "grpc.testing.SimpleRequest"
  packedMessageDescriptor _
    = "\n\
      \\rSimpleRequest\DC2>\n\
      \\rresponse_type\CAN\SOH \SOH(\SO2\EM.grpc.testing.PayloadTypeR\fresponseType\DC2#\n\
      \\rresponse_size\CAN\STX \SOH(\ENQR\fresponseSize\DC2/\n\
      \\apayload\CAN\ETX \SOH(\v2\NAK.grpc.testing.PayloadR\apayload\DC2#\n\
      \\rfill_username\CAN\EOT \SOH(\bR\ffillUsername\DC2(\n\
      \\DLEfill_oauth_scope\CAN\ENQ \SOH(\bR\SOfillOauthScope\DC2H\n\
      \\DC3response_compressed\CAN\ACK \SOH(\v2\ETB.grpc.testing.BoolValueR\DC2responseCompressed\DC2A\n\
      \\SIresponse_status\CAN\a \SOH(\v2\CAN.grpc.testing.EchoStatusR\SOresponseStatus\DC2D\n\
      \\DC1expect_compressed\CAN\b \SOH(\v2\ETB.grpc.testing.BoolValueR\DLEexpectCompressed\DC2$\n\
      \\SOfill_server_id\CAN\t \SOH(\bR\ffillServerId\DC23\n\
      \\SYNfill_grpclb_route_type\CAN\n\
      \ \SOH(\bR\DC3fillGrpclbRouteType\DC2O\n\
      \\NAKorca_per_query_report\CAN\v \SOH(\v2\FS.grpc.testing.TestOrcaReportR\DC2orcaPerQueryReport"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        responseType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "response_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor PayloadType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"responseType")) ::
              Data.ProtoLens.FieldDescriptor SimpleRequest
        responseSize__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "response_size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"responseSize")) ::
              Data.ProtoLens.FieldDescriptor SimpleRequest
        payload__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "payload"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Payload)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'payload")) ::
              Data.ProtoLens.FieldDescriptor SimpleRequest
        fillUsername__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "fill_username"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"fillUsername")) ::
              Data.ProtoLens.FieldDescriptor SimpleRequest
        fillOauthScope__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "fill_oauth_scope"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"fillOauthScope")) ::
              Data.ProtoLens.FieldDescriptor SimpleRequest
        responseCompressed__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "response_compressed"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BoolValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'responseCompressed")) ::
              Data.ProtoLens.FieldDescriptor SimpleRequest
        responseStatus__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "response_status"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EchoStatus)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'responseStatus")) ::
              Data.ProtoLens.FieldDescriptor SimpleRequest
        expectCompressed__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "expect_compressed"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BoolValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'expectCompressed")) ::
              Data.ProtoLens.FieldDescriptor SimpleRequest
        fillServerId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "fill_server_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"fillServerId")) ::
              Data.ProtoLens.FieldDescriptor SimpleRequest
        fillGrpclbRouteType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "fill_grpclb_route_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"fillGrpclbRouteType")) ::
              Data.ProtoLens.FieldDescriptor SimpleRequest
        orcaPerQueryReport__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "orca_per_query_report"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TestOrcaReport)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'orcaPerQueryReport")) ::
              Data.ProtoLens.FieldDescriptor SimpleRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, responseType__field_descriptor),
           (Data.ProtoLens.Tag 2, responseSize__field_descriptor),
           (Data.ProtoLens.Tag 3, payload__field_descriptor),
           (Data.ProtoLens.Tag 4, fillUsername__field_descriptor),
           (Data.ProtoLens.Tag 5, fillOauthScope__field_descriptor),
           (Data.ProtoLens.Tag 6, responseCompressed__field_descriptor),
           (Data.ProtoLens.Tag 7, responseStatus__field_descriptor),
           (Data.ProtoLens.Tag 8, expectCompressed__field_descriptor),
           (Data.ProtoLens.Tag 9, fillServerId__field_descriptor),
           (Data.ProtoLens.Tag 10, fillGrpclbRouteType__field_descriptor),
           (Data.ProtoLens.Tag 11, orcaPerQueryReport__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SimpleRequest'_unknownFields
        (\ x__ y__ -> x__ {_SimpleRequest'_unknownFields = y__})
  defMessage
    = SimpleRequest'_constructor
        {_SimpleRequest'responseType = Data.ProtoLens.fieldDefault,
         _SimpleRequest'responseSize = Data.ProtoLens.fieldDefault,
         _SimpleRequest'payload = Prelude.Nothing,
         _SimpleRequest'fillUsername = Data.ProtoLens.fieldDefault,
         _SimpleRequest'fillOauthScope = Data.ProtoLens.fieldDefault,
         _SimpleRequest'responseCompressed = Prelude.Nothing,
         _SimpleRequest'responseStatus = Prelude.Nothing,
         _SimpleRequest'expectCompressed = Prelude.Nothing,
         _SimpleRequest'fillServerId = Data.ProtoLens.fieldDefault,
         _SimpleRequest'fillGrpclbRouteType = Data.ProtoLens.fieldDefault,
         _SimpleRequest'orcaPerQueryReport = Prelude.Nothing,
         _SimpleRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SimpleRequest -> Data.ProtoLens.Encoding.Bytes.Parser SimpleRequest
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
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "response_type"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"responseType") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "response_size"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"responseSize") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "payload"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"payload") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "fill_username"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"fillUsername") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "fill_oauth_scope"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"fillOauthScope") y x)
                        50
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "response_compressed"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"responseCompressed") y x)
                        58
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "response_status"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"responseStatus") y x)
                        66
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "expect_compressed"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"expectCompressed") y x)
                        72
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "fill_server_id"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"fillServerId") y x)
                        80
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "fill_grpclb_route_type"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"fillGrpclbRouteType") y x)
                        90
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "orca_per_query_report"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"orcaPerQueryReport") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SimpleRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"responseType") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                         Prelude.fromEnum _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"responseSize") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'payload") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view (Data.ProtoLens.Field.field @"fillUsername") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (\ b -> if b then 1 else 0) _v))
                      ((Data.Monoid.<>)
                         (let
                            _v
                              = Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"fillOauthScope") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (\ b -> if b then 1 else 0) _v))
                         ((Data.Monoid.<>)
                            (case
                                 Lens.Family2.view
                                   (Data.ProtoLens.Field.field @"maybe'responseCompressed") _x
                             of
                               Prelude.Nothing -> Data.Monoid.mempty
                               (Prelude.Just _v)
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                      ((Prelude..)
                                         (\ bs
                                            -> (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    (Prelude.fromIntegral
                                                       (Data.ByteString.length bs)))
                                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                         Data.ProtoLens.encodeMessage _v))
                            ((Data.Monoid.<>)
                               (case
                                    Lens.Family2.view
                                      (Data.ProtoLens.Field.field @"maybe'responseStatus") _x
                                of
                                  Prelude.Nothing -> Data.Monoid.mempty
                                  (Prelude.Just _v)
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                                         ((Prelude..)
                                            (\ bs
                                               -> (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                       (Prelude.fromIntegral
                                                          (Data.ByteString.length bs)))
                                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                            Data.ProtoLens.encodeMessage _v))
                               ((Data.Monoid.<>)
                                  (case
                                       Lens.Family2.view
                                         (Data.ProtoLens.Field.field @"maybe'expectCompressed") _x
                                   of
                                     Prelude.Nothing -> Data.Monoid.mempty
                                     (Prelude.Just _v)
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt 66)
                                            ((Prelude..)
                                               (\ bs
                                                  -> (Data.Monoid.<>)
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          (Prelude.fromIntegral
                                                             (Data.ByteString.length bs)))
                                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                               Data.ProtoLens.encodeMessage _v))
                                  ((Data.Monoid.<>)
                                     (let
                                        _v
                                          = Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"fillServerId") _x
                                      in
                                        if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                            Data.Monoid.mempty
                                        else
                                            (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt 72)
                                              ((Prelude..)
                                                 Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (\ b -> if b then 1 else 0) _v))
                                     ((Data.Monoid.<>)
                                        (let
                                           _v
                                             = Lens.Family2.view
                                                 (Data.ProtoLens.Field.field @"fillGrpclbRouteType")
                                                 _x
                                         in
                                           if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                               Data.Monoid.mempty
                                           else
                                               (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 80)
                                                 ((Prelude..)
                                                    Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    (\ b -> if b then 1 else 0) _v))
                                        ((Data.Monoid.<>)
                                           (case
                                                Lens.Family2.view
                                                  (Data.ProtoLens.Field.field
                                                     @"maybe'orcaPerQueryReport")
                                                  _x
                                            of
                                              Prelude.Nothing -> Data.Monoid.mempty
                                              (Prelude.Just _v)
                                                -> (Data.Monoid.<>)
                                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 90)
                                                     ((Prelude..)
                                                        (\ bs
                                                           -> (Data.Monoid.<>)
                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                   (Prelude.fromIntegral
                                                                      (Data.ByteString.length bs)))
                                                                (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                   bs))
                                                        Data.ProtoLens.encodeMessage _v))
                                           (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                              (Lens.Family2.view
                                                 Data.ProtoLens.unknownFields _x))))))))))))
instance Control.DeepSeq.NFData SimpleRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SimpleRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SimpleRequest'responseType x__)
                (Control.DeepSeq.deepseq
                   (_SimpleRequest'responseSize x__)
                   (Control.DeepSeq.deepseq
                      (_SimpleRequest'payload x__)
                      (Control.DeepSeq.deepseq
                         (_SimpleRequest'fillUsername x__)
                         (Control.DeepSeq.deepseq
                            (_SimpleRequest'fillOauthScope x__)
                            (Control.DeepSeq.deepseq
                               (_SimpleRequest'responseCompressed x__)
                               (Control.DeepSeq.deepseq
                                  (_SimpleRequest'responseStatus x__)
                                  (Control.DeepSeq.deepseq
                                     (_SimpleRequest'expectCompressed x__)
                                     (Control.DeepSeq.deepseq
                                        (_SimpleRequest'fillServerId x__)
                                        (Control.DeepSeq.deepseq
                                           (_SimpleRequest'fillGrpclbRouteType x__)
                                           (Control.DeepSeq.deepseq
                                              (_SimpleRequest'orcaPerQueryReport x__) ())))))))))))
{- | Fields :

         * 'Proto.Messages_Fields.payload' @:: Lens' SimpleResponse Payload@
         * 'Proto.Messages_Fields.maybe'payload' @:: Lens' SimpleResponse (Prelude.Maybe Payload)@
         * 'Proto.Messages_Fields.username' @:: Lens' SimpleResponse Data.Text.Text@
         * 'Proto.Messages_Fields.oauthScope' @:: Lens' SimpleResponse Data.Text.Text@
         * 'Proto.Messages_Fields.serverId' @:: Lens' SimpleResponse Data.Text.Text@
         * 'Proto.Messages_Fields.grpclbRouteType' @:: Lens' SimpleResponse GrpclbRouteType@
         * 'Proto.Messages_Fields.hostname' @:: Lens' SimpleResponse Data.Text.Text@ -}
data SimpleResponse
  = SimpleResponse'_constructor {_SimpleResponse'payload :: !(Prelude.Maybe Payload),
                                 _SimpleResponse'username :: !Data.Text.Text,
                                 _SimpleResponse'oauthScope :: !Data.Text.Text,
                                 _SimpleResponse'serverId :: !Data.Text.Text,
                                 _SimpleResponse'grpclbRouteType :: !GrpclbRouteType,
                                 _SimpleResponse'hostname :: !Data.Text.Text,
                                 _SimpleResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SimpleResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SimpleResponse "payload" Payload where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleResponse'payload
           (\ x__ y__ -> x__ {_SimpleResponse'payload = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SimpleResponse "maybe'payload" (Prelude.Maybe Payload) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleResponse'payload
           (\ x__ y__ -> x__ {_SimpleResponse'payload = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleResponse "username" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleResponse'username
           (\ x__ y__ -> x__ {_SimpleResponse'username = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleResponse "oauthScope" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleResponse'oauthScope
           (\ x__ y__ -> x__ {_SimpleResponse'oauthScope = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleResponse "serverId" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleResponse'serverId
           (\ x__ y__ -> x__ {_SimpleResponse'serverId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleResponse "grpclbRouteType" GrpclbRouteType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleResponse'grpclbRouteType
           (\ x__ y__ -> x__ {_SimpleResponse'grpclbRouteType = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SimpleResponse "hostname" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SimpleResponse'hostname
           (\ x__ y__ -> x__ {_SimpleResponse'hostname = y__}))
        Prelude.id
instance Data.ProtoLens.Message SimpleResponse where
  messageName _ = Data.Text.pack "grpc.testing.SimpleResponse"
  packedMessageDescriptor _
    = "\n\
      \\SOSimpleResponse\DC2/\n\
      \\apayload\CAN\SOH \SOH(\v2\NAK.grpc.testing.PayloadR\apayload\DC2\SUB\n\
      \\busername\CAN\STX \SOH(\tR\busername\DC2\US\n\
      \\voauth_scope\CAN\ETX \SOH(\tR\n\
      \oauthScope\DC2\ESC\n\
      \\tserver_id\CAN\EOT \SOH(\tR\bserverId\DC2I\n\
      \\DC1grpclb_route_type\CAN\ENQ \SOH(\SO2\GS.grpc.testing.GrpclbRouteTypeR\SIgrpclbRouteType\DC2\SUB\n\
      \\bhostname\CAN\ACK \SOH(\tR\bhostname"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        payload__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "payload"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Payload)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'payload")) ::
              Data.ProtoLens.FieldDescriptor SimpleResponse
        username__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "username"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"username")) ::
              Data.ProtoLens.FieldDescriptor SimpleResponse
        oauthScope__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "oauth_scope"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"oauthScope")) ::
              Data.ProtoLens.FieldDescriptor SimpleResponse
        serverId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "server_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"serverId")) ::
              Data.ProtoLens.FieldDescriptor SimpleResponse
        grpclbRouteType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "grpclb_route_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor GrpclbRouteType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"grpclbRouteType")) ::
              Data.ProtoLens.FieldDescriptor SimpleResponse
        hostname__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "hostname"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"hostname")) ::
              Data.ProtoLens.FieldDescriptor SimpleResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, payload__field_descriptor),
           (Data.ProtoLens.Tag 2, username__field_descriptor),
           (Data.ProtoLens.Tag 3, oauthScope__field_descriptor),
           (Data.ProtoLens.Tag 4, serverId__field_descriptor),
           (Data.ProtoLens.Tag 5, grpclbRouteType__field_descriptor),
           (Data.ProtoLens.Tag 6, hostname__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SimpleResponse'_unknownFields
        (\ x__ y__ -> x__ {_SimpleResponse'_unknownFields = y__})
  defMessage
    = SimpleResponse'_constructor
        {_SimpleResponse'payload = Prelude.Nothing,
         _SimpleResponse'username = Data.ProtoLens.fieldDefault,
         _SimpleResponse'oauthScope = Data.ProtoLens.fieldDefault,
         _SimpleResponse'serverId = Data.ProtoLens.fieldDefault,
         _SimpleResponse'grpclbRouteType = Data.ProtoLens.fieldDefault,
         _SimpleResponse'hostname = Data.ProtoLens.fieldDefault,
         _SimpleResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SimpleResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser SimpleResponse
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
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "payload"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"payload") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "username"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"username") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "oauth_scope"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"oauthScope") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "server_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"serverId") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "grpclb_route_type"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"grpclbRouteType") y x)
                        50
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "hostname"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"hostname") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SimpleResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'payload") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"username") _x
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
                   (let
                      _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"oauthScope") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8 _v))
                   ((Data.Monoid.<>)
                      (let
                         _v = Lens.Family2.view (Data.ProtoLens.Field.field @"serverId") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                               ((Prelude..)
                                  (\ bs
                                     -> (Data.Monoid.<>)
                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                             (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                  Data.Text.Encoding.encodeUtf8 _v))
                      ((Data.Monoid.<>)
                         (let
                            _v
                              = Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"grpclbRouteType") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     ((Prelude..)
                                        Data.ProtoLens.Encoding.Bytes.putVarInt
                                        Prelude.fromIntegral)
                                     Prelude.fromEnum _v))
                         ((Data.Monoid.<>)
                            (let
                               _v = Lens.Family2.view (Data.ProtoLens.Field.field @"hostname") _x
                             in
                               if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                     ((Prelude..)
                                        (\ bs
                                           -> (Data.Monoid.<>)
                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                   (Prelude.fromIntegral
                                                      (Data.ByteString.length bs)))
                                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                        Data.Text.Encoding.encodeUtf8 _v))
                            (Data.ProtoLens.Encoding.Wire.buildFieldSet
                               (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))))
instance Control.DeepSeq.NFData SimpleResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SimpleResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SimpleResponse'payload x__)
                (Control.DeepSeq.deepseq
                   (_SimpleResponse'username x__)
                   (Control.DeepSeq.deepseq
                      (_SimpleResponse'oauthScope x__)
                      (Control.DeepSeq.deepseq
                         (_SimpleResponse'serverId x__)
                         (Control.DeepSeq.deepseq
                            (_SimpleResponse'grpclbRouteType x__)
                            (Control.DeepSeq.deepseq (_SimpleResponse'hostname x__) ()))))))
{- | Fields :

         * 'Proto.Messages_Fields.payload' @:: Lens' StreamingInputCallRequest Payload@
         * 'Proto.Messages_Fields.maybe'payload' @:: Lens' StreamingInputCallRequest (Prelude.Maybe Payload)@
         * 'Proto.Messages_Fields.expectCompressed' @:: Lens' StreamingInputCallRequest BoolValue@
         * 'Proto.Messages_Fields.maybe'expectCompressed' @:: Lens' StreamingInputCallRequest (Prelude.Maybe BoolValue)@ -}
data StreamingInputCallRequest
  = StreamingInputCallRequest'_constructor {_StreamingInputCallRequest'payload :: !(Prelude.Maybe Payload),
                                            _StreamingInputCallRequest'expectCompressed :: !(Prelude.Maybe BoolValue),
                                            _StreamingInputCallRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StreamingInputCallRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StreamingInputCallRequest "payload" Payload where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingInputCallRequest'payload
           (\ x__ y__ -> x__ {_StreamingInputCallRequest'payload = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StreamingInputCallRequest "maybe'payload" (Prelude.Maybe Payload) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingInputCallRequest'payload
           (\ x__ y__ -> x__ {_StreamingInputCallRequest'payload = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StreamingInputCallRequest "expectCompressed" BoolValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingInputCallRequest'expectCompressed
           (\ x__ y__
              -> x__ {_StreamingInputCallRequest'expectCompressed = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StreamingInputCallRequest "maybe'expectCompressed" (Prelude.Maybe BoolValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingInputCallRequest'expectCompressed
           (\ x__ y__
              -> x__ {_StreamingInputCallRequest'expectCompressed = y__}))
        Prelude.id
instance Data.ProtoLens.Message StreamingInputCallRequest where
  messageName _
    = Data.Text.pack "grpc.testing.StreamingInputCallRequest"
  packedMessageDescriptor _
    = "\n\
      \\EMStreamingInputCallRequest\DC2/\n\
      \\apayload\CAN\SOH \SOH(\v2\NAK.grpc.testing.PayloadR\apayload\DC2D\n\
      \\DC1expect_compressed\CAN\STX \SOH(\v2\ETB.grpc.testing.BoolValueR\DLEexpectCompressed"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        payload__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "payload"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Payload)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'payload")) ::
              Data.ProtoLens.FieldDescriptor StreamingInputCallRequest
        expectCompressed__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "expect_compressed"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BoolValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'expectCompressed")) ::
              Data.ProtoLens.FieldDescriptor StreamingInputCallRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, payload__field_descriptor),
           (Data.ProtoLens.Tag 2, expectCompressed__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StreamingInputCallRequest'_unknownFields
        (\ x__ y__
           -> x__ {_StreamingInputCallRequest'_unknownFields = y__})
  defMessage
    = StreamingInputCallRequest'_constructor
        {_StreamingInputCallRequest'payload = Prelude.Nothing,
         _StreamingInputCallRequest'expectCompressed = Prelude.Nothing,
         _StreamingInputCallRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StreamingInputCallRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser StreamingInputCallRequest
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
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "payload"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"payload") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "expect_compressed"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"expectCompressed") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "StreamingInputCallRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'payload") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'expectCompressed") _x
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
instance Control.DeepSeq.NFData StreamingInputCallRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StreamingInputCallRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_StreamingInputCallRequest'payload x__)
                (Control.DeepSeq.deepseq
                   (_StreamingInputCallRequest'expectCompressed x__) ()))
{- | Fields :

         * 'Proto.Messages_Fields.aggregatedPayloadSize' @:: Lens' StreamingInputCallResponse Data.Int.Int32@ -}
data StreamingInputCallResponse
  = StreamingInputCallResponse'_constructor {_StreamingInputCallResponse'aggregatedPayloadSize :: !Data.Int.Int32,
                                             _StreamingInputCallResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StreamingInputCallResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StreamingInputCallResponse "aggregatedPayloadSize" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingInputCallResponse'aggregatedPayloadSize
           (\ x__ y__
              -> x__ {_StreamingInputCallResponse'aggregatedPayloadSize = y__}))
        Prelude.id
instance Data.ProtoLens.Message StreamingInputCallResponse where
  messageName _
    = Data.Text.pack "grpc.testing.StreamingInputCallResponse"
  packedMessageDescriptor _
    = "\n\
      \\SUBStreamingInputCallResponse\DC26\n\
      \\ETBaggregated_payload_size\CAN\SOH \SOH(\ENQR\NAKaggregatedPayloadSize"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        aggregatedPayloadSize__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "aggregated_payload_size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"aggregatedPayloadSize")) ::
              Data.ProtoLens.FieldDescriptor StreamingInputCallResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, aggregatedPayloadSize__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StreamingInputCallResponse'_unknownFields
        (\ x__ y__
           -> x__ {_StreamingInputCallResponse'_unknownFields = y__})
  defMessage
    = StreamingInputCallResponse'_constructor
        {_StreamingInputCallResponse'aggregatedPayloadSize = Data.ProtoLens.fieldDefault,
         _StreamingInputCallResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StreamingInputCallResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser StreamingInputCallResponse
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
                                       "aggregated_payload_size"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"aggregatedPayloadSize") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "StreamingInputCallResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"aggregatedPayloadSize") _x
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
instance Control.DeepSeq.NFData StreamingInputCallResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StreamingInputCallResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_StreamingInputCallResponse'aggregatedPayloadSize x__) ())
{- | Fields :

         * 'Proto.Messages_Fields.responseType' @:: Lens' StreamingOutputCallRequest PayloadType@
         * 'Proto.Messages_Fields.responseParameters' @:: Lens' StreamingOutputCallRequest [ResponseParameters]@
         * 'Proto.Messages_Fields.vec'responseParameters' @:: Lens' StreamingOutputCallRequest (Data.Vector.Vector ResponseParameters)@
         * 'Proto.Messages_Fields.payload' @:: Lens' StreamingOutputCallRequest Payload@
         * 'Proto.Messages_Fields.maybe'payload' @:: Lens' StreamingOutputCallRequest (Prelude.Maybe Payload)@
         * 'Proto.Messages_Fields.responseStatus' @:: Lens' StreamingOutputCallRequest EchoStatus@
         * 'Proto.Messages_Fields.maybe'responseStatus' @:: Lens' StreamingOutputCallRequest (Prelude.Maybe EchoStatus)@
         * 'Proto.Messages_Fields.orcaOobReport' @:: Lens' StreamingOutputCallRequest TestOrcaReport@
         * 'Proto.Messages_Fields.maybe'orcaOobReport' @:: Lens' StreamingOutputCallRequest (Prelude.Maybe TestOrcaReport)@ -}
data StreamingOutputCallRequest
  = StreamingOutputCallRequest'_constructor {_StreamingOutputCallRequest'responseType :: !PayloadType,
                                             _StreamingOutputCallRequest'responseParameters :: !(Data.Vector.Vector ResponseParameters),
                                             _StreamingOutputCallRequest'payload :: !(Prelude.Maybe Payload),
                                             _StreamingOutputCallRequest'responseStatus :: !(Prelude.Maybe EchoStatus),
                                             _StreamingOutputCallRequest'orcaOobReport :: !(Prelude.Maybe TestOrcaReport),
                                             _StreamingOutputCallRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StreamingOutputCallRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StreamingOutputCallRequest "responseType" PayloadType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingOutputCallRequest'responseType
           (\ x__ y__
              -> x__ {_StreamingOutputCallRequest'responseType = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StreamingOutputCallRequest "responseParameters" [ResponseParameters] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingOutputCallRequest'responseParameters
           (\ x__ y__
              -> x__ {_StreamingOutputCallRequest'responseParameters = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField StreamingOutputCallRequest "vec'responseParameters" (Data.Vector.Vector ResponseParameters) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingOutputCallRequest'responseParameters
           (\ x__ y__
              -> x__ {_StreamingOutputCallRequest'responseParameters = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StreamingOutputCallRequest "payload" Payload where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingOutputCallRequest'payload
           (\ x__ y__ -> x__ {_StreamingOutputCallRequest'payload = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StreamingOutputCallRequest "maybe'payload" (Prelude.Maybe Payload) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingOutputCallRequest'payload
           (\ x__ y__ -> x__ {_StreamingOutputCallRequest'payload = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StreamingOutputCallRequest "responseStatus" EchoStatus where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingOutputCallRequest'responseStatus
           (\ x__ y__
              -> x__ {_StreamingOutputCallRequest'responseStatus = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StreamingOutputCallRequest "maybe'responseStatus" (Prelude.Maybe EchoStatus) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingOutputCallRequest'responseStatus
           (\ x__ y__
              -> x__ {_StreamingOutputCallRequest'responseStatus = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StreamingOutputCallRequest "orcaOobReport" TestOrcaReport where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingOutputCallRequest'orcaOobReport
           (\ x__ y__
              -> x__ {_StreamingOutputCallRequest'orcaOobReport = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StreamingOutputCallRequest "maybe'orcaOobReport" (Prelude.Maybe TestOrcaReport) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingOutputCallRequest'orcaOobReport
           (\ x__ y__
              -> x__ {_StreamingOutputCallRequest'orcaOobReport = y__}))
        Prelude.id
instance Data.ProtoLens.Message StreamingOutputCallRequest where
  messageName _
    = Data.Text.pack "grpc.testing.StreamingOutputCallRequest"
  packedMessageDescriptor _
    = "\n\
      \\SUBStreamingOutputCallRequest\DC2>\n\
      \\rresponse_type\CAN\SOH \SOH(\SO2\EM.grpc.testing.PayloadTypeR\fresponseType\DC2Q\n\
      \\DC3response_parameters\CAN\STX \ETX(\v2 .grpc.testing.ResponseParametersR\DC2responseParameters\DC2/\n\
      \\apayload\CAN\ETX \SOH(\v2\NAK.grpc.testing.PayloadR\apayload\DC2A\n\
      \\SIresponse_status\CAN\a \SOH(\v2\CAN.grpc.testing.EchoStatusR\SOresponseStatus\DC2D\n\
      \\SIorca_oob_report\CAN\b \SOH(\v2\FS.grpc.testing.TestOrcaReportR\rorcaOobReport"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        responseType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "response_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor PayloadType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"responseType")) ::
              Data.ProtoLens.FieldDescriptor StreamingOutputCallRequest
        responseParameters__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "response_parameters"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ResponseParameters)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"responseParameters")) ::
              Data.ProtoLens.FieldDescriptor StreamingOutputCallRequest
        payload__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "payload"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Payload)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'payload")) ::
              Data.ProtoLens.FieldDescriptor StreamingOutputCallRequest
        responseStatus__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "response_status"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor EchoStatus)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'responseStatus")) ::
              Data.ProtoLens.FieldDescriptor StreamingOutputCallRequest
        orcaOobReport__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "orca_oob_report"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TestOrcaReport)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'orcaOobReport")) ::
              Data.ProtoLens.FieldDescriptor StreamingOutputCallRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, responseType__field_descriptor),
           (Data.ProtoLens.Tag 2, responseParameters__field_descriptor),
           (Data.ProtoLens.Tag 3, payload__field_descriptor),
           (Data.ProtoLens.Tag 7, responseStatus__field_descriptor),
           (Data.ProtoLens.Tag 8, orcaOobReport__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StreamingOutputCallRequest'_unknownFields
        (\ x__ y__
           -> x__ {_StreamingOutputCallRequest'_unknownFields = y__})
  defMessage
    = StreamingOutputCallRequest'_constructor
        {_StreamingOutputCallRequest'responseType = Data.ProtoLens.fieldDefault,
         _StreamingOutputCallRequest'responseParameters = Data.Vector.Generic.empty,
         _StreamingOutputCallRequest'payload = Prelude.Nothing,
         _StreamingOutputCallRequest'responseStatus = Prelude.Nothing,
         _StreamingOutputCallRequest'orcaOobReport = Prelude.Nothing,
         _StreamingOutputCallRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StreamingOutputCallRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ResponseParameters
             -> Data.ProtoLens.Encoding.Bytes.Parser StreamingOutputCallRequest
        loop x mutable'responseParameters
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'responseParameters <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                     (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                        mutable'responseParameters)
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
                              (Data.ProtoLens.Field.field @"vec'responseParameters")
                              frozen'responseParameters x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "response_type"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"responseType") y x)
                                  mutable'responseParameters
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "response_parameters"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'responseParameters y)
                                loop x v
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "payload"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"payload") y x)
                                  mutable'responseParameters
                        58
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "response_status"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"responseStatus") y x)
                                  mutable'responseParameters
                        66
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "orca_oob_report"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"orcaOobReport") y x)
                                  mutable'responseParameters
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'responseParameters
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'responseParameters <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'responseParameters)
          "StreamingOutputCallRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"responseType") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                         Prelude.fromEnum _v))
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
                   (Lens.Family2.view
                      (Data.ProtoLens.Field.field @"vec'responseParameters") _x))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'payload") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage _v))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'responseStatus") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                                ((Prelude..)
                                   (\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                   Data.ProtoLens.encodeMessage _v))
                      ((Data.Monoid.<>)
                         (case
                              Lens.Family2.view
                                (Data.ProtoLens.Field.field @"maybe'orcaOobReport") _x
                          of
                            Prelude.Nothing -> Data.Monoid.mempty
                            (Prelude.Just _v)
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt 66)
                                   ((Prelude..)
                                      (\ bs
                                         -> (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                              (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Data.ProtoLens.encodeMessage _v))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData StreamingOutputCallRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StreamingOutputCallRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_StreamingOutputCallRequest'responseType x__)
                (Control.DeepSeq.deepseq
                   (_StreamingOutputCallRequest'responseParameters x__)
                   (Control.DeepSeq.deepseq
                      (_StreamingOutputCallRequest'payload x__)
                      (Control.DeepSeq.deepseq
                         (_StreamingOutputCallRequest'responseStatus x__)
                         (Control.DeepSeq.deepseq
                            (_StreamingOutputCallRequest'orcaOobReport x__) ())))))
{- | Fields :

         * 'Proto.Messages_Fields.payload' @:: Lens' StreamingOutputCallResponse Payload@
         * 'Proto.Messages_Fields.maybe'payload' @:: Lens' StreamingOutputCallResponse (Prelude.Maybe Payload)@ -}
data StreamingOutputCallResponse
  = StreamingOutputCallResponse'_constructor {_StreamingOutputCallResponse'payload :: !(Prelude.Maybe Payload),
                                              _StreamingOutputCallResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StreamingOutputCallResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StreamingOutputCallResponse "payload" Payload where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingOutputCallResponse'payload
           (\ x__ y__ -> x__ {_StreamingOutputCallResponse'payload = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StreamingOutputCallResponse "maybe'payload" (Prelude.Maybe Payload) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StreamingOutputCallResponse'payload
           (\ x__ y__ -> x__ {_StreamingOutputCallResponse'payload = y__}))
        Prelude.id
instance Data.ProtoLens.Message StreamingOutputCallResponse where
  messageName _
    = Data.Text.pack "grpc.testing.StreamingOutputCallResponse"
  packedMessageDescriptor _
    = "\n\
      \\ESCStreamingOutputCallResponse\DC2/\n\
      \\apayload\CAN\SOH \SOH(\v2\NAK.grpc.testing.PayloadR\apayload"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        payload__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "payload"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Payload)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'payload")) ::
              Data.ProtoLens.FieldDescriptor StreamingOutputCallResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, payload__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StreamingOutputCallResponse'_unknownFields
        (\ x__ y__
           -> x__ {_StreamingOutputCallResponse'_unknownFields = y__})
  defMessage
    = StreamingOutputCallResponse'_constructor
        {_StreamingOutputCallResponse'payload = Prelude.Nothing,
         _StreamingOutputCallResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StreamingOutputCallResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser StreamingOutputCallResponse
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
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "payload"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"payload") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "StreamingOutputCallResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'payload") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData StreamingOutputCallResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StreamingOutputCallResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_StreamingOutputCallResponse'payload x__) ())
{- | Fields :

         * 'Proto.Messages_Fields.cpuUtilization' @:: Lens' TestOrcaReport Prelude.Double@
         * 'Proto.Messages_Fields.memoryUtilization' @:: Lens' TestOrcaReport Prelude.Double@
         * 'Proto.Messages_Fields.requestCost' @:: Lens' TestOrcaReport (Data.Map.Map Data.Text.Text Prelude.Double)@
         * 'Proto.Messages_Fields.utilization' @:: Lens' TestOrcaReport (Data.Map.Map Data.Text.Text Prelude.Double)@ -}
data TestOrcaReport
  = TestOrcaReport'_constructor {_TestOrcaReport'cpuUtilization :: !Prelude.Double,
                                 _TestOrcaReport'memoryUtilization :: !Prelude.Double,
                                 _TestOrcaReport'requestCost :: !(Data.Map.Map Data.Text.Text Prelude.Double),
                                 _TestOrcaReport'utilization :: !(Data.Map.Map Data.Text.Text Prelude.Double),
                                 _TestOrcaReport'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TestOrcaReport where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TestOrcaReport "cpuUtilization" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TestOrcaReport'cpuUtilization
           (\ x__ y__ -> x__ {_TestOrcaReport'cpuUtilization = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TestOrcaReport "memoryUtilization" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TestOrcaReport'memoryUtilization
           (\ x__ y__ -> x__ {_TestOrcaReport'memoryUtilization = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TestOrcaReport "requestCost" (Data.Map.Map Data.Text.Text Prelude.Double) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TestOrcaReport'requestCost
           (\ x__ y__ -> x__ {_TestOrcaReport'requestCost = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TestOrcaReport "utilization" (Data.Map.Map Data.Text.Text Prelude.Double) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TestOrcaReport'utilization
           (\ x__ y__ -> x__ {_TestOrcaReport'utilization = y__}))
        Prelude.id
instance Data.ProtoLens.Message TestOrcaReport where
  messageName _ = Data.Text.pack "grpc.testing.TestOrcaReport"
  packedMessageDescriptor _
    = "\n\
      \\SOTestOrcaReport\DC2'\n\
      \\SIcpu_utilization\CAN\SOH \SOH(\SOHR\SOcpuUtilization\DC2-\n\
      \\DC2memory_utilization\CAN\STX \SOH(\SOHR\DC1memoryUtilization\DC2P\n\
      \\frequest_cost\CAN\ETX \ETX(\v2-.grpc.testing.TestOrcaReport.RequestCostEntryR\vrequestCost\DC2O\n\
      \\vutilization\CAN\EOT \ETX(\v2-.grpc.testing.TestOrcaReport.UtilizationEntryR\vutilization\SUB>\n\
      \\DLERequestCostEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEUtilizationEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        cpuUtilization__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "cpu_utilization"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"cpuUtilization")) ::
              Data.ProtoLens.FieldDescriptor TestOrcaReport
        memoryUtilization__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "memory_utilization"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"memoryUtilization")) ::
              Data.ProtoLens.FieldDescriptor TestOrcaReport
        requestCost__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "request_cost"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TestOrcaReport'RequestCostEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"requestCost")) ::
              Data.ProtoLens.FieldDescriptor TestOrcaReport
        utilization__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "utilization"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TestOrcaReport'UtilizationEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"utilization")) ::
              Data.ProtoLens.FieldDescriptor TestOrcaReport
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, cpuUtilization__field_descriptor),
           (Data.ProtoLens.Tag 2, memoryUtilization__field_descriptor),
           (Data.ProtoLens.Tag 3, requestCost__field_descriptor),
           (Data.ProtoLens.Tag 4, utilization__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TestOrcaReport'_unknownFields
        (\ x__ y__ -> x__ {_TestOrcaReport'_unknownFields = y__})
  defMessage
    = TestOrcaReport'_constructor
        {_TestOrcaReport'cpuUtilization = Data.ProtoLens.fieldDefault,
         _TestOrcaReport'memoryUtilization = Data.ProtoLens.fieldDefault,
         _TestOrcaReport'requestCost = Data.Map.empty,
         _TestOrcaReport'utilization = Data.Map.empty,
         _TestOrcaReport'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TestOrcaReport
          -> Data.ProtoLens.Encoding.Bytes.Parser TestOrcaReport
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
                        9 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToDouble
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "cpu_utilization"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"cpuUtilization") y x)
                        17
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToDouble
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "memory_utilization"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"memoryUtilization") y x)
                        26
                          -> do !(entry :: TestOrcaReport'RequestCostEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "request_cost"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"requestCost")
                                        (\ !t -> Data.Map.insert key value t) x))
                        34
                          -> do !(entry :: TestOrcaReport'UtilizationEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                 (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                     Data.ProtoLens.Encoding.Bytes.isolate
                                                                                       (Prelude.fromIntegral
                                                                                          len)
                                                                                       Data.ProtoLens.parseMessage)
                                                                                 "utilization"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"utilization")
                                        (\ !t -> Data.Map.insert key value t) x))
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TestOrcaReport"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"cpuUtilization") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 9)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putFixed64
                         Data.ProtoLens.Encoding.Bytes.doubleToWord _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"memoryUtilization") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 17)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putFixed64
                            Data.ProtoLens.Encoding.Bytes.doubleToWord _v))
                ((Data.Monoid.<>)
                   (Data.Monoid.mconcat
                      (Prelude.map
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage
                                    (Lens.Family2.set
                                       (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                       (Lens.Family2.set
                                          (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                          (Data.ProtoLens.defMessage ::
                                             TestOrcaReport'RequestCostEntry)))))
                         (Data.Map.toList
                            (Lens.Family2.view
                               (Data.ProtoLens.Field.field @"requestCost") _x))))
                   ((Data.Monoid.<>)
                      (Data.Monoid.mconcat
                         (Prelude.map
                            (\ _v
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                    ((Prelude..)
                                       (\ bs
                                          -> (Data.Monoid.<>)
                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                  (Prelude.fromIntegral
                                                     (Data.ByteString.length bs)))
                                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                       Data.ProtoLens.encodeMessage
                                       (Lens.Family2.set
                                          (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                             (Data.ProtoLens.defMessage ::
                                                TestOrcaReport'UtilizationEntry)))))
                            (Data.Map.toList
                               (Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"utilization") _x))))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData TestOrcaReport where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TestOrcaReport'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TestOrcaReport'cpuUtilization x__)
                (Control.DeepSeq.deepseq
                   (_TestOrcaReport'memoryUtilization x__)
                   (Control.DeepSeq.deepseq
                      (_TestOrcaReport'requestCost x__)
                      (Control.DeepSeq.deepseq (_TestOrcaReport'utilization x__) ()))))
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' TestOrcaReport'RequestCostEntry Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' TestOrcaReport'RequestCostEntry Prelude.Double@ -}
data TestOrcaReport'RequestCostEntry
  = TestOrcaReport'RequestCostEntry'_constructor {_TestOrcaReport'RequestCostEntry'key :: !Data.Text.Text,
                                                  _TestOrcaReport'RequestCostEntry'value :: !Prelude.Double,
                                                  _TestOrcaReport'RequestCostEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TestOrcaReport'RequestCostEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TestOrcaReport'RequestCostEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TestOrcaReport'RequestCostEntry'key
           (\ x__ y__ -> x__ {_TestOrcaReport'RequestCostEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TestOrcaReport'RequestCostEntry "value" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TestOrcaReport'RequestCostEntry'value
           (\ x__ y__ -> x__ {_TestOrcaReport'RequestCostEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message TestOrcaReport'RequestCostEntry where
  messageName _
    = Data.Text.pack "grpc.testing.TestOrcaReport.RequestCostEntry"
  packedMessageDescriptor _
    = "\n\
      \\DLERequestCostEntry\DC2\DLE\n\
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
              Data.ProtoLens.FieldDescriptor TestOrcaReport'RequestCostEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor TestOrcaReport'RequestCostEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TestOrcaReport'RequestCostEntry'_unknownFields
        (\ x__ y__
           -> x__ {_TestOrcaReport'RequestCostEntry'_unknownFields = y__})
  defMessage
    = TestOrcaReport'RequestCostEntry'_constructor
        {_TestOrcaReport'RequestCostEntry'key = Data.ProtoLens.fieldDefault,
         _TestOrcaReport'RequestCostEntry'value = Data.ProtoLens.fieldDefault,
         _TestOrcaReport'RequestCostEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TestOrcaReport'RequestCostEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser TestOrcaReport'RequestCostEntry
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
          (do loop Data.ProtoLens.defMessage) "RequestCostEntry"
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
instance Control.DeepSeq.NFData TestOrcaReport'RequestCostEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TestOrcaReport'RequestCostEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TestOrcaReport'RequestCostEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_TestOrcaReport'RequestCostEntry'value x__) ()))
{- | Fields :

         * 'Proto.Messages_Fields.key' @:: Lens' TestOrcaReport'UtilizationEntry Data.Text.Text@
         * 'Proto.Messages_Fields.value' @:: Lens' TestOrcaReport'UtilizationEntry Prelude.Double@ -}
data TestOrcaReport'UtilizationEntry
  = TestOrcaReport'UtilizationEntry'_constructor {_TestOrcaReport'UtilizationEntry'key :: !Data.Text.Text,
                                                  _TestOrcaReport'UtilizationEntry'value :: !Prelude.Double,
                                                  _TestOrcaReport'UtilizationEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TestOrcaReport'UtilizationEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TestOrcaReport'UtilizationEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TestOrcaReport'UtilizationEntry'key
           (\ x__ y__ -> x__ {_TestOrcaReport'UtilizationEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TestOrcaReport'UtilizationEntry "value" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TestOrcaReport'UtilizationEntry'value
           (\ x__ y__ -> x__ {_TestOrcaReport'UtilizationEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message TestOrcaReport'UtilizationEntry where
  messageName _
    = Data.Text.pack "grpc.testing.TestOrcaReport.UtilizationEntry"
  packedMessageDescriptor _
    = "\n\
      \\DLEUtilizationEntry\DC2\DLE\n\
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
              Data.ProtoLens.FieldDescriptor TestOrcaReport'UtilizationEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor TestOrcaReport'UtilizationEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TestOrcaReport'UtilizationEntry'_unknownFields
        (\ x__ y__
           -> x__ {_TestOrcaReport'UtilizationEntry'_unknownFields = y__})
  defMessage
    = TestOrcaReport'UtilizationEntry'_constructor
        {_TestOrcaReport'UtilizationEntry'key = Data.ProtoLens.fieldDefault,
         _TestOrcaReport'UtilizationEntry'value = Data.ProtoLens.fieldDefault,
         _TestOrcaReport'UtilizationEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TestOrcaReport'UtilizationEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser TestOrcaReport'UtilizationEntry
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
          (do loop Data.ProtoLens.defMessage) "UtilizationEntry"
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
instance Control.DeepSeq.NFData TestOrcaReport'UtilizationEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TestOrcaReport'UtilizationEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TestOrcaReport'UtilizationEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_TestOrcaReport'UtilizationEntry'value x__) ()))
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\SOmessages.proto\DC2\fgrpc.testing\"!\n\
    \\tBoolValue\DC2\DC4\n\
    \\ENQvalue\CAN\SOH \SOH(\bR\ENQvalue\"L\n\
    \\aPayload\DC2-\n\
    \\EOTtype\CAN\SOH \SOH(\SO2\EM.grpc.testing.PayloadTypeR\EOTtype\DC2\DC2\n\
    \\EOTbody\CAN\STX \SOH(\fR\EOTbody\":\n\
    \\n\
    \EchoStatus\DC2\DC2\n\
    \\EOTcode\CAN\SOH \SOH(\ENQR\EOTcode\DC2\CAN\n\
    \\amessage\CAN\STX \SOH(\tR\amessage\"\243\EOT\n\
    \\rSimpleRequest\DC2>\n\
    \\rresponse_type\CAN\SOH \SOH(\SO2\EM.grpc.testing.PayloadTypeR\fresponseType\DC2#\n\
    \\rresponse_size\CAN\STX \SOH(\ENQR\fresponseSize\DC2/\n\
    \\apayload\CAN\ETX \SOH(\v2\NAK.grpc.testing.PayloadR\apayload\DC2#\n\
    \\rfill_username\CAN\EOT \SOH(\bR\ffillUsername\DC2(\n\
    \\DLEfill_oauth_scope\CAN\ENQ \SOH(\bR\SOfillOauthScope\DC2H\n\
    \\DC3response_compressed\CAN\ACK \SOH(\v2\ETB.grpc.testing.BoolValueR\DC2responseCompressed\DC2A\n\
    \\SIresponse_status\CAN\a \SOH(\v2\CAN.grpc.testing.EchoStatusR\SOresponseStatus\DC2D\n\
    \\DC1expect_compressed\CAN\b \SOH(\v2\ETB.grpc.testing.BoolValueR\DLEexpectCompressed\DC2$\n\
    \\SOfill_server_id\CAN\t \SOH(\bR\ffillServerId\DC23\n\
    \\SYNfill_grpclb_route_type\CAN\n\
    \ \SOH(\bR\DC3fillGrpclbRouteType\DC2O\n\
    \\NAKorca_per_query_report\CAN\v \SOH(\v2\FS.grpc.testing.TestOrcaReportR\DC2orcaPerQueryReport\"\130\STX\n\
    \\SOSimpleResponse\DC2/\n\
    \\apayload\CAN\SOH \SOH(\v2\NAK.grpc.testing.PayloadR\apayload\DC2\SUB\n\
    \\busername\CAN\STX \SOH(\tR\busername\DC2\US\n\
    \\voauth_scope\CAN\ETX \SOH(\tR\n\
    \oauthScope\DC2\ESC\n\
    \\tserver_id\CAN\EOT \SOH(\tR\bserverId\DC2I\n\
    \\DC1grpclb_route_type\CAN\ENQ \SOH(\SO2\GS.grpc.testing.GrpclbRouteTypeR\SIgrpclbRouteType\DC2\SUB\n\
    \\bhostname\CAN\ACK \SOH(\tR\bhostname\"\146\SOH\n\
    \\EMStreamingInputCallRequest\DC2/\n\
    \\apayload\CAN\SOH \SOH(\v2\NAK.grpc.testing.PayloadR\apayload\DC2D\n\
    \\DC1expect_compressed\CAN\STX \SOH(\v2\ETB.grpc.testing.BoolValueR\DLEexpectCompressed\"T\n\
    \\SUBStreamingInputCallResponse\DC26\n\
    \\ETBaggregated_payload_size\CAN\SOH \SOH(\ENQR\NAKaggregatedPayloadSize\"\130\SOH\n\
    \\DC2ResponseParameters\DC2\DC2\n\
    \\EOTsize\CAN\SOH \SOH(\ENQR\EOTsize\DC2\US\n\
    \\vinterval_us\CAN\STX \SOH(\ENQR\n\
    \intervalUs\DC27\n\
    \\n\
    \compressed\CAN\ETX \SOH(\v2\ETB.grpc.testing.BoolValueR\n\
    \compressed\"\233\STX\n\
    \\SUBStreamingOutputCallRequest\DC2>\n\
    \\rresponse_type\CAN\SOH \SOH(\SO2\EM.grpc.testing.PayloadTypeR\fresponseType\DC2Q\n\
    \\DC3response_parameters\CAN\STX \ETX(\v2 .grpc.testing.ResponseParametersR\DC2responseParameters\DC2/\n\
    \\apayload\CAN\ETX \SOH(\v2\NAK.grpc.testing.PayloadR\apayload\DC2A\n\
    \\SIresponse_status\CAN\a \SOH(\v2\CAN.grpc.testing.EchoStatusR\SOresponseStatus\DC2D\n\
    \\SIorca_oob_report\CAN\b \SOH(\v2\FS.grpc.testing.TestOrcaReportR\rorcaOobReport\"N\n\
    \\ESCStreamingOutputCallResponse\DC2/\n\
    \\apayload\CAN\SOH \SOH(\v2\NAK.grpc.testing.PayloadR\apayload\"J\n\
    \\SIReconnectParams\DC27\n\
    \\CANmax_reconnect_backoff_ms\CAN\SOH \SOH(\ENQR\NAKmaxReconnectBackoffMs\"F\n\
    \\rReconnectInfo\DC2\SYN\n\
    \\ACKpassed\CAN\SOH \SOH(\bR\ACKpassed\DC2\GS\n\
    \\n\
    \backoff_ms\CAN\STX \ETX(\ENQR\tbackoffMs\"{\n\
    \\CANLoadBalancerStatsRequest\DC2\EM\n\
    \\bnum_rpcs\CAN\SOH \SOH(\ENQR\anumRpcs\DC2\US\n\
    \\vtimeout_sec\CAN\STX \SOH(\ENQR\n\
    \timeoutSec\DC2#\n\
    \\rmetadata_keys\CAN\ETX \ETX(\tR\fmetadataKeys\"\208\t\n\
    \\EMLoadBalancerStatsResponse\DC2Y\n\
    \\frpcs_by_peer\CAN\SOH \ETX(\v27.grpc.testing.LoadBalancerStatsResponse.RpcsByPeerEntryR\n\
    \rpcsByPeer\DC2!\n\
    \\fnum_failures\CAN\STX \SOH(\ENQR\vnumFailures\DC2_\n\
    \\SOrpcs_by_method\CAN\ETX \ETX(\v29.grpc.testing.LoadBalancerStatsResponse.RpcsByMethodEntryR\frpcsByMethod\DC2h\n\
    \\DC1metadatas_by_peer\CAN\EOT \ETX(\v2<.grpc.testing.LoadBalancerStatsResponse.MetadatasByPeerEntryR\SImetadatasByPeer\SUB\129\SOH\n\
    \\rMetadataEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\tR\ENQvalue\DC2H\n\
    \\EOTtype\CAN\ETX \SOH(\SO24.grpc.testing.LoadBalancerStatsResponse.MetadataTypeR\EOTtype\SUB`\n\
    \\vRpcMetadata\DC2Q\n\
    \\bmetadata\CAN\SOH \ETX(\v25.grpc.testing.LoadBalancerStatsResponse.MetadataEntryR\bmetadata\SUBh\n\
    \\SOMetadataByPeer\DC2V\n\
    \\frpc_metadata\CAN\SOH \ETX(\v23.grpc.testing.LoadBalancerStatsResponse.RpcMetadataR\vrpcMetadata\SUB\177\SOH\n\
    \\n\
    \RpcsByPeer\DC2d\n\
    \\frpcs_by_peer\CAN\SOH \ETX(\v2B.grpc.testing.LoadBalancerStatsResponse.RpcsByPeer.RpcsByPeerEntryR\n\
    \rpcsByPeer\SUB=\n\
    \\SIRpcsByPeerEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUB=\n\
    \\SIRpcsByPeerEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUBs\n\
    \\DC1RpcsByMethodEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2H\n\
    \\ENQvalue\CAN\STX \SOH(\v22.grpc.testing.LoadBalancerStatsResponse.RpcsByPeerR\ENQvalue:\STX8\SOH\SUBz\n\
    \\DC4MetadatasByPeerEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2L\n\
    \\ENQvalue\CAN\STX \SOH(\v26.grpc.testing.LoadBalancerStatsResponse.MetadataByPeerR\ENQvalue:\STX8\SOH\"6\n\
    \\fMetadataType\DC2\v\n\
    \\aUNKNOWN\DLE\NUL\DC2\v\n\
    \\aINITIAL\DLE\SOH\DC2\f\n\
    \\bTRAILING\DLE\STX\"%\n\
    \#LoadBalancerAccumulatedStatsRequest\"\134\t\n\
    \$LoadBalancerAccumulatedStatsResponse\DC2\142\SOH\n\
    \\SUBnum_rpcs_started_by_method\CAN\SOH \ETX(\v2N.grpc.testing.LoadBalancerAccumulatedStatsResponse.NumRpcsStartedByMethodEntryR\SYNnumRpcsStartedByMethodB\STX\CAN\SOH\DC2\148\SOH\n\
    \\FSnum_rpcs_succeeded_by_method\CAN\STX \ETX(\v2P.grpc.testing.LoadBalancerAccumulatedStatsResponse.NumRpcsSucceededByMethodEntryR\CANnumRpcsSucceededByMethodB\STX\CAN\SOH\DC2\139\SOH\n\
    \\EMnum_rpcs_failed_by_method\CAN\ETX \ETX(\v2M.grpc.testing.LoadBalancerAccumulatedStatsResponse.NumRpcsFailedByMethodEntryR\NAKnumRpcsFailedByMethodB\STX\CAN\SOH\DC2p\n\
    \\DLEstats_per_method\CAN\EOT \ETX(\v2F.grpc.testing.LoadBalancerAccumulatedStatsResponse.StatsPerMethodEntryR\SOstatsPerMethod\SUBI\n\
    \\ESCNumRpcsStartedByMethodEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUBK\n\
    \\GSNumRpcsSucceededByMethodEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUBH\n\
    \\SUBNumRpcsFailedByMethodEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUB\207\SOH\n\
    \\vMethodStats\DC2!\n\
    \\frpcs_started\CAN\SOH \SOH(\ENQR\vrpcsStarted\DC2b\n\
    \\ACKresult\CAN\STX \ETX(\v2J.grpc.testing.LoadBalancerAccumulatedStatsResponse.MethodStats.ResultEntryR\ACKresult\SUB9\n\
    \\vResultEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\ENQR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\ENQR\ENQvalue:\STX8\SOH\SUB\129\SOH\n\
    \\DC3StatsPerMethodEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2T\n\
    \\ENQvalue\CAN\STX \SOH(\v2>.grpc.testing.LoadBalancerAccumulatedStatsResponse.MethodStatsR\ENQvalue:\STX8\SOH\"\233\STX\n\
    \\SYNClientConfigureRequest\DC2B\n\
    \\ENQtypes\CAN\SOH \ETX(\SO2,.grpc.testing.ClientConfigureRequest.RpcTypeR\ENQtypes\DC2I\n\
    \\bmetadata\CAN\STX \ETX(\v2-.grpc.testing.ClientConfigureRequest.MetadataR\bmetadata\DC2\US\n\
    \\vtimeout_sec\CAN\ETX \SOH(\ENQR\n\
    \timeoutSec\SUBt\n\
    \\bMetadata\DC2@\n\
    \\EOTtype\CAN\SOH \SOH(\SO2,.grpc.testing.ClientConfigureRequest.RpcTypeR\EOTtype\DC2\DLE\n\
    \\ETXkey\CAN\STX \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\ETX \SOH(\tR\ENQvalue\")\n\
    \\aRpcType\DC2\SO\n\
    \\n\
    \EMPTY_CALL\DLE\NUL\DC2\SO\n\
    \\n\
    \UNARY_CALL\DLE\SOH\"\EM\n\
    \\ETBClientConfigureResponse\"\RS\n\
    \\n\
    \MemorySize\DC2\DLE\n\
    \\ETXrss\CAN\SOH \SOH(\ETXR\ETXrss\"\139\ETX\n\
    \\SOTestOrcaReport\DC2'\n\
    \\SIcpu_utilization\CAN\SOH \SOH(\SOHR\SOcpuUtilization\DC2-\n\
    \\DC2memory_utilization\CAN\STX \SOH(\SOHR\DC1memoryUtilization\DC2P\n\
    \\frequest_cost\CAN\ETX \ETX(\v2-.grpc.testing.TestOrcaReport.RequestCostEntryR\vrequestCost\DC2O\n\
    \\vutilization\CAN\EOT \ETX(\v2-.grpc.testing.TestOrcaReport.UtilizationEntryR\vutilization\SUB>\n\
    \\DLERequestCostEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEUtilizationEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOH\"\DEL\n\
    \\SYNSetReturnStatusRequest\DC2-\n\
    \\DC3grpc_code_to_return\CAN\SOH \SOH(\ENQR\DLEgrpcCodeToReturn\DC26\n\
    \\ETBgrpc_status_description\CAN\STX \SOH(\tR\NAKgrpcStatusDescription\"\165\STX\n\
    \\vHookRequest\DC2F\n\
    \\acommand\CAN\SOH \SOH(\SO2,.grpc.testing.HookRequest.HookRequestCommandR\acommand\DC2-\n\
    \\DC3grpc_code_to_return\CAN\STX \SOH(\ENQR\DLEgrpcCodeToReturn\DC26\n\
    \\ETBgrpc_status_description\CAN\ETX \SOH(\tR\NAKgrpcStatusDescription\DC2\US\n\
    \\vserver_port\CAN\EOT \SOH(\ENQR\n\
    \serverPort\"F\n\
    \\DC2HookRequestCommand\DC2\SI\n\
    \\vUNSPECIFIED\DLE\NUL\DC2\t\n\
    \\ENQSTART\DLE\SOH\DC2\b\n\
    \\EOTSTOP\DLE\STX\DC2\n\
    \\n\
    \\ACKRETURN\DLE\ETX\"\SO\n\
    \\fHookResponse*\US\n\
    \\vPayloadType\DC2\DLE\n\
    \\fCOMPRESSABLE\DLE\NUL*o\n\
    \\SIGrpclbRouteType\DC2\GS\n\
    \\EMGRPCLB_ROUTE_TYPE_UNKNOWN\DLE\NUL\DC2\RS\n\
    \\SUBGRPCLB_ROUTE_TYPE_FALLBACK\DLE\SOH\DC2\GS\n\
    \\EMGRPCLB_ROUTE_TYPE_BACKEND\DLE\STXB\GS\n\
    \\ESCio.grpc.testing.integrationJ\171i\n\
    \\a\DC2\ENQ\DLE\NUL\218\STX\SOH\n\
    \\143\ENQ\n\
    \\SOH\f\DC2\ETX\DLE\NUL\DC22\185\EOT Copyright 2015-2016 gRPC authors.\n\
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
    \2I Message definitions to be used by integration test service definitions.\n\
    \\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\DC2\NUL\NAK\n\
    \\b\n\
    \\SOH\b\DC2\ETX\DC4\NUL4\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\DC4\NUL4\n\
    \\164\SOH\n\
    \\STX\EOT\NUL\DC2\EOT\EM\NUL\FS\SOH\SUB\151\SOH TODO(dgq): Go back to using well-known types once\n\
    \ https://github.com/grpc/grpc/issues/6980 has been fixed.\n\
    \ import \"google/protobuf/wrappers.proto\";\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\EM\b\DC1\n\
    \\RS\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\ESC\STX\DC1\SUB\DC1 The bool value.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\ESC\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\ESC\a\f\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\ESC\SI\DLE\n\
    \:\n\
    \\STX\ENQ\NUL\DC2\EOT\US\NUL\"\SOH\SUB. The type of payload that should be returned.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ENQ\NUL\SOH\DC2\ETX\US\ENQ\DLE\n\
    \(\n\
    \\EOT\ENQ\NUL\STX\NUL\DC2\ETX!\STX\DC3\SUB\ESC Compressable text format.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\SOH\DC2\ETX!\STX\SO\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\STX\DC2\ETX!\DC1\DC2\n\
    \D\n\
    \\STX\EOT\SOH\DC2\EOT%\NUL*\SOH\SUB8 A block of data, to simply increase gRPC message size.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX%\b\SI\n\
    \(\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX'\STX\ETB\SUB\ESC The type of data in body.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX'\STX\r\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX'\SO\DC2\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX'\NAK\SYN\n\
    \+\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX)\STX\DC1\SUB\RS Primary contents of payload.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ENQ\DC2\ETX)\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX)\b\f\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX)\SI\DLE\n\
    \\149\SOH\n\
    \\STX\EOT\STX\DC2\EOT.\NUL1\SOH\SUB\136\SOH A protobuf representation for grpc status. This is used by test\n\
    \ clients to specify a status that the server should attempt to return.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX.\b\DC2\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX/\STX\DC1\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX/\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX/\b\f\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX/\SI\DLE\n\
    \\v\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX0\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ENQ\DC2\ETX0\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX0\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX0\DC3\DC4\n\
    \\184\ETX\n\
    \\STX\ENQ\SOH\DC2\EOT9\NUL@\SOH\SUB\171\ETX The type of route that a client took to reach a server w.r.t. gRPCLB.\n\
    \ The server must fill in \"fallback\" if it detects that the RPC reached\n\
    \ the server via the \"gRPCLB fallback\" path, and \"backend\" if it detects\n\
    \ that the RPC reached the server via \"gRPCLB backend\" path (i.e. if it got\n\
    \ the address of this server from the gRPCLB server BalanceLoad RPC). Exactly\n\
    \ how this detection is done is context and server dependent.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ENQ\SOH\SOH\DC2\ETX9\ENQ\DC4\n\
    \M\n\
    \\EOT\ENQ\SOH\STX\NUL\DC2\ETX;\STX \SUB@ Server didn't detect the route that a client took to reach it.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\NUL\SOH\DC2\ETX;\STX\ESC\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\NUL\STX\DC2\ETX;\RS\US\n\
    \L\n\
    \\EOT\ENQ\SOH\STX\SOH\DC2\ETX=\STX!\SUB? Indicates that a client reached a server via gRPCLB fallback.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\SOH\SOH\DC2\ETX=\STX\FS\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\SOH\STX\DC2\ETX=\US \n\
    \R\n\
    \\EOT\ENQ\SOH\STX\STX\DC2\ETX?\STX \SUBE Indicates that a client reached a server as a gRPCLB-given backend.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\STX\SOH\DC2\ETX?\STX\ESC\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\STX\STX\DC2\ETX?\RS\US\n\
    \\FS\n\
    \\STX\EOT\ETX\DC2\EOTC\NULh\SOH\SUB\DLE Unary request.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETXC\b\NAK\n\
    \\146\SOH\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETXF\STX \SUB\132\SOH Desired payload type in the response from the server.\n\
    \ If response_type is RANDOM, server randomly chooses one from other formats.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ACK\DC2\ETXF\STX\r\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETXF\SO\ESC\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETXF\RS\US\n\
    \D\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\ETXI\STX\SUB\SUB7 Desired payload size in the response from the server.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ENQ\DC2\ETXI\STX\a\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\ETXI\b\NAK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\ETXI\CAN\EM\n\
    \B\n\
    \\EOT\EOT\ETX\STX\STX\DC2\ETXL\STX\SYN\SUB5 Optional input payload sent along with the request.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ACK\DC2\ETXL\STX\t\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\SOH\DC2\ETXL\n\
    \\DC1\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ETX\DC2\ETXL\DC4\NAK\n\
    \>\n\
    \\EOT\EOT\ETX\STX\ETX\DC2\ETXO\STX\EM\SUB1 Whether SimpleResponse should include username.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\ENQ\DC2\ETXO\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\SOH\DC2\ETXO\a\DC4\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\ETX\DC2\ETXO\ETB\CAN\n\
    \A\n\
    \\EOT\EOT\ETX\STX\EOT\DC2\ETXR\STX\FS\SUB4 Whether SimpleResponse should include OAuth scope.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\ENQ\DC2\ETXR\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\SOH\DC2\ETXR\a\ETB\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\ETX\DC2\ETXR\SUB\ESC\n\
    \\140\STX\n\
    \\EOT\EOT\ETX\STX\ENQ\DC2\ETXX\STX$\SUB\254\SOH Whether to request the server to compress the response. This field is\n\
    \ \"nullable\" in order to interoperate seamlessly with clients not able to\n\
    \ implement the full compression tests by introspecting the call to verify\n\
    \ the response's compression status.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ENQ\ACK\DC2\ETXX\STX\v\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ENQ\SOH\DC2\ETXX\f\US\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ENQ\ETX\DC2\ETXX\"#\n\
    \:\n\
    \\EOT\EOT\ETX\STX\ACK\DC2\ETX[\STX!\SUB- Whether server should return a given status\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ACK\ACK\DC2\ETX[\STX\f\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ACK\SOH\DC2\ETX[\r\FS\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ACK\ETX\DC2\ETX[\US \n\
    \N\n\
    \\EOT\EOT\ETX\STX\a\DC2\ETX^\STX\"\SUBA Whether the server should expect this request to be compressed.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\a\ACK\DC2\ETX^\STX\v\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\a\SOH\DC2\ETX^\f\GS\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\a\ETX\DC2\ETX^ !\n\
    \?\n\
    \\EOT\EOT\ETX\STX\b\DC2\ETXa\STX\SUB\SUB2 Whether SimpleResponse should include server_id.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\b\ENQ\DC2\ETXa\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\b\SOH\DC2\ETXa\a\NAK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\b\ETX\DC2\ETXa\CAN\EM\n\
    \G\n\
    \\EOT\EOT\ETX\STX\t\DC2\ETXd\STX#\SUB: Whether SimpleResponse should include grpclb_route_type.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\t\ENQ\DC2\ETXd\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\t\SOH\DC2\ETXd\a\GS\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\t\ETX\DC2\ETXd \"\n\
    \\\\n\
    \\EOT\EOT\ETX\STX\n\
    \\DC2\ETXg\STX,\SUBO If set the server should record this metrics report data for the current RPC.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\n\
    \\ACK\DC2\ETXg\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\n\
    \\SOH\DC2\ETXg\DC1&\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\n\
    \\ETX\DC2\ETXg)+\n\
    \;\n\
    \\STX\EOT\EOT\DC2\EOTk\NUL|\SOH\SUB/ Unary response, as configured by the request.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETXk\b\SYN\n\
    \0\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETXm\STX\SYN\SUB# Payload to increase message size.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ACK\DC2\ETXm\STX\t\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETXm\n\
    \\DC1\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETXm\DC4\NAK\n\
    \x\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETXp\STX\SYN\SUBk The user the request came from, for verifying authentication was\n\
    \ successful when the client expected it.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ENQ\DC2\ETXp\STX\b\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETXp\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETXp\DC4\NAK\n\
    \\ESC\n\
    \\EOT\EOT\EOT\STX\STX\DC2\ETXr\STX\EM\SUB\SO OAuth scope.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ENQ\DC2\ETXr\STX\b\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\SOH\DC2\ETXr\t\DC4\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ETX\DC2\ETXr\ETB\CAN\n\
    \\149\SOH\n\
    \\EOT\EOT\EOT\STX\ETX\DC2\ETXv\STX\ETB\SUB\135\SOH Server ID. This must be unique among different server instances,\n\
    \ but the same across all RPC's made to a particular server instance.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\ENQ\DC2\ETXv\STX\b\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\SOH\DC2\ETXv\t\DC2\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\ETX\DC2\ETXv\NAK\SYN\n\
    \\ESC\n\
    \\EOT\EOT\EOT\STX\EOT\DC2\ETXx\STX(\SUB\SO gRPCLB Path.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\ACK\DC2\ETXx\STX\DC1\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\SOH\DC2\ETXx\DC2#\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\ETX\DC2\ETXx&'\n\
    \\US\n\
    \\EOT\EOT\EOT\STX\ENQ\DC2\ETX{\STX\SYN\SUB\DC2 Server hostname.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\ENQ\DC2\ETX{\STX\b\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\SOH\DC2\ETX{\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\ETX\DC2\ETX{\DC4\NAK\n\
    \(\n\
    \\STX\EOT\ENQ\DC2\ENQ\DEL\NUL\138\SOH\SOH\SUB\ESC Client-streaming request.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX\DEL\b!\n\
    \C\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\EOT\129\SOH\STX\SYN\SUB5 Optional input payload sent along with the request.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\NUL\ACK\DC2\EOT\129\SOH\STX\t\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\EOT\129\SOH\n\
    \\DC1\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\EOT\129\SOH\DC4\NAK\n\
    \\148\STX\n\
    \\EOT\EOT\ENQ\STX\SOH\DC2\EOT\135\SOH\STX\"\SUB\133\STX Whether the server should expect this request to be compressed. This field\n\
    \ is \"nullable\" in order to interoperate seamlessly with servers not able to\n\
    \ implement the full compression tests by introspecting the call to verify\n\
    \ the request's compression status.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\SOH\ACK\DC2\EOT\135\SOH\STX\v\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\SOH\SOH\DC2\EOT\135\SOH\f\GS\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\SOH\ETX\DC2\EOT\135\SOH !\n\
    \*\n\
    \\STX\EOT\ACK\DC2\ACK\141\SOH\NUL\144\SOH\SOH\SUB\FS Client-streaming response.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\ACK\SOH\DC2\EOT\141\SOH\b\"\n\
    \E\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\EOT\143\SOH\STX$\SUB7 Aggregated size of payloads received from the client.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\EOT\143\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\EOT\143\SOH\b\US\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\EOT\143\SOH\"#\n\
    \8\n\
    \\STX\EOT\a\DC2\ACK\147\SOH\NUL\160\SOH\SOH\SUB* Configuration for a particular response.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\a\SOH\DC2\EOT\147\SOH\b\SUB\n\
    \C\n\
    \\EOT\EOT\a\STX\NUL\DC2\EOT\149\SOH\STX\DC1\SUB5 Desired payload sizes in responses from the server.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\ENQ\DC2\EOT\149\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\EOT\149\SOH\b\f\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\EOT\149\SOH\SI\DLE\n\
    \g\n\
    \\EOT\EOT\a\STX\SOH\DC2\EOT\153\SOH\STX\CAN\SUBY Desired interval between consecutive responses in the response stream in\n\
    \ microseconds.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\ENQ\DC2\EOT\153\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\EOT\153\SOH\b\DC3\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\EOT\153\SOH\SYN\ETB\n\
    \\141\STX\n\
    \\EOT\EOT\a\STX\STX\DC2\EOT\159\SOH\STX\ESC\SUB\254\SOH Whether to request the server to compress the response. This field is\n\
    \ \"nullable\" in order to interoperate seamlessly with clients not able to\n\
    \ implement the full compression tests by introspecting the call to verify\n\
    \ the response's compression status.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\ACK\DC2\EOT\159\SOH\STX\v\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\SOH\DC2\EOT\159\SOH\f\SYN\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\ETX\DC2\EOT\159\SOH\EM\SUB\n\
    \)\n\
    \\STX\EOT\b\DC2\ACK\163\SOH\NUL\181\SOH\SOH\SUB\ESC Server-streaming request.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\b\SOH\DC2\EOT\163\SOH\b\"\n\
    \\227\SOH\n\
    \\EOT\EOT\b\STX\NUL\DC2\EOT\168\SOH\STX \SUB\212\SOH Desired payload type in the response from the server.\n\
    \ If response_type is RANDOM, the payload from each response in the stream\n\
    \ might be of different types. This is to simulate a mixed type of payload\n\
    \ stream.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\ACK\DC2\EOT\168\SOH\STX\r\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\EOT\168\SOH\SO\ESC\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\EOT\168\SOH\RS\US\n\
    \A\n\
    \\EOT\EOT\b\STX\SOH\DC2\EOT\171\SOH\STX6\SUB3 Configuration for each expected response message.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\EOT\DC2\EOT\171\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\ACK\DC2\EOT\171\SOH\v\GS\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\SOH\DC2\EOT\171\SOH\RS1\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\ETX\DC2\EOT\171\SOH45\n\
    \C\n\
    \\EOT\EOT\b\STX\STX\DC2\EOT\174\SOH\STX\SYN\SUB5 Optional input payload sent along with the request.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\ACK\DC2\EOT\174\SOH\STX\t\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\SOH\DC2\EOT\174\SOH\n\
    \\DC1\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\ETX\DC2\EOT\174\SOH\DC4\NAK\n\
    \;\n\
    \\EOT\EOT\b\STX\ETX\DC2\EOT\177\SOH\STX!\SUB- Whether server should return a given status\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\ETX\ACK\DC2\EOT\177\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\b\STX\ETX\SOH\DC2\EOT\177\SOH\r\FS\n\
    \\r\n\
    \\ENQ\EOT\b\STX\ETX\ETX\DC2\EOT\177\SOH\US \n\
    \[\n\
    \\EOT\EOT\b\STX\EOT\DC2\EOT\180\SOH\STX%\SUBM If set the server should update this metrics report data at the OOB server.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\EOT\ACK\DC2\EOT\180\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\b\STX\EOT\SOH\DC2\EOT\180\SOH\DC1 \n\
    \\r\n\
    \\ENQ\EOT\b\STX\EOT\ETX\DC2\EOT\180\SOH#$\n\
    \W\n\
    \\STX\EOT\t\DC2\ACK\184\SOH\NUL\187\SOH\SOH\SUBI Server-streaming response, as configured by the request and parameters.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\t\SOH\DC2\EOT\184\SOH\b#\n\
    \2\n\
    \\EOT\EOT\t\STX\NUL\DC2\EOT\186\SOH\STX\SYN\SUB$ Payload to increase response size.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\ACK\DC2\EOT\186\SOH\STX\t\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\EOT\186\SOH\n\
    \\DC1\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\EOT\186\SOH\DC4\NAK\n\
    \k\n\
    \\STX\EOT\n\
    \\DC2\ACK\191\SOH\NUL\193\SOH\SOH\SUB] For reconnect interop test only.\n\
    \ Client tells server what reconnection parameters it used.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\n\
    \\SOH\DC2\EOT\191\SOH\b\ETB\n\
    \\f\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\EOT\192\SOH\STX%\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ENQ\DC2\EOT\192\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\EOT\192\SOH\b \n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\EOT\192\SOH#$\n\
    \\152\SOH\n\
    \\STX\EOT\v\DC2\ACK\198\SOH\NUL\201\SOH\SOH\SUB\137\SOH For reconnect interop test only.\n\
    \ Server tells client whether its reconnects are following the spec and the\n\
    \ reconnect backoffs it saw.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\v\SOH\DC2\EOT\198\SOH\b\NAK\n\
    \\f\n\
    \\EOT\EOT\v\STX\NUL\DC2\EOT\199\SOH\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\v\STX\NUL\ENQ\DC2\EOT\199\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\EOT\199\SOH\a\r\n\
    \\r\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\EOT\199\SOH\DLE\DC1\n\
    \\f\n\
    \\EOT\EOT\v\STX\SOH\DC2\EOT\200\SOH\STX \n\
    \\r\n\
    \\ENQ\EOT\v\STX\SOH\EOT\DC2\EOT\200\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\v\STX\SOH\ENQ\DC2\EOT\200\SOH\v\DLE\n\
    \\r\n\
    \\ENQ\EOT\v\STX\SOH\SOH\DC2\EOT\200\SOH\DC1\ESC\n\
    \\r\n\
    \\ENQ\EOT\v\STX\SOH\ETX\DC2\EOT\200\SOH\RS\US\n\
    \\f\n\
    \\STX\EOT\f\DC2\ACK\203\SOH\NUL\212\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\f\SOH\DC2\EOT\203\SOH\b \n\
    \C\n\
    \\EOT\EOT\f\STX\NUL\DC2\EOT\205\SOH\STX\NAK\SUB5 Request stats for the next num_rpcs sent by client.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\f\STX\NUL\ENQ\DC2\EOT\205\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\f\STX\NUL\SOH\DC2\EOT\205\SOH\b\DLE\n\
    \\r\n\
    \\ENQ\EOT\f\STX\NUL\ETX\DC2\EOT\205\SOH\DC3\DC4\n\
    \Z\n\
    \\EOT\EOT\f\STX\SOH\DC2\EOT\207\SOH\STX\CAN\SUBL If num_rpcs have not completed within timeout_sec, return partial results.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\f\STX\SOH\ENQ\DC2\EOT\207\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\f\STX\SOH\SOH\DC2\EOT\207\SOH\b\DC3\n\
    \\r\n\
    \\ENQ\EOT\f\STX\SOH\ETX\DC2\EOT\207\SOH\SYN\ETB\n\
    \\224\SOH\n\
    \\EOT\EOT\f\STX\STX\DC2\EOT\211\SOH\STX$\SUB\209\SOH Response header + trailer metadata entries we want the values of.\n\
    \ Matching of the keys is case-insensitive as per rfc7540#section-8.1.2\n\
    \ * (asterisk) is a special value that will return all metadata entries\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\f\STX\STX\EOT\DC2\EOT\211\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\f\STX\STX\ENQ\DC2\EOT\211\SOH\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\f\STX\STX\SOH\DC2\EOT\211\SOH\DC2\US\n\
    \\r\n\
    \\ENQ\EOT\f\STX\STX\ETX\DC2\EOT\211\SOH\"#\n\
    \\f\n\
    \\STX\EOT\r\DC2\ACK\214\SOH\NUL\249\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\r\SOH\DC2\EOT\214\SOH\b!\n\
    \\SO\n\
    \\EOT\EOT\r\EOT\NUL\DC2\ACK\215\SOH\STX\219\SOH\ETX\n\
    \\r\n\
    \\ENQ\EOT\r\EOT\NUL\SOH\DC2\EOT\215\SOH\a\DC3\n\
    \\SO\n\
    \\ACK\EOT\r\EOT\NUL\STX\NUL\DC2\EOT\216\SOH\EOT\DLE\n\
    \\SI\n\
    \\a\EOT\r\EOT\NUL\STX\NUL\SOH\DC2\EOT\216\SOH\EOT\v\n\
    \\SI\n\
    \\a\EOT\r\EOT\NUL\STX\NUL\STX\DC2\EOT\216\SOH\SO\SI\n\
    \\SO\n\
    \\ACK\EOT\r\EOT\NUL\STX\SOH\DC2\EOT\217\SOH\EOT\DLE\n\
    \\SI\n\
    \\a\EOT\r\EOT\NUL\STX\SOH\SOH\DC2\EOT\217\SOH\EOT\v\n\
    \\SI\n\
    \\a\EOT\r\EOT\NUL\STX\SOH\STX\DC2\EOT\217\SOH\SO\SI\n\
    \\SO\n\
    \\ACK\EOT\r\EOT\NUL\STX\STX\DC2\EOT\218\SOH\EOT\DC1\n\
    \\SI\n\
    \\a\EOT\r\EOT\NUL\STX\STX\SOH\DC2\EOT\218\SOH\EOT\f\n\
    \\SI\n\
    \\a\EOT\r\EOT\NUL\STX\STX\STX\DC2\EOT\218\SOH\SI\DLE\n\
    \\SO\n\
    \\EOT\EOT\r\ETX\NUL\DC2\ACK\220\SOH\STX\228\SOH\ETX\n\
    \\r\n\
    \\ENQ\EOT\r\ETX\NUL\SOH\DC2\EOT\220\SOH\n\
    \\ETB\n\
    \\139\SOH\n\
    \\ACK\EOT\r\ETX\NUL\STX\NUL\DC2\EOT\223\SOH\EOT\DC3\SUB{ Key, exactly as received from the server. Case may be different from what\n\
    \ was requested in the LoadBalancerStatsRequest)\n\
    \\n\
    \\SI\n\
    \\a\EOT\r\ETX\NUL\STX\NUL\ENQ\DC2\EOT\223\SOH\EOT\n\
    \\n\
    \\SI\n\
    \\a\EOT\r\ETX\NUL\STX\NUL\SOH\DC2\EOT\223\SOH\v\SO\n\
    \\SI\n\
    \\a\EOT\r\ETX\NUL\STX\NUL\ETX\DC2\EOT\223\SOH\DC1\DC2\n\
    \=\n\
    \\ACK\EOT\r\ETX\NUL\STX\SOH\DC2\EOT\225\SOH\EOT\NAK\SUB- Value, exactly as received from the server.\n\
    \\n\
    \\SI\n\
    \\a\EOT\r\ETX\NUL\STX\SOH\ENQ\DC2\EOT\225\SOH\EOT\n\
    \\n\
    \\SI\n\
    \\a\EOT\r\ETX\NUL\STX\SOH\SOH\DC2\EOT\225\SOH\v\DLE\n\
    \\SI\n\
    \\a\EOT\r\ETX\NUL\STX\SOH\ETX\DC2\EOT\225\SOH\DC3\DC4\n\
    \\US\n\
    \\ACK\EOT\r\ETX\NUL\STX\STX\DC2\EOT\227\SOH\EOT\SUB\SUB\SI Metadata type\n\
    \\n\
    \\SI\n\
    \\a\EOT\r\ETX\NUL\STX\STX\ACK\DC2\EOT\227\SOH\EOT\DLE\n\
    \\SI\n\
    \\a\EOT\r\ETX\NUL\STX\STX\SOH\DC2\EOT\227\SOH\DC1\NAK\n\
    \\SI\n\
    \\a\EOT\r\ETX\NUL\STX\STX\ETX\DC2\EOT\227\SOH\CAN\EM\n\
    \\SO\n\
    \\EOT\EOT\r\ETX\SOH\DC2\ACK\229\SOH\STX\233\SOH\ETX\n\
    \\r\n\
    \\ENQ\EOT\r\ETX\SOH\SOH\DC2\EOT\229\SOH\n\
    \\NAK\n\
    \q\n\
    \\ACK\EOT\r\ETX\SOH\STX\NUL\DC2\EOT\232\SOH\EOT(\SUBa metadata values for each rpc for the keys specified in\n\
    \ LoadBalancerStatsRequest.metadata_keys.\n\
    \\n\
    \\SI\n\
    \\a\EOT\r\ETX\SOH\STX\NUL\EOT\DC2\EOT\232\SOH\EOT\f\n\
    \\SI\n\
    \\a\EOT\r\ETX\SOH\STX\NUL\ACK\DC2\EOT\232\SOH\r\SUB\n\
    \\SI\n\
    \\a\EOT\r\ETX\SOH\STX\NUL\SOH\DC2\EOT\232\SOH\ESC#\n\
    \\SI\n\
    \\a\EOT\r\ETX\SOH\STX\NUL\ETX\DC2\EOT\232\SOH&'\n\
    \\SO\n\
    \\EOT\EOT\r\ETX\STX\DC2\ACK\234\SOH\STX\237\SOH\ETX\n\
    \\r\n\
    \\ENQ\EOT\r\ETX\STX\SOH\DC2\EOT\234\SOH\n\
    \\CAN\n\
    \G\n\
    \\ACK\EOT\r\ETX\STX\STX\NUL\DC2\EOT\236\SOH\EOT*\SUB7 List of RpcMetadata in for each RPC with a given peer\n\
    \\n\
    \\SI\n\
    \\a\EOT\r\ETX\STX\STX\NUL\EOT\DC2\EOT\236\SOH\EOT\f\n\
    \\SI\n\
    \\a\EOT\r\ETX\STX\STX\NUL\ACK\DC2\EOT\236\SOH\r\CAN\n\
    \\SI\n\
    \\a\EOT\r\ETX\STX\STX\NUL\SOH\DC2\EOT\236\SOH\EM%\n\
    \\SI\n\
    \\a\EOT\r\ETX\STX\STX\NUL\ETX\DC2\EOT\236\SOH()\n\
    \\SO\n\
    \\EOT\EOT\r\ETX\ETX\DC2\ACK\238\SOH\STX\241\SOH\ETX\n\
    \\r\n\
    \\ENQ\EOT\r\ETX\ETX\SOH\DC2\EOT\238\SOH\n\
    \\DC4\n\
    \=\n\
    \\ACK\EOT\r\ETX\ETX\STX\NUL\DC2\EOT\240\SOH\EOT(\SUB- The number of completed RPCs for each peer.\n\
    \\n\
    \\SI\n\
    \\a\EOT\r\ETX\ETX\STX\NUL\ACK\DC2\EOT\240\SOH\EOT\SYN\n\
    \\SI\n\
    \\a\EOT\r\ETX\ETX\STX\NUL\SOH\DC2\EOT\240\SOH\ETB#\n\
    \\SI\n\
    \\a\EOT\r\ETX\ETX\STX\NUL\ETX\DC2\EOT\240\SOH&'\n\
    \;\n\
    \\EOT\EOT\r\STX\NUL\DC2\EOT\243\SOH\STX&\SUB- The number of completed RPCs for each peer.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\r\STX\NUL\ACK\DC2\EOT\243\SOH\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\r\STX\NUL\SOH\DC2\EOT\243\SOH\NAK!\n\
    \\r\n\
    \\ENQ\EOT\r\STX\NUL\ETX\DC2\EOT\243\SOH$%\n\
    \G\n\
    \\EOT\EOT\r\STX\SOH\DC2\EOT\245\SOH\STX\EM\SUB9 The number of RPCs that failed to record a remote peer.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\r\STX\SOH\ENQ\DC2\EOT\245\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\r\STX\SOH\SOH\DC2\EOT\245\SOH\b\DC4\n\
    \\r\n\
    \\ENQ\EOT\r\STX\SOH\ETX\DC2\EOT\245\SOH\ETB\CAN\n\
    \\f\n\
    \\EOT\EOT\r\STX\STX\DC2\EOT\246\SOH\STX-\n\
    \\r\n\
    \\ENQ\EOT\r\STX\STX\ACK\DC2\EOT\246\SOH\STX\EM\n\
    \\r\n\
    \\ENQ\EOT\r\STX\STX\SOH\DC2\EOT\246\SOH\SUB(\n\
    \\r\n\
    \\ENQ\EOT\r\STX\STX\ETX\DC2\EOT\246\SOH+,\n\
    \;\n\
    \\EOT\EOT\r\STX\ETX\DC2\EOT\248\SOH\STX4\SUB- All the metadata of all RPCs for each peer.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\r\STX\ETX\ACK\DC2\EOT\248\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\r\STX\ETX\SOH\DC2\EOT\248\SOH\RS/\n\
    \\r\n\
    \\ENQ\EOT\r\STX\ETX\ETX\DC2\EOT\248\SOH23\n\
    \G\n\
    \\STX\EOT\SO\DC2\EOT\252\SOH\NUL.\SUB; Request for retrieving a test client's accumulated stats.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\SO\SOH\DC2\EOT\252\SOH\b+\n\
    \A\n\
    \\STX\EOT\SI\DC2\ACK\255\SOH\NUL\150\STX\SOH\SUB3 Accumulated stats for RPCs sent by a test client.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\SI\SOH\DC2\EOT\255\SOH\b,\n\
    \\128\SOH\n\
    \\EOT\EOT\SI\STX\NUL\DC2\EOT\130\STX\STXH\SUBr The total number of RPCs have ever issued for each type.\n\
    \ Deprecated: use stats_per_method.rpcs_started instead.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\NUL\ACK\DC2\EOT\130\STX\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\NUL\SOH\DC2\EOT\130\STX\NAK/\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\NUL\ETX\DC2\EOT\130\STX23\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\NUL\b\DC2\EOT\130\STX4G\n\
    \\SO\n\
    \\ACK\EOT\SI\STX\NUL\b\ETX\DC2\EOT\130\STX5F\n\
    \\138\SOH\n\
    \\EOT\EOT\SI\STX\SOH\DC2\EOT\133\STX\STXJ\SUB| The total number of RPCs have ever completed successfully for each type.\n\
    \ Deprecated: use stats_per_method.result instead.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\SOH\ACK\DC2\EOT\133\STX\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\SOH\SOH\DC2\EOT\133\STX\NAK1\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\SOH\ETX\DC2\EOT\133\STX45\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\SOH\b\DC2\EOT\133\STX6I\n\
    \\SO\n\
    \\ACK\EOT\SI\STX\SOH\b\ETX\DC2\EOT\133\STX7H\n\
    \z\n\
    \\EOT\EOT\SI\STX\STX\DC2\EOT\136\STX\STXG\SUBl The total number of RPCs have ever failed for each type.\n\
    \ Deprecated: use stats_per_method.result instead.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\STX\ACK\DC2\EOT\136\STX\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\STX\SOH\DC2\EOT\136\STX\NAK.\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\STX\ETX\DC2\EOT\136\STX12\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\STX\b\DC2\EOT\136\STX3F\n\
    \\SO\n\
    \\ACK\EOT\SI\STX\STX\b\ETX\DC2\EOT\136\STX4E\n\
    \\SO\n\
    \\EOT\EOT\SI\ETX\ETX\DC2\ACK\138\STX\STX\145\STX\ETX\n\
    \\r\n\
    \\ENQ\EOT\SI\ETX\ETX\SOH\DC2\EOT\138\STX\n\
    \\NAK\n\
    \G\n\
    \\ACK\EOT\SI\ETX\ETX\STX\NUL\DC2\EOT\140\STX\EOT\ESC\SUB7 The number of RPCs that were started for this method.\n\
    \\n\
    \\SI\n\
    \\a\EOT\SI\ETX\ETX\STX\NUL\ENQ\DC2\EOT\140\STX\EOT\t\n\
    \\SI\n\
    \\a\EOT\SI\ETX\ETX\STX\NUL\SOH\DC2\EOT\140\STX\n\
    \\SYN\n\
    \\SI\n\
    \\a\EOT\SI\ETX\ETX\STX\NUL\ETX\DC2\EOT\140\STX\EM\SUB\n\
    \\164\SOH\n\
    \\ACK\EOT\SI\ETX\ETX\STX\SOH\DC2\EOT\144\STX\EOT!\SUB\147\SOH The number of RPCs that completed with each status for this method.  The\n\
    \ key is the integral value of a google.rpc.Code; the value is the count.\n\
    \\n\
    \\SI\n\
    \\a\EOT\SI\ETX\ETX\STX\SOH\ACK\DC2\EOT\144\STX\EOT\NAK\n\
    \\SI\n\
    \\a\EOT\SI\ETX\ETX\STX\SOH\SOH\DC2\EOT\144\STX\SYN\FS\n\
    \\SI\n\
    \\a\EOT\SI\ETX\ETX\STX\SOH\ETX\DC2\EOT\144\STX\US \n\
    \u\n\
    \\EOT\EOT\SI\STX\ETX\DC2\EOT\149\STX\STX0\SUBg Per-method RPC statistics.  The key is the RpcType in string form; e.g.\n\
    \ 'EMPTY_CALL' or 'UNARY_CALL'\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\ETX\ACK\DC2\EOT\149\STX\STX\SUB\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\ETX\SOH\DC2\EOT\149\STX\ESC+\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\ETX\ETX\DC2\EOT\149\STX./\n\
    \1\n\
    \\STX\EOT\DLE\DC2\ACK\153\STX\NUL\174\STX\SOH\SUB# Configurations for a test client.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\DLE\SOH\DC2\EOT\153\STX\b\RS\n\
    \'\n\
    \\EOT\EOT\DLE\EOT\NUL\DC2\ACK\155\STX\STX\158\STX\ETX\SUB\ETB Type of RPCs to send.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DLE\EOT\NUL\SOH\DC2\EOT\155\STX\a\SO\n\
    \\SO\n\
    \\ACK\EOT\DLE\EOT\NUL\STX\NUL\DC2\EOT\156\STX\EOT\DC3\n\
    \\SI\n\
    \\a\EOT\DLE\EOT\NUL\STX\NUL\SOH\DC2\EOT\156\STX\EOT\SO\n\
    \\SI\n\
    \\a\EOT\DLE\EOT\NUL\STX\NUL\STX\DC2\EOT\156\STX\DC1\DC2\n\
    \\SO\n\
    \\ACK\EOT\DLE\EOT\NUL\STX\SOH\DC2\EOT\157\STX\EOT\DC3\n\
    \\SI\n\
    \\a\EOT\DLE\EOT\NUL\STX\SOH\SOH\DC2\EOT\157\STX\EOT\SO\n\
    \\SI\n\
    \\a\EOT\DLE\EOT\NUL\STX\SOH\STX\DC2\EOT\157\STX\DC1\DC2\n\
    \E\n\
    \\EOT\EOT\DLE\ETX\NUL\DC2\ACK\161\STX\STX\165\STX\ETX\SUB5 Metadata to be attached for the given type of RPCs.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DLE\ETX\NUL\SOH\DC2\EOT\161\STX\n\
    \\DC2\n\
    \\SO\n\
    \\ACK\EOT\DLE\ETX\NUL\STX\NUL\DC2\EOT\162\STX\EOT\NAK\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\NUL\ACK\DC2\EOT\162\STX\EOT\v\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\NUL\SOH\DC2\EOT\162\STX\f\DLE\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\NUL\ETX\DC2\EOT\162\STX\DC3\DC4\n\
    \\SO\n\
    \\ACK\EOT\DLE\ETX\NUL\STX\SOH\DC2\EOT\163\STX\EOT\DC3\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\SOH\ENQ\DC2\EOT\163\STX\EOT\n\
    \\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\SOH\SOH\DC2\EOT\163\STX\v\SO\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\SOH\ETX\DC2\EOT\163\STX\DC1\DC2\n\
    \\SO\n\
    \\ACK\EOT\DLE\ETX\NUL\STX\STX\DC2\EOT\164\STX\EOT\NAK\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\STX\ENQ\DC2\EOT\164\STX\EOT\n\
    \\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\STX\SOH\DC2\EOT\164\STX\v\DLE\n\
    \\SI\n\
    \\a\EOT\DLE\ETX\NUL\STX\STX\ETX\DC2\EOT\164\STX\DC3\DC4\n\
    \3\n\
    \\EOT\EOT\DLE\STX\NUL\DC2\EOT\168\STX\STX\GS\SUB% The types of RPCs the client sends.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\EOT\DC2\EOT\168\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\ACK\DC2\EOT\168\STX\v\DC2\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\SOH\DC2\EOT\168\STX\DC3\CAN\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\ETX\DC2\EOT\168\STX\ESC\FS\n\
    \\\\n\
    \\EOT\EOT\DLE\STX\SOH\DC2\EOT\170\STX\STX!\SUBN The collection of custom metadata to be attached to RPCs sent by the client.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\EOT\DC2\EOT\170\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\ACK\DC2\EOT\170\STX\v\DC3\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\SOH\DC2\EOT\170\STX\DC4\FS\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\ETX\DC2\EOT\170\STX\US \n\
    \\137\SOH\n\
    \\EOT\EOT\DLE\STX\STX\DC2\EOT\173\STX\STX\CAN\SUB{ The deadline to use, in seconds, for all RPCs.  If unset or zero, the\n\
    \ client will use the default from the command-line.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\STX\ENQ\DC2\EOT\173\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\STX\SOH\DC2\EOT\173\STX\b\DC3\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\STX\ETX\DC2\EOT\173\STX\SYN\ETB\n\
    \B\n\
    \\STX\EOT\DC1\DC2\EOT\177\STX\NUL\"\SUB6 Response for updating a test client's configuration.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\DC1\SOH\DC2\EOT\177\STX\b\US\n\
    \\f\n\
    \\STX\EOT\DC2\DC2\ACK\179\STX\NUL\181\STX\SOH\n\
    \\v\n\
    \\ETX\EOT\DC2\SOH\DC2\EOT\179\STX\b\DC2\n\
    \\f\n\
    \\EOT\EOT\DC2\STX\NUL\DC2\EOT\180\STX\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\ENQ\DC2\EOT\180\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\SOH\DC2\EOT\180\STX\b\v\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\ETX\DC2\EOT\180\STX\SO\SI\n\
    \\181\STX\n\
    \\STX\EOT\DC3\DC2\ACK\186\STX\NUL\191\STX\SOH\SUB\166\STX Metrics data the server will update and send to the client. It mirrors orca load report\n\
    \ https://github.com/cncf/xds/blob/eded343319d09f30032952beda9840bbd3dcf7ac/xds/data/orca/v3/orca_load_report.proto#L15,\n\
    \ but avoids orca dependency. Used by both per-query and out-of-band reporting tests.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\DC3\SOH\DC2\EOT\186\STX\b\SYN\n\
    \\f\n\
    \\EOT\EOT\DC3\STX\NUL\DC2\EOT\187\STX\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\ENQ\DC2\EOT\187\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\SOH\DC2\EOT\187\STX\t\CAN\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\ETX\DC2\EOT\187\STX\ESC\FS\n\
    \\f\n\
    \\EOT\EOT\DC3\STX\SOH\DC2\EOT\188\STX\STX \n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\SOH\ENQ\DC2\EOT\188\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\SOH\SOH\DC2\EOT\188\STX\t\ESC\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\SOH\ETX\DC2\EOT\188\STX\RS\US\n\
    \\f\n\
    \\EOT\EOT\DC3\STX\STX\DC2\EOT\189\STX\STX'\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\STX\ACK\DC2\EOT\189\STX\STX\NAK\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\STX\SOH\DC2\EOT\189\STX\SYN\"\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\STX\ETX\DC2\EOT\189\STX%&\n\
    \\f\n\
    \\EOT\EOT\DC3\STX\ETX\DC2\EOT\190\STX\STX&\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\ETX\ACK\DC2\EOT\190\STX\STX\NAK\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\ETX\SOH\DC2\EOT\190\STX\SYN!\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\ETX\ETX\DC2\EOT\190\STX$%\n\
    \H\n\
    \\STX\EOT\DC4\DC2\ACK\194\STX\NUL\197\STX\SOH\SUB: Status that will be return to callers of the Hook method\n\
    \\n\
    \\v\n\
    \\ETX\EOT\DC4\SOH\DC2\EOT\194\STX\b\RS\n\
    \\f\n\
    \\EOT\EOT\DC4\STX\NUL\DC2\EOT\195\STX\STX \n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\ENQ\DC2\EOT\195\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\SOH\DC2\EOT\195\STX\b\ESC\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\ETX\DC2\EOT\195\STX\RS\US\n\
    \\f\n\
    \\EOT\EOT\DC4\STX\SOH\DC2\EOT\196\STX\STX%\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\ENQ\DC2\EOT\196\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\SOH\DC2\EOT\196\STX\t \n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\ETX\DC2\EOT\196\STX#$\n\
    \\f\n\
    \\STX\EOT\NAK\DC2\ACK\199\STX\NUL\215\STX\SOH\n\
    \\v\n\
    \\ETX\EOT\NAK\SOH\DC2\EOT\199\STX\b\DC3\n\
    \\SO\n\
    \\EOT\EOT\NAK\EOT\NUL\DC2\ACK\200\STX\STX\209\STX\ETX\n\
    \\r\n\
    \\ENQ\EOT\NAK\EOT\NUL\SOH\DC2\EOT\200\STX\a\EM\n\
    \\US\n\
    \\ACK\EOT\NAK\EOT\NUL\STX\NUL\DC2\EOT\202\STX\EOT\DC4\SUB\SI Default value\n\
    \\n\
    \\SI\n\
    \\a\EOT\NAK\EOT\NUL\STX\NUL\SOH\DC2\EOT\202\STX\EOT\SI\n\
    \\SI\n\
    \\a\EOT\NAK\EOT\NUL\STX\NUL\STX\DC2\EOT\202\STX\DC2\DC3\n\
    \)\n\
    \\ACK\EOT\NAK\EOT\NUL\STX\SOH\DC2\EOT\204\STX\EOT\SO\SUB\EM Start the HTTP endpoint\n\
    \\n\
    \\SI\n\
    \\a\EOT\NAK\EOT\NUL\STX\SOH\SOH\DC2\EOT\204\STX\EOT\t\n\
    \\SI\n\
    \\a\EOT\NAK\EOT\NUL\STX\SOH\STX\DC2\EOT\204\STX\f\r\n\
    \\SYN\n\
    \\ACK\EOT\NAK\EOT\NUL\STX\STX\DC2\EOT\206\STX\EOT\r\SUB\ACK Stop\n\
    \\n\
    \\SI\n\
    \\a\EOT\NAK\EOT\NUL\STX\STX\SOH\DC2\EOT\206\STX\EOT\b\n\
    \\SI\n\
    \\a\EOT\NAK\EOT\NUL\STX\STX\STX\DC2\EOT\206\STX\v\f\n\
    \+\n\
    \\ACK\EOT\NAK\EOT\NUL\STX\ETX\DC2\EOT\208\STX\EOT\SI\SUB\ESC Return from HTTP GET/POST\n\
    \\n\
    \\SI\n\
    \\a\EOT\NAK\EOT\NUL\STX\ETX\SOH\DC2\EOT\208\STX\EOT\n\
    \\n\
    \\SI\n\
    \\a\EOT\NAK\EOT\NUL\STX\ETX\STX\DC2\EOT\208\STX\r\SO\n\
    \\f\n\
    \\EOT\EOT\NAK\STX\NUL\DC2\EOT\210\STX\STX!\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\ACK\DC2\EOT\210\STX\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\SOH\DC2\EOT\210\STX\NAK\FS\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\ETX\DC2\EOT\210\STX\US \n\
    \\f\n\
    \\EOT\EOT\NAK\STX\SOH\DC2\EOT\211\STX\STX \n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\SOH\ENQ\DC2\EOT\211\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\SOH\SOH\DC2\EOT\211\STX\b\ESC\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\SOH\ETX\DC2\EOT\211\STX\RS\US\n\
    \\f\n\
    \\EOT\EOT\NAK\STX\STX\DC2\EOT\212\STX\STX%\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\STX\ENQ\DC2\EOT\212\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\STX\SOH\DC2\EOT\212\STX\t \n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\STX\ETX\DC2\EOT\212\STX#$\n\
    \(\n\
    \\EOT\EOT\NAK\STX\ETX\DC2\EOT\214\STX\STX\CAN\SUB\SUB Server port to listen to\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ETX\ENQ\DC2\EOT\214\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ETX\SOH\DC2\EOT\214\STX\b\DC3\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ETX\ETX\DC2\EOT\214\STX\SYN\ETB\n\
    \\f\n\
    \\STX\EOT\SYN\DC2\ACK\217\STX\NUL\218\STX\SOH\n\
    \\v\n\
    \\ETX\EOT\SYN\SOH\DC2\EOT\217\STX\b\DC4b\ACKproto3"