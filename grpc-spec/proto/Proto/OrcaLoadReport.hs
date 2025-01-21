{-# OPTIONS_GHC -Wno-prepositive-qualified-module -Wno-identities #-}
{- This file was auto-generated from orca_load_report.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.OrcaLoadReport (
        OrcaLoadReport(), OrcaLoadReport'NamedMetricsEntry(),
        OrcaLoadReport'RequestCostEntry(),
        OrcaLoadReport'UtilizationEntry()
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
     
         * 'Proto.OrcaLoadReport_Fields.cpuUtilization' @:: Lens' OrcaLoadReport Prelude.Double@
         * 'Proto.OrcaLoadReport_Fields.memUtilization' @:: Lens' OrcaLoadReport Prelude.Double@
         * 'Proto.OrcaLoadReport_Fields.rps' @:: Lens' OrcaLoadReport Data.Word.Word64@
         * 'Proto.OrcaLoadReport_Fields.requestCost' @:: Lens' OrcaLoadReport (Data.Map.Map Data.Text.Text Prelude.Double)@
         * 'Proto.OrcaLoadReport_Fields.utilization' @:: Lens' OrcaLoadReport (Data.Map.Map Data.Text.Text Prelude.Double)@
         * 'Proto.OrcaLoadReport_Fields.rpsFractional' @:: Lens' OrcaLoadReport Prelude.Double@
         * 'Proto.OrcaLoadReport_Fields.eps' @:: Lens' OrcaLoadReport Prelude.Double@
         * 'Proto.OrcaLoadReport_Fields.namedMetrics' @:: Lens' OrcaLoadReport (Data.Map.Map Data.Text.Text Prelude.Double)@
         * 'Proto.OrcaLoadReport_Fields.applicationUtilization' @:: Lens' OrcaLoadReport Prelude.Double@ -}
data OrcaLoadReport
  = OrcaLoadReport'_constructor {_OrcaLoadReport'cpuUtilization :: !Prelude.Double,
                                 _OrcaLoadReport'memUtilization :: !Prelude.Double,
                                 _OrcaLoadReport'rps :: !Data.Word.Word64,
                                 _OrcaLoadReport'requestCost :: !(Data.Map.Map Data.Text.Text Prelude.Double),
                                 _OrcaLoadReport'utilization :: !(Data.Map.Map Data.Text.Text Prelude.Double),
                                 _OrcaLoadReport'rpsFractional :: !Prelude.Double,
                                 _OrcaLoadReport'eps :: !Prelude.Double,
                                 _OrcaLoadReport'namedMetrics :: !(Data.Map.Map Data.Text.Text Prelude.Double),
                                 _OrcaLoadReport'applicationUtilization :: !Prelude.Double,
                                 _OrcaLoadReport'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show OrcaLoadReport where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField OrcaLoadReport "cpuUtilization" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'cpuUtilization
           (\ x__ y__ -> x__ {_OrcaLoadReport'cpuUtilization = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField OrcaLoadReport "memUtilization" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'memUtilization
           (\ x__ y__ -> x__ {_OrcaLoadReport'memUtilization = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField OrcaLoadReport "rps" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'rps (\ x__ y__ -> x__ {_OrcaLoadReport'rps = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField OrcaLoadReport "requestCost" (Data.Map.Map Data.Text.Text Prelude.Double) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'requestCost
           (\ x__ y__ -> x__ {_OrcaLoadReport'requestCost = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField OrcaLoadReport "utilization" (Data.Map.Map Data.Text.Text Prelude.Double) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'utilization
           (\ x__ y__ -> x__ {_OrcaLoadReport'utilization = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField OrcaLoadReport "rpsFractional" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'rpsFractional
           (\ x__ y__ -> x__ {_OrcaLoadReport'rpsFractional = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField OrcaLoadReport "eps" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'eps (\ x__ y__ -> x__ {_OrcaLoadReport'eps = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField OrcaLoadReport "namedMetrics" (Data.Map.Map Data.Text.Text Prelude.Double) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'namedMetrics
           (\ x__ y__ -> x__ {_OrcaLoadReport'namedMetrics = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField OrcaLoadReport "applicationUtilization" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'applicationUtilization
           (\ x__ y__ -> x__ {_OrcaLoadReport'applicationUtilization = y__}))
        Prelude.id
instance Data.ProtoLens.Message OrcaLoadReport where
  messageName _ = Data.Text.pack "xds.data.orca.v3.OrcaLoadReport"
  packedMessageDescriptor _
    = "\n\
      \\SOOrcaLoadReport\DC2'\n\
      \\SIcpu_utilization\CAN\SOH \SOH(\SOHR\SOcpuUtilization\DC2'\n\
      \\SImem_utilization\CAN\STX \SOH(\SOHR\SOmemUtilization\DC2\DC4\n\
      \\ETXrps\CAN\ETX \SOH(\EOTR\ETXrpsB\STX\CAN\SOH\DC2T\n\
      \\frequest_cost\CAN\EOT \ETX(\v21.xds.data.orca.v3.OrcaLoadReport.RequestCostEntryR\vrequestCost\DC2S\n\
      \\vutilization\CAN\ENQ \ETX(\v21.xds.data.orca.v3.OrcaLoadReport.UtilizationEntryR\vutilization\DC2%\n\
      \\SOrps_fractional\CAN\ACK \SOH(\SOHR\rrpsFractional\DC2\DLE\n\
      \\ETXeps\CAN\a \SOH(\SOHR\ETXeps\DC2W\n\
      \\rnamed_metrics\CAN\b \ETX(\v22.xds.data.orca.v3.OrcaLoadReport.NamedMetricsEntryR\fnamedMetrics\DC27\n\
      \\ETBapplication_utilization\CAN\t \SOH(\SOHR\SYNapplicationUtilization\SUB>\n\
      \\DLERequestCostEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOH\SUB>\n\
      \\DLEUtilizationEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOH\SUB?\n\
      \\DC1NamedMetricsEntry\DC2\DLE\n\
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
              Data.ProtoLens.FieldDescriptor OrcaLoadReport
        memUtilization__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mem_utilization"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"memUtilization")) ::
              Data.ProtoLens.FieldDescriptor OrcaLoadReport
        rps__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rps"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"rps")) ::
              Data.ProtoLens.FieldDescriptor OrcaLoadReport
        requestCost__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "request_cost"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor OrcaLoadReport'RequestCostEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"requestCost")) ::
              Data.ProtoLens.FieldDescriptor OrcaLoadReport
        utilization__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "utilization"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor OrcaLoadReport'UtilizationEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"utilization")) ::
              Data.ProtoLens.FieldDescriptor OrcaLoadReport
        rpsFractional__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rps_fractional"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"rpsFractional")) ::
              Data.ProtoLens.FieldDescriptor OrcaLoadReport
        eps__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "eps"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"eps")) ::
              Data.ProtoLens.FieldDescriptor OrcaLoadReport
        namedMetrics__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "named_metrics"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor OrcaLoadReport'NamedMetricsEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"namedMetrics")) ::
              Data.ProtoLens.FieldDescriptor OrcaLoadReport
        applicationUtilization__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "application_utilization"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"applicationUtilization")) ::
              Data.ProtoLens.FieldDescriptor OrcaLoadReport
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, cpuUtilization__field_descriptor),
           (Data.ProtoLens.Tag 2, memUtilization__field_descriptor),
           (Data.ProtoLens.Tag 3, rps__field_descriptor),
           (Data.ProtoLens.Tag 4, requestCost__field_descriptor),
           (Data.ProtoLens.Tag 5, utilization__field_descriptor),
           (Data.ProtoLens.Tag 6, rpsFractional__field_descriptor),
           (Data.ProtoLens.Tag 7, eps__field_descriptor),
           (Data.ProtoLens.Tag 8, namedMetrics__field_descriptor),
           (Data.ProtoLens.Tag 9, applicationUtilization__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _OrcaLoadReport'_unknownFields
        (\ x__ y__ -> x__ {_OrcaLoadReport'_unknownFields = y__})
  defMessage
    = OrcaLoadReport'_constructor
        {_OrcaLoadReport'cpuUtilization = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'memUtilization = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'rps = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'requestCost = Data.Map.empty,
         _OrcaLoadReport'utilization = Data.Map.empty,
         _OrcaLoadReport'rpsFractional = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'eps = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'namedMetrics = Data.Map.empty,
         _OrcaLoadReport'applicationUtilization = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'_unknownFields = []}
  parseMessage
    = let
        loop ::
          OrcaLoadReport
          -> Data.ProtoLens.Encoding.Bytes.Parser OrcaLoadReport
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
                                       "mem_utilization"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"memUtilization") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "rps"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"rps") y x)
                        34
                          -> do !(entry :: OrcaLoadReport'RequestCostEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
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
                        42
                          -> do !(entry :: OrcaLoadReport'UtilizationEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
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
                        49
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToDouble
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "rps_fractional"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"rpsFractional") y x)
                        57
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToDouble
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "eps"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"eps") y x)
                        66
                          -> do !(entry :: OrcaLoadReport'NamedMetricsEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                                  (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                      Data.ProtoLens.Encoding.Bytes.isolate
                                                                                        (Prelude.fromIntegral
                                                                                           len)
                                                                                        Data.ProtoLens.parseMessage)
                                                                                  "named_metrics"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"namedMetrics")
                                        (\ !t -> Data.Map.insert key value t) x))
                        73
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToDouble
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "application_utilization"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"applicationUtilization") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "OrcaLoadReport"
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
                         (Data.ProtoLens.Field.field @"memUtilization") _x
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
                   (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"rps") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
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
                                                OrcaLoadReport'RequestCostEntry)))))
                            (Data.Map.toList
                               (Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"requestCost") _x))))
                      ((Data.Monoid.<>)
                         (Data.Monoid.mconcat
                            (Prelude.map
                               (\ _v
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
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
                                                (Data.ProtoLens.Field.field @"value")
                                                (Prelude.snd _v)
                                                (Data.ProtoLens.defMessage ::
                                                   OrcaLoadReport'UtilizationEntry)))))
                               (Data.Map.toList
                                  (Lens.Family2.view
                                     (Data.ProtoLens.Field.field @"utilization") _x))))
                         ((Data.Monoid.<>)
                            (let
                               _v
                                 = Lens.Family2.view
                                     (Data.ProtoLens.Field.field @"rpsFractional") _x
                             in
                               if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 49)
                                     ((Prelude..)
                                        Data.ProtoLens.Encoding.Bytes.putFixed64
                                        Data.ProtoLens.Encoding.Bytes.doubleToWord _v))
                            ((Data.Monoid.<>)
                               (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"eps") _x
                                in
                                  if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                      Data.Monoid.mempty
                                  else
                                      (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 57)
                                        ((Prelude..)
                                           Data.ProtoLens.Encoding.Bytes.putFixed64
                                           Data.ProtoLens.Encoding.Bytes.doubleToWord _v))
                               ((Data.Monoid.<>)
                                  (Data.Monoid.mconcat
                                     (Prelude.map
                                        (\ _v
                                           -> (Data.Monoid.<>)
                                                (Data.ProtoLens.Encoding.Bytes.putVarInt 66)
                                                ((Prelude..)
                                                   (\ bs
                                                      -> (Data.Monoid.<>)
                                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                              (Prelude.fromIntegral
                                                                 (Data.ByteString.length bs)))
                                                           (Data.ProtoLens.Encoding.Bytes.putBytes
                                                              bs))
                                                   Data.ProtoLens.encodeMessage
                                                   (Lens.Family2.set
                                                      (Data.ProtoLens.Field.field @"key")
                                                      (Prelude.fst _v)
                                                      (Lens.Family2.set
                                                         (Data.ProtoLens.Field.field @"value")
                                                         (Prelude.snd _v)
                                                         (Data.ProtoLens.defMessage ::
                                                            OrcaLoadReport'NamedMetricsEntry)))))
                                        (Data.Map.toList
                                           (Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"namedMetrics") _x))))
                                  ((Data.Monoid.<>)
                                     (let
                                        _v
                                          = Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"applicationUtilization")
                                              _x
                                      in
                                        if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                            Data.Monoid.mempty
                                        else
                                            (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt 73)
                                              ((Prelude..)
                                                 Data.ProtoLens.Encoding.Bytes.putFixed64
                                                 Data.ProtoLens.Encoding.Bytes.doubleToWord _v))
                                     (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                        (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))))))
instance Control.DeepSeq.NFData OrcaLoadReport where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_OrcaLoadReport'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_OrcaLoadReport'cpuUtilization x__)
                (Control.DeepSeq.deepseq
                   (_OrcaLoadReport'memUtilization x__)
                   (Control.DeepSeq.deepseq
                      (_OrcaLoadReport'rps x__)
                      (Control.DeepSeq.deepseq
                         (_OrcaLoadReport'requestCost x__)
                         (Control.DeepSeq.deepseq
                            (_OrcaLoadReport'utilization x__)
                            (Control.DeepSeq.deepseq
                               (_OrcaLoadReport'rpsFractional x__)
                               (Control.DeepSeq.deepseq
                                  (_OrcaLoadReport'eps x__)
                                  (Control.DeepSeq.deepseq
                                     (_OrcaLoadReport'namedMetrics x__)
                                     (Control.DeepSeq.deepseq
                                        (_OrcaLoadReport'applicationUtilization x__) ())))))))))
{- | Fields :
     
         * 'Proto.OrcaLoadReport_Fields.key' @:: Lens' OrcaLoadReport'NamedMetricsEntry Data.Text.Text@
         * 'Proto.OrcaLoadReport_Fields.value' @:: Lens' OrcaLoadReport'NamedMetricsEntry Prelude.Double@ -}
data OrcaLoadReport'NamedMetricsEntry
  = OrcaLoadReport'NamedMetricsEntry'_constructor {_OrcaLoadReport'NamedMetricsEntry'key :: !Data.Text.Text,
                                                   _OrcaLoadReport'NamedMetricsEntry'value :: !Prelude.Double,
                                                   _OrcaLoadReport'NamedMetricsEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show OrcaLoadReport'NamedMetricsEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField OrcaLoadReport'NamedMetricsEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'NamedMetricsEntry'key
           (\ x__ y__ -> x__ {_OrcaLoadReport'NamedMetricsEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField OrcaLoadReport'NamedMetricsEntry "value" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'NamedMetricsEntry'value
           (\ x__ y__ -> x__ {_OrcaLoadReport'NamedMetricsEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message OrcaLoadReport'NamedMetricsEntry where
  messageName _
    = Data.Text.pack
        "xds.data.orca.v3.OrcaLoadReport.NamedMetricsEntry"
  packedMessageDescriptor _
    = "\n\
      \\DC1NamedMetricsEntry\DC2\DLE\n\
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
              Data.ProtoLens.FieldDescriptor OrcaLoadReport'NamedMetricsEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor OrcaLoadReport'NamedMetricsEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _OrcaLoadReport'NamedMetricsEntry'_unknownFields
        (\ x__ y__
           -> x__ {_OrcaLoadReport'NamedMetricsEntry'_unknownFields = y__})
  defMessage
    = OrcaLoadReport'NamedMetricsEntry'_constructor
        {_OrcaLoadReport'NamedMetricsEntry'key = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'NamedMetricsEntry'value = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'NamedMetricsEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          OrcaLoadReport'NamedMetricsEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser OrcaLoadReport'NamedMetricsEntry
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
          (do loop Data.ProtoLens.defMessage) "NamedMetricsEntry"
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
instance Control.DeepSeq.NFData OrcaLoadReport'NamedMetricsEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_OrcaLoadReport'NamedMetricsEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_OrcaLoadReport'NamedMetricsEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_OrcaLoadReport'NamedMetricsEntry'value x__) ()))
{- | Fields :
     
         * 'Proto.OrcaLoadReport_Fields.key' @:: Lens' OrcaLoadReport'RequestCostEntry Data.Text.Text@
         * 'Proto.OrcaLoadReport_Fields.value' @:: Lens' OrcaLoadReport'RequestCostEntry Prelude.Double@ -}
data OrcaLoadReport'RequestCostEntry
  = OrcaLoadReport'RequestCostEntry'_constructor {_OrcaLoadReport'RequestCostEntry'key :: !Data.Text.Text,
                                                  _OrcaLoadReport'RequestCostEntry'value :: !Prelude.Double,
                                                  _OrcaLoadReport'RequestCostEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show OrcaLoadReport'RequestCostEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField OrcaLoadReport'RequestCostEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'RequestCostEntry'key
           (\ x__ y__ -> x__ {_OrcaLoadReport'RequestCostEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField OrcaLoadReport'RequestCostEntry "value" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'RequestCostEntry'value
           (\ x__ y__ -> x__ {_OrcaLoadReport'RequestCostEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message OrcaLoadReport'RequestCostEntry where
  messageName _
    = Data.Text.pack "xds.data.orca.v3.OrcaLoadReport.RequestCostEntry"
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
              Data.ProtoLens.FieldDescriptor OrcaLoadReport'RequestCostEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor OrcaLoadReport'RequestCostEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _OrcaLoadReport'RequestCostEntry'_unknownFields
        (\ x__ y__
           -> x__ {_OrcaLoadReport'RequestCostEntry'_unknownFields = y__})
  defMessage
    = OrcaLoadReport'RequestCostEntry'_constructor
        {_OrcaLoadReport'RequestCostEntry'key = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'RequestCostEntry'value = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'RequestCostEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          OrcaLoadReport'RequestCostEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser OrcaLoadReport'RequestCostEntry
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
instance Control.DeepSeq.NFData OrcaLoadReport'RequestCostEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_OrcaLoadReport'RequestCostEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_OrcaLoadReport'RequestCostEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_OrcaLoadReport'RequestCostEntry'value x__) ()))
{- | Fields :
     
         * 'Proto.OrcaLoadReport_Fields.key' @:: Lens' OrcaLoadReport'UtilizationEntry Data.Text.Text@
         * 'Proto.OrcaLoadReport_Fields.value' @:: Lens' OrcaLoadReport'UtilizationEntry Prelude.Double@ -}
data OrcaLoadReport'UtilizationEntry
  = OrcaLoadReport'UtilizationEntry'_constructor {_OrcaLoadReport'UtilizationEntry'key :: !Data.Text.Text,
                                                  _OrcaLoadReport'UtilizationEntry'value :: !Prelude.Double,
                                                  _OrcaLoadReport'UtilizationEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show OrcaLoadReport'UtilizationEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField OrcaLoadReport'UtilizationEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'UtilizationEntry'key
           (\ x__ y__ -> x__ {_OrcaLoadReport'UtilizationEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField OrcaLoadReport'UtilizationEntry "value" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _OrcaLoadReport'UtilizationEntry'value
           (\ x__ y__ -> x__ {_OrcaLoadReport'UtilizationEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message OrcaLoadReport'UtilizationEntry where
  messageName _
    = Data.Text.pack "xds.data.orca.v3.OrcaLoadReport.UtilizationEntry"
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
              Data.ProtoLens.FieldDescriptor OrcaLoadReport'UtilizationEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor OrcaLoadReport'UtilizationEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _OrcaLoadReport'UtilizationEntry'_unknownFields
        (\ x__ y__
           -> x__ {_OrcaLoadReport'UtilizationEntry'_unknownFields = y__})
  defMessage
    = OrcaLoadReport'UtilizationEntry'_constructor
        {_OrcaLoadReport'UtilizationEntry'key = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'UtilizationEntry'value = Data.ProtoLens.fieldDefault,
         _OrcaLoadReport'UtilizationEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          OrcaLoadReport'UtilizationEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser OrcaLoadReport'UtilizationEntry
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
instance Control.DeepSeq.NFData OrcaLoadReport'UtilizationEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_OrcaLoadReport'UtilizationEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_OrcaLoadReport'UtilizationEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_OrcaLoadReport'UtilizationEntry'value x__) ()))
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\SYNorca_load_report.proto\DC2\DLExds.data.orca.v3\"\175\ENQ\n\
    \\SOOrcaLoadReport\DC2'\n\
    \\SIcpu_utilization\CAN\SOH \SOH(\SOHR\SOcpuUtilization\DC2'\n\
    \\SImem_utilization\CAN\STX \SOH(\SOHR\SOmemUtilization\DC2\DC4\n\
    \\ETXrps\CAN\ETX \SOH(\EOTR\ETXrpsB\STX\CAN\SOH\DC2T\n\
    \\frequest_cost\CAN\EOT \ETX(\v21.xds.data.orca.v3.OrcaLoadReport.RequestCostEntryR\vrequestCost\DC2S\n\
    \\vutilization\CAN\ENQ \ETX(\v21.xds.data.orca.v3.OrcaLoadReport.UtilizationEntryR\vutilization\DC2%\n\
    \\SOrps_fractional\CAN\ACK \SOH(\SOHR\rrpsFractional\DC2\DLE\n\
    \\ETXeps\CAN\a \SOH(\SOHR\ETXeps\DC2W\n\
    \\rnamed_metrics\CAN\b \ETX(\v22.xds.data.orca.v3.OrcaLoadReport.NamedMetricsEntryR\fnamedMetrics\DC27\n\
    \\ETBapplication_utilization\CAN\t \SOH(\SOHR\SYNapplicationUtilization\SUB>\n\
    \\DLERequestCostEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOH\SUB>\n\
    \\DLEUtilizationEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOH\SUB?\n\
    \\DC1NamedMetricsEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\SOHR\ENQvalue:\STX8\SOHJ\156\DC1\n\
    \\ACK\DC2\EOT\NUL\NUL1\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\EM\n\
    \\134\SOH\n\
    \\STX\EOT\NUL\DC2\EOT\a\NUL1\SOH2z See section `ORCA load report format` of the design document in\n\
    \ :ref:`https://github.com/envoyproxy/envoy/issues/6614`.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\a\b\SYN\n\
    \\250\SOH\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\f\STX\GS\SUB\236\SOH CPU utilization expressed as a fraction of available CPU resources. This\n\
    \ should be derived from the latest sample or measurement. The value may be\n\
    \ larger than 1.0 when the usage exceeds the reporter dependent notion of\n\
    \ soft limits.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\f\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\f\t\CAN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\f\ESC\FS\n\
    \\152\SOH\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\DLE\STX\GS\SUB\138\SOH Memory utilization expressed as a fraction of available memory\n\
    \ resources. This should be derived from the latest sample or measurement.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ENQ\DC2\ETX\DLE\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\DLE\t\CAN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\DLE\ESC\FS\n\
    \\176\SOH\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETX\NAK\STX%\SUB\162\SOH Total RPS being served by an endpoint. This should cover all services that an endpoint is\n\
    \ responsible for.\n\
    \ Deprecated -- use ``rps_fractional`` field instead.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ENQ\DC2\ETX\NAK\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETX\NAK\t\f\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETX\NAK\SI\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\b\DC2\ETX\NAK\DC1$\n\
    \\r\n\
    \\ACK\EOT\NUL\STX\STX\b\ETX\DC2\ETX\NAK\DC2#\n\
    \\142\SOH\n\
    \\EOT\EOT\NUL\STX\ETX\DC2\ETX\EM\STX'\SUB\128\SOH Application specific requests costs. Each value is an absolute cost (e.g. 3487 bytes of\n\
    \ storage) associated with the request.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ACK\DC2\ETX\EM\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\SOH\DC2\ETX\EM\SYN\"\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ETX\DC2\ETX\EM%&\n\
    \\160\SOH\n\
    \\EOT\EOT\NUL\STX\EOT\DC2\ETX\GS\STX&\SUB\146\SOH Resource utilization values. Each value is expressed as a fraction of total resources\n\
    \ available, derived from the latest sample or measurement.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\ACK\DC2\ETX\GS\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\SOH\DC2\ETX\GS\SYN!\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\ETX\DC2\ETX\GS$%\n\
    \z\n\
    \\EOT\EOT\NUL\STX\ENQ\DC2\ETX!\STX\FS\SUBm Total RPS being served by an endpoint. This should cover all services that an endpoint is\n\
    \ responsible for.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\ENQ\DC2\ETX!\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\SOH\DC2\ETX!\t\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\ETX\DC2\ETX!\SUB\ESC\n\
    \\138\SOH\n\
    \\EOT\EOT\NUL\STX\ACK\DC2\ETX%\STX\DC1\SUB} Total EPS (errors/second) being served by an endpoint. This should cover\n\
    \ all services that an endpoint is responsible for.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ACK\ENQ\DC2\ETX%\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ACK\SOH\DC2\ETX%\t\f\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ACK\ETX\DC2\ETX%\SI\DLE\n\
    \3\n\
    \\EOT\EOT\NUL\STX\a\DC2\ETX(\STX(\SUB& Application specific opaque metrics.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\a\ACK\DC2\ETX(\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\a\SOH\DC2\ETX(\SYN#\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\a\ETX\DC2\ETX(&'\n\
    \\148\ETX\n\
    \\EOT\EOT\NUL\STX\b\DC2\ETX0\STX%\SUB\134\ETX Application specific utilization expressed as a fraction of available\n\
    \ resources. For example, an application may report the max of CPU and memory\n\
    \ utilization for better load balancing if it is both CPU and memory bound.\n\
    \ This should be derived from the latest sample or measurement.\n\
    \ The value may be larger than 1.0 when the usage exceeds the reporter\n\
    \ dependent notion of soft limits.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\b\ENQ\DC2\ETX0\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\b\SOH\DC2\ETX0\t \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\b\ETX\DC2\ETX0#$b\ACKproto3"