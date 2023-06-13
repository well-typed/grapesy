-- proto-lens-0.7.1 introduces 'packedServiceDescriptor' to 'Service'.
-- For backwards compatibility with proto-lens-0.7.0, we use proto-lens-protoc
-- and disable the warning that these methods are missing (grapesy does not
-- depend on them).
{-# OPTIONS_GHC -Wno-missing-methods #-}

{- This file was auto-generated from route_guide.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.RouteGuide (
        RouteGuide(..), Feature(), Point(), Rectangle(), RouteNote(),
        RouteSummary()
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
     
         * 'Proto.RouteGuide_Fields.name' @:: Lens' Feature Data.Text.Text@
         * 'Proto.RouteGuide_Fields.location' @:: Lens' Feature Point@
         * 'Proto.RouteGuide_Fields.maybe'location' @:: Lens' Feature (Prelude.Maybe Point)@ -}
data Feature
  = Feature'_constructor {_Feature'name :: !Data.Text.Text,
                          _Feature'location :: !(Prelude.Maybe Point),
                          _Feature'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Feature where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Feature "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Feature'name (\ x__ y__ -> x__ {_Feature'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Feature "location" Point where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Feature'location (\ x__ y__ -> x__ {_Feature'location = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Feature "maybe'location" (Prelude.Maybe Point) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Feature'location (\ x__ y__ -> x__ {_Feature'location = y__}))
        Prelude.id
instance Data.ProtoLens.Message Feature where
  messageName _ = Data.Text.pack "routeguide.Feature"
  packedMessageDescriptor _
    = "\n\
      \\aFeature\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2-\n\
      \\blocation\CAN\STX \SOH(\v2\DC1.routeguide.PointR\blocation"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor Feature
        location__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "location"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Point)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'location")) ::
              Data.ProtoLens.FieldDescriptor Feature
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, location__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Feature'_unknownFields
        (\ x__ y__ -> x__ {_Feature'_unknownFields = y__})
  defMessage
    = Feature'_constructor
        {_Feature'name = Data.ProtoLens.fieldDefault,
         _Feature'location = Prelude.Nothing, _Feature'_unknownFields = []}
  parseMessage
    = let
        loop :: Feature -> Data.ProtoLens.Encoding.Bytes.Parser Feature
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
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "location"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"location") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Feature"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
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
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'location") _x
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
instance Control.DeepSeq.NFData Feature where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Feature'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Feature'name x__)
                (Control.DeepSeq.deepseq (_Feature'location x__) ()))
{- | Fields :
     
         * 'Proto.RouteGuide_Fields.latitude' @:: Lens' Point Data.Int.Int32@
         * 'Proto.RouteGuide_Fields.longitude' @:: Lens' Point Data.Int.Int32@ -}
data Point
  = Point'_constructor {_Point'latitude :: !Data.Int.Int32,
                        _Point'longitude :: !Data.Int.Int32,
                        _Point'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Point where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Point "latitude" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Point'latitude (\ x__ y__ -> x__ {_Point'latitude = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Point "longitude" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Point'longitude (\ x__ y__ -> x__ {_Point'longitude = y__}))
        Prelude.id
instance Data.ProtoLens.Message Point where
  messageName _ = Data.Text.pack "routeguide.Point"
  packedMessageDescriptor _
    = "\n\
      \\ENQPoint\DC2\SUB\n\
      \\blatitude\CAN\SOH \SOH(\ENQR\blatitude\DC2\FS\n\
      \\tlongitude\CAN\STX \SOH(\ENQR\tlongitude"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        latitude__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "latitude"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"latitude")) ::
              Data.ProtoLens.FieldDescriptor Point
        longitude__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "longitude"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"longitude")) ::
              Data.ProtoLens.FieldDescriptor Point
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, latitude__field_descriptor),
           (Data.ProtoLens.Tag 2, longitude__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Point'_unknownFields
        (\ x__ y__ -> x__ {_Point'_unknownFields = y__})
  defMessage
    = Point'_constructor
        {_Point'latitude = Data.ProtoLens.fieldDefault,
         _Point'longitude = Data.ProtoLens.fieldDefault,
         _Point'_unknownFields = []}
  parseMessage
    = let
        loop :: Point -> Data.ProtoLens.Encoding.Bytes.Parser Point
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
                                       "latitude"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"latitude") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "longitude"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"longitude") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Point"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"latitude") _x
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
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"longitude") _x
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
instance Control.DeepSeq.NFData Point where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Point'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Point'latitude x__)
                (Control.DeepSeq.deepseq (_Point'longitude x__) ()))
{- | Fields :
     
         * 'Proto.RouteGuide_Fields.lo' @:: Lens' Rectangle Point@
         * 'Proto.RouteGuide_Fields.maybe'lo' @:: Lens' Rectangle (Prelude.Maybe Point)@
         * 'Proto.RouteGuide_Fields.hi' @:: Lens' Rectangle Point@
         * 'Proto.RouteGuide_Fields.maybe'hi' @:: Lens' Rectangle (Prelude.Maybe Point)@ -}
data Rectangle
  = Rectangle'_constructor {_Rectangle'lo :: !(Prelude.Maybe Point),
                            _Rectangle'hi :: !(Prelude.Maybe Point),
                            _Rectangle'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Rectangle where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Rectangle "lo" Point where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Rectangle'lo (\ x__ y__ -> x__ {_Rectangle'lo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Rectangle "maybe'lo" (Prelude.Maybe Point) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Rectangle'lo (\ x__ y__ -> x__ {_Rectangle'lo = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Rectangle "hi" Point where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Rectangle'hi (\ x__ y__ -> x__ {_Rectangle'hi = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Rectangle "maybe'hi" (Prelude.Maybe Point) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Rectangle'hi (\ x__ y__ -> x__ {_Rectangle'hi = y__}))
        Prelude.id
instance Data.ProtoLens.Message Rectangle where
  messageName _ = Data.Text.pack "routeguide.Rectangle"
  packedMessageDescriptor _
    = "\n\
      \\tRectangle\DC2!\n\
      \\STXlo\CAN\SOH \SOH(\v2\DC1.routeguide.PointR\STXlo\DC2!\n\
      \\STXhi\CAN\STX \SOH(\v2\DC1.routeguide.PointR\STXhi"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        lo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "lo"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Point)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'lo")) ::
              Data.ProtoLens.FieldDescriptor Rectangle
        hi__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "hi"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Point)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'hi")) ::
              Data.ProtoLens.FieldDescriptor Rectangle
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, lo__field_descriptor),
           (Data.ProtoLens.Tag 2, hi__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Rectangle'_unknownFields
        (\ x__ y__ -> x__ {_Rectangle'_unknownFields = y__})
  defMessage
    = Rectangle'_constructor
        {_Rectangle'lo = Prelude.Nothing, _Rectangle'hi = Prelude.Nothing,
         _Rectangle'_unknownFields = []}
  parseMessage
    = let
        loop :: Rectangle -> Data.ProtoLens.Encoding.Bytes.Parser Rectangle
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
                                       "lo"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"lo") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "hi"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"hi") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Rectangle"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'lo") _x
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
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'hi") _x
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
instance Control.DeepSeq.NFData Rectangle where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Rectangle'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Rectangle'lo x__)
                (Control.DeepSeq.deepseq (_Rectangle'hi x__) ()))
{- | Fields :
     
         * 'Proto.RouteGuide_Fields.location' @:: Lens' RouteNote Point@
         * 'Proto.RouteGuide_Fields.maybe'location' @:: Lens' RouteNote (Prelude.Maybe Point)@
         * 'Proto.RouteGuide_Fields.message' @:: Lens' RouteNote Data.Text.Text@ -}
data RouteNote
  = RouteNote'_constructor {_RouteNote'location :: !(Prelude.Maybe Point),
                            _RouteNote'message :: !Data.Text.Text,
                            _RouteNote'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show RouteNote where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField RouteNote "location" Point where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RouteNote'location (\ x__ y__ -> x__ {_RouteNote'location = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField RouteNote "maybe'location" (Prelude.Maybe Point) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RouteNote'location (\ x__ y__ -> x__ {_RouteNote'location = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField RouteNote "message" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RouteNote'message (\ x__ y__ -> x__ {_RouteNote'message = y__}))
        Prelude.id
instance Data.ProtoLens.Message RouteNote where
  messageName _ = Data.Text.pack "routeguide.RouteNote"
  packedMessageDescriptor _
    = "\n\
      \\tRouteNote\DC2-\n\
      \\blocation\CAN\SOH \SOH(\v2\DC1.routeguide.PointR\blocation\DC2\CAN\n\
      \\amessage\CAN\STX \SOH(\tR\amessage"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        location__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "location"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Point)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'location")) ::
              Data.ProtoLens.FieldDescriptor RouteNote
        message__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"message")) ::
              Data.ProtoLens.FieldDescriptor RouteNote
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, location__field_descriptor),
           (Data.ProtoLens.Tag 2, message__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _RouteNote'_unknownFields
        (\ x__ y__ -> x__ {_RouteNote'_unknownFields = y__})
  defMessage
    = RouteNote'_constructor
        {_RouteNote'location = Prelude.Nothing,
         _RouteNote'message = Data.ProtoLens.fieldDefault,
         _RouteNote'_unknownFields = []}
  parseMessage
    = let
        loop :: RouteNote -> Data.ProtoLens.Encoding.Bytes.Parser RouteNote
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
                                       "location"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"location") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
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
          (do loop Data.ProtoLens.defMessage) "RouteNote"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'location") _x
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
instance Control.DeepSeq.NFData RouteNote where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_RouteNote'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_RouteNote'location x__)
                (Control.DeepSeq.deepseq (_RouteNote'message x__) ()))
{- | Fields :
     
         * 'Proto.RouteGuide_Fields.pointCount' @:: Lens' RouteSummary Data.Int.Int32@
         * 'Proto.RouteGuide_Fields.featureCount' @:: Lens' RouteSummary Data.Int.Int32@
         * 'Proto.RouteGuide_Fields.distance' @:: Lens' RouteSummary Data.Int.Int32@
         * 'Proto.RouteGuide_Fields.elapsedTime' @:: Lens' RouteSummary Data.Int.Int32@ -}
data RouteSummary
  = RouteSummary'_constructor {_RouteSummary'pointCount :: !Data.Int.Int32,
                               _RouteSummary'featureCount :: !Data.Int.Int32,
                               _RouteSummary'distance :: !Data.Int.Int32,
                               _RouteSummary'elapsedTime :: !Data.Int.Int32,
                               _RouteSummary'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show RouteSummary where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField RouteSummary "pointCount" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RouteSummary'pointCount
           (\ x__ y__ -> x__ {_RouteSummary'pointCount = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField RouteSummary "featureCount" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RouteSummary'featureCount
           (\ x__ y__ -> x__ {_RouteSummary'featureCount = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField RouteSummary "distance" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RouteSummary'distance
           (\ x__ y__ -> x__ {_RouteSummary'distance = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField RouteSummary "elapsedTime" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RouteSummary'elapsedTime
           (\ x__ y__ -> x__ {_RouteSummary'elapsedTime = y__}))
        Prelude.id
instance Data.ProtoLens.Message RouteSummary where
  messageName _ = Data.Text.pack "routeguide.RouteSummary"
  packedMessageDescriptor _
    = "\n\
      \\fRouteSummary\DC2\US\n\
      \\vpoint_count\CAN\SOH \SOH(\ENQR\n\
      \pointCount\DC2#\n\
      \\rfeature_count\CAN\STX \SOH(\ENQR\ffeatureCount\DC2\SUB\n\
      \\bdistance\CAN\ETX \SOH(\ENQR\bdistance\DC2!\n\
      \\felapsed_time\CAN\EOT \SOH(\ENQR\velapsedTime"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        pointCount__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "point_count"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"pointCount")) ::
              Data.ProtoLens.FieldDescriptor RouteSummary
        featureCount__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "feature_count"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"featureCount")) ::
              Data.ProtoLens.FieldDescriptor RouteSummary
        distance__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "distance"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"distance")) ::
              Data.ProtoLens.FieldDescriptor RouteSummary
        elapsedTime__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "elapsed_time"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"elapsedTime")) ::
              Data.ProtoLens.FieldDescriptor RouteSummary
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, pointCount__field_descriptor),
           (Data.ProtoLens.Tag 2, featureCount__field_descriptor),
           (Data.ProtoLens.Tag 3, distance__field_descriptor),
           (Data.ProtoLens.Tag 4, elapsedTime__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _RouteSummary'_unknownFields
        (\ x__ y__ -> x__ {_RouteSummary'_unknownFields = y__})
  defMessage
    = RouteSummary'_constructor
        {_RouteSummary'pointCount = Data.ProtoLens.fieldDefault,
         _RouteSummary'featureCount = Data.ProtoLens.fieldDefault,
         _RouteSummary'distance = Data.ProtoLens.fieldDefault,
         _RouteSummary'elapsedTime = Data.ProtoLens.fieldDefault,
         _RouteSummary'_unknownFields = []}
  parseMessage
    = let
        loop ::
          RouteSummary -> Data.ProtoLens.Encoding.Bytes.Parser RouteSummary
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
                                       "point_count"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"pointCount") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "feature_count"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"featureCount") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "distance"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"distance") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "elapsed_time"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"elapsedTime") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "RouteSummary"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"pointCount") _x
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
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"featureCount") _x
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
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"distance") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view (Data.ProtoLens.Field.field @"elapsedTime") _x
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
instance Control.DeepSeq.NFData RouteSummary where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_RouteSummary'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_RouteSummary'pointCount x__)
                (Control.DeepSeq.deepseq
                   (_RouteSummary'featureCount x__)
                   (Control.DeepSeq.deepseq
                      (_RouteSummary'distance x__)
                      (Control.DeepSeq.deepseq (_RouteSummary'elapsedTime x__) ()))))
data RouteGuide = RouteGuide {}
instance Data.ProtoLens.Service.Types.Service RouteGuide where
  type ServiceName RouteGuide = "RouteGuide"
  type ServicePackage RouteGuide = "routeguide"
  type ServiceMethods RouteGuide = '["getFeature",
                                     "listFeatures",
                                     "recordRoute",
                                     "routeChat"]
instance Data.ProtoLens.Service.Types.HasMethodImpl RouteGuide "getFeature" where
  type MethodName RouteGuide "getFeature" = "GetFeature"
  type MethodInput RouteGuide "getFeature" = Point
  type MethodOutput RouteGuide "getFeature" = Feature
  type MethodStreamingType RouteGuide "getFeature" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl RouteGuide "listFeatures" where
  type MethodName RouteGuide "listFeatures" = "ListFeatures"
  type MethodInput RouteGuide "listFeatures" = Rectangle
  type MethodOutput RouteGuide "listFeatures" = Feature
  type MethodStreamingType RouteGuide "listFeatures" = 'Data.ProtoLens.Service.Types.ServerStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl RouteGuide "recordRoute" where
  type MethodName RouteGuide "recordRoute" = "RecordRoute"
  type MethodInput RouteGuide "recordRoute" = Point
  type MethodOutput RouteGuide "recordRoute" = RouteSummary
  type MethodStreamingType RouteGuide "recordRoute" = 'Data.ProtoLens.Service.Types.ClientStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl RouteGuide "routeChat" where
  type MethodName RouteGuide "routeChat" = "RouteChat"
  type MethodInput RouteGuide "routeChat" = RouteNote
  type MethodOutput RouteGuide "routeChat" = RouteNote
  type MethodStreamingType RouteGuide "routeChat" = 'Data.ProtoLens.Service.Types.BiDiStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\DC1route_guide.proto\DC2\n\
    \routeguide\"A\n\
    \\ENQPoint\DC2\SUB\n\
    \\blatitude\CAN\SOH \SOH(\ENQR\blatitude\DC2\FS\n\
    \\tlongitude\CAN\STX \SOH(\ENQR\tlongitude\"Q\n\
    \\tRectangle\DC2!\n\
    \\STXlo\CAN\SOH \SOH(\v2\DC1.routeguide.PointR\STXlo\DC2!\n\
    \\STXhi\CAN\STX \SOH(\v2\DC1.routeguide.PointR\STXhi\"L\n\
    \\aFeature\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2-\n\
    \\blocation\CAN\STX \SOH(\v2\DC1.routeguide.PointR\blocation\"T\n\
    \\tRouteNote\DC2-\n\
    \\blocation\CAN\SOH \SOH(\v2\DC1.routeguide.PointR\blocation\DC2\CAN\n\
    \\amessage\CAN\STX \SOH(\tR\amessage\"\147\SOH\n\
    \\fRouteSummary\DC2\US\n\
    \\vpoint_count\CAN\SOH \SOH(\ENQR\n\
    \pointCount\DC2#\n\
    \\rfeature_count\CAN\STX \SOH(\ENQR\ffeatureCount\DC2\SUB\n\
    \\bdistance\CAN\ETX \SOH(\ENQR\bdistance\DC2!\n\
    \\felapsed_time\CAN\EOT \SOH(\ENQR\velapsedTime2\133\STX\n\
    \\n\
    \RouteGuide\DC26\n\
    \\n\
    \GetFeature\DC2\DC1.routeguide.Point\SUB\DC3.routeguide.Feature\"\NUL\DC2>\n\
    \\fListFeatures\DC2\NAK.routeguide.Rectangle\SUB\DC3.routeguide.Feature\"\NUL0\SOH\DC2>\n\
    \\vRecordRoute\DC2\DC1.routeguide.Point\SUB\CAN.routeguide.RouteSummary\"\NUL(\SOH\DC2?\n\
    \\tRouteChat\DC2\NAK.routeguide.RouteNote\SUB\NAK.routeguide.RouteNote\"\NUL(\SOH0\SOHB6\n\
    \\ESCio.grpc.examples.routeguideB\SIRouteGuideProtoP\SOH\162\STX\ETXRTGJ\130\GS\n\
    \\ACK\DC2\EOT\SO\NULn\SOH\n\
    \\191\EOT\n\
    \\SOH\f\DC2\ETX\SO\NUL\DC22\180\EOT Copyright 2015 gRPC authors.\n\
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
    \\SOH\b\DC2\ETX\DLE\NUL\"\n\
    \\t\n\
    \\STX\b\n\
    \\DC2\ETX\DLE\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX\DC1\NUL4\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\DC1\NUL4\n\
    \\b\n\
    \\SOH\b\DC2\ETX\DC2\NUL0\n\
    \\t\n\
    \\STX\b\b\DC2\ETX\DC2\NUL0\n\
    \\b\n\
    \\SOH\b\DC2\ETX\DC3\NUL!\n\
    \\t\n\
    \\STX\b$\DC2\ETX\DC3\NUL!\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\NAK\NUL\DC3\n\
    \/\n\
    \\STX\ACK\NUL\DC2\EOT\CAN\NUL4\SOH\SUB# Interface exported by the server.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\CAN\b\DC2\n\
    \\161\SOH\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\US\STX,\SUB\147\SOH A simple RPC.\n\
    \\n\
    \ Obtains the feature at a given position.\n\
    \\n\
    \ A feature with an empty name is returned if there's no feature at the given\n\
    \ position.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\US\ACK\DLE\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\US\DC1\SYN\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\US!(\n\
    \\167\STX\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX'\STX9\SUB\153\STX A server-to-client streaming RPC.\n\
    \\n\
    \ Obtains the Features available within the given Rectangle.  Results are\n\
    \ streamed rather than returned at once (e.g. in a response message with a\n\
    \ repeated field), as the rectangle may cover a large area and contain a\n\
    \ huge number of features.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX'\ACK\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX'\DC3\FS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ACK\DC2\ETX''-\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX'.5\n\
    \\161\SOH\n\
    \\EOT\ACK\NUL\STX\STX\DC2\ETX-\STX9\SUB\147\SOH A client-to-server streaming RPC.\n\
    \\n\
    \ Accepts a stream of Points on a route being traversed, returning a\n\
    \ RouteSummary when traversal is completed.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\SOH\DC2\ETX-\ACK\DC1\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\ENQ\DC2\ETX-\DC2\CAN\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\STX\DC2\ETX-\EM\RS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\ETX\DC2\ETX-)5\n\
    \\177\SOH\n\
    \\EOT\ACK\NUL\STX\ETX\DC2\ETX3\STX?\SUB\163\SOH A Bidirectional streaming RPC.\n\
    \\n\
    \ Accepts a stream of RouteNotes sent while a route is being traversed,\n\
    \ while receiving other RouteNotes (e.g. from other users).\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\SOH\DC2\ETX3\ACK\SI\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\ENQ\DC2\ETX3\DLE\SYN\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\STX\DC2\ETX3\ETB \n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\ACK\DC2\ETX3+1\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\ETX\DC2\ETX32;\n\
    \\145\STX\n\
    \\STX\EOT\NUL\DC2\EOT:\NUL=\SOH\SUB\132\STX Points are represented as latitude-longitude pairs in the E7 representation\n\
    \ (degrees multiplied by 10**7 and rounded to the nearest integer).\n\
    \ Latitudes should be in the range +/- 90 degrees and longitude should be in\n\
    \ the range +/- 180 degrees (inclusive).\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX:\b\r\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX;\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX;\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX;\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX;\DC3\DC4\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX<\STX\SYN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ENQ\DC2\ETX<\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX<\b\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX<\DC4\NAK\n\
    \k\n\
    \\STX\EOT\SOH\DC2\EOTA\NULG\SOH\SUB_ A latitude-longitude rectangle, represented as two diagonally opposite\n\
    \ points \"lo\" and \"hi\".\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETXA\b\DC1\n\
    \+\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETXC\STX\SI\SUB\RS One corner of the rectangle.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETXC\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETXC\b\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETXC\r\SO\n\
    \1\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETXF\STX\SI\SUB$ The other corner of the rectangle.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETXF\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETXF\b\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETXF\r\SO\n\
    \o\n\
    \\STX\EOT\STX\DC2\EOTL\NULR\SOH\SUBc A feature names something at a given point.\n\
    \\n\
    \ If a feature could not be named, the name is empty.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETXL\b\SI\n\
    \'\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETXN\STX\DC2\SUB\SUB The name of the feature.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETXN\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETXN\t\r\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETXN\DLE\DC1\n\
    \7\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETXQ\STX\NAK\SUB* The point where the feature is detected.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ACK\DC2\ETXQ\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETXQ\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETXQ\DC3\DC4\n\
    \C\n\
    \\STX\EOT\ETX\DC2\EOTU\NUL[\SOH\SUB7 A RouteNote is a message sent while at a given point.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETXU\b\DC1\n\
    \;\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETXW\STX\NAK\SUB. The location from which the message is sent.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ACK\DC2\ETXW\STX\a\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETXW\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETXW\DC3\DC4\n\
    \&\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\ETXZ\STX\NAK\SUB\EM The message to be sent.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ENQ\DC2\ETXZ\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\ETXZ\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\ETXZ\DC3\DC4\n\
    \\255\SOH\n\
    \\STX\EOT\EOT\DC2\EOTb\NULn\SOH\SUB\242\SOH A RouteSummary is received in response to a RecordRoute rpc.\n\
    \\n\
    \ It contains the number of individual points received, the number of\n\
    \ detected features, and the total distance covered as the cumulative sum of\n\
    \ the distance between each point.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETXb\b\DC4\n\
    \-\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETXd\STX\CAN\SUB  The number of points received.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ENQ\DC2\ETXd\STX\a\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETXd\b\DC3\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETXd\SYN\ETB\n\
    \N\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETXg\STX\SUB\SUBA The number of known features passed while traversing the route.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ENQ\DC2\ETXg\STX\a\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETXg\b\NAK\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETXg\CAN\EM\n\
    \.\n\
    \\EOT\EOT\EOT\STX\STX\DC2\ETXj\STX\NAK\SUB! The distance covered in metres.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ENQ\DC2\ETXj\STX\a\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\SOH\DC2\ETXj\b\DLE\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ETX\DC2\ETXj\DC3\DC4\n\
    \8\n\
    \\EOT\EOT\EOT\STX\ETX\DC2\ETXm\STX\EM\SUB+ The duration of the traversal in seconds.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\ENQ\DC2\ETXm\STX\a\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\SOH\DC2\ETXm\b\DC4\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\ETX\DC2\ETXm\ETB\CANb\ACKproto3"
