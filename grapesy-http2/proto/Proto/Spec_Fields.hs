{-# OPTIONS_GHC -Wno-prepositive-qualified-module -Wno-identities #-}
{- This file was auto-generated from spec.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Spec_Fields where
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
defaultAnother ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultAnother" a) =>
  Lens.Family2.LensLike' f s a
defaultAnother = Data.ProtoLens.Field.field @"defaultAnother"
defaultEnum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultEnum" a) =>
  Lens.Family2.LensLike' f s a
defaultEnum = Data.ProtoLens.Field.field @"defaultEnum"
defaultNested ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultNested" a) =>
  Lens.Family2.LensLike' f s a
defaultNested = Data.ProtoLens.Field.field @"defaultNested"
defaultScalar01 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar01" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar01 = Data.ProtoLens.Field.field @"defaultScalar01"
defaultScalar02 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar02" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar02 = Data.ProtoLens.Field.field @"defaultScalar02"
defaultScalar03 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar03" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar03 = Data.ProtoLens.Field.field @"defaultScalar03"
defaultScalar04 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar04" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar04 = Data.ProtoLens.Field.field @"defaultScalar04"
defaultScalar05 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar05" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar05 = Data.ProtoLens.Field.field @"defaultScalar05"
defaultScalar06 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar06" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar06 = Data.ProtoLens.Field.field @"defaultScalar06"
defaultScalar07 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar07" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar07 = Data.ProtoLens.Field.field @"defaultScalar07"
defaultScalar08 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar08" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar08 = Data.ProtoLens.Field.field @"defaultScalar08"
defaultScalar09 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar09" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar09 = Data.ProtoLens.Field.field @"defaultScalar09"
defaultScalar10 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar10" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar10 = Data.ProtoLens.Field.field @"defaultScalar10"
defaultScalar11 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar11" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar11 = Data.ProtoLens.Field.field @"defaultScalar11"
defaultScalar12 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar12" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar12 = Data.ProtoLens.Field.field @"defaultScalar12"
defaultScalar13 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar13" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar13 = Data.ProtoLens.Field.field @"defaultScalar13"
defaultScalar14 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar14" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar14 = Data.ProtoLens.Field.field @"defaultScalar14"
defaultScalar15 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "defaultScalar15" a) =>
  Lens.Family2.LensLike' f s a
defaultScalar15 = Data.ProtoLens.Field.field @"defaultScalar15"
key ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
  Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
mapAnother ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapAnother" a) =>
  Lens.Family2.LensLike' f s a
mapAnother = Data.ProtoLens.Field.field @"mapAnother"
mapEnum ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "mapEnum" a) =>
  Lens.Family2.LensLike' f s a
mapEnum = Data.ProtoLens.Field.field @"mapEnum"
mapNested ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapNested" a) =>
  Lens.Family2.LensLike' f s a
mapNested = Data.ProtoLens.Field.field @"mapNested"
mapScalar01 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar01" a) =>
  Lens.Family2.LensLike' f s a
mapScalar01 = Data.ProtoLens.Field.field @"mapScalar01"
mapScalar02 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar02" a) =>
  Lens.Family2.LensLike' f s a
mapScalar02 = Data.ProtoLens.Field.field @"mapScalar02"
mapScalar03 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar03" a) =>
  Lens.Family2.LensLike' f s a
mapScalar03 = Data.ProtoLens.Field.field @"mapScalar03"
mapScalar04 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar04" a) =>
  Lens.Family2.LensLike' f s a
mapScalar04 = Data.ProtoLens.Field.field @"mapScalar04"
mapScalar05 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar05" a) =>
  Lens.Family2.LensLike' f s a
mapScalar05 = Data.ProtoLens.Field.field @"mapScalar05"
mapScalar06 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar06" a) =>
  Lens.Family2.LensLike' f s a
mapScalar06 = Data.ProtoLens.Field.field @"mapScalar06"
mapScalar07 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar07" a) =>
  Lens.Family2.LensLike' f s a
mapScalar07 = Data.ProtoLens.Field.field @"mapScalar07"
mapScalar08 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar08" a) =>
  Lens.Family2.LensLike' f s a
mapScalar08 = Data.ProtoLens.Field.field @"mapScalar08"
mapScalar09 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar09" a) =>
  Lens.Family2.LensLike' f s a
mapScalar09 = Data.ProtoLens.Field.field @"mapScalar09"
mapScalar10 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar10" a) =>
  Lens.Family2.LensLike' f s a
mapScalar10 = Data.ProtoLens.Field.field @"mapScalar10"
mapScalar11 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar11" a) =>
  Lens.Family2.LensLike' f s a
mapScalar11 = Data.ProtoLens.Field.field @"mapScalar11"
mapScalar12 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar12" a) =>
  Lens.Family2.LensLike' f s a
mapScalar12 = Data.ProtoLens.Field.field @"mapScalar12"
mapScalar13 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar13" a) =>
  Lens.Family2.LensLike' f s a
mapScalar13 = Data.ProtoLens.Field.field @"mapScalar13"
mapScalar14 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar14" a) =>
  Lens.Family2.LensLike' f s a
mapScalar14 = Data.ProtoLens.Field.field @"mapScalar14"
mapScalar15 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "mapScalar15" a) =>
  Lens.Family2.LensLike' f s a
mapScalar15 = Data.ProtoLens.Field.field @"mapScalar15"
maybe'defaultAnother ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'defaultAnother" a) =>
  Lens.Family2.LensLike' f s a
maybe'defaultAnother
  = Data.ProtoLens.Field.field @"maybe'defaultAnother"
maybe'defaultNested ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'defaultNested" a) =>
  Lens.Family2.LensLike' f s a
maybe'defaultNested
  = Data.ProtoLens.Field.field @"maybe'defaultNested"
maybe'exampleOneOf ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'exampleOneOf" a) =>
  Lens.Family2.LensLike' f s a
maybe'exampleOneOf
  = Data.ProtoLens.Field.field @"maybe'exampleOneOf"
maybe'oneofAnother ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofAnother" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofAnother
  = Data.ProtoLens.Field.field @"maybe'oneofAnother"
maybe'oneofEnum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofEnum" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofEnum = Data.ProtoLens.Field.field @"maybe'oneofEnum"
maybe'oneofNested ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofNested" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofNested = Data.ProtoLens.Field.field @"maybe'oneofNested"
maybe'oneofScalar01 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar01" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar01
  = Data.ProtoLens.Field.field @"maybe'oneofScalar01"
maybe'oneofScalar02 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar02" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar02
  = Data.ProtoLens.Field.field @"maybe'oneofScalar02"
maybe'oneofScalar03 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar03" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar03
  = Data.ProtoLens.Field.field @"maybe'oneofScalar03"
maybe'oneofScalar04 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar04" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar04
  = Data.ProtoLens.Field.field @"maybe'oneofScalar04"
maybe'oneofScalar05 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar05" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar05
  = Data.ProtoLens.Field.field @"maybe'oneofScalar05"
maybe'oneofScalar06 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar06" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar06
  = Data.ProtoLens.Field.field @"maybe'oneofScalar06"
maybe'oneofScalar07 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar07" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar07
  = Data.ProtoLens.Field.field @"maybe'oneofScalar07"
maybe'oneofScalar08 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar08" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar08
  = Data.ProtoLens.Field.field @"maybe'oneofScalar08"
maybe'oneofScalar09 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar09" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar09
  = Data.ProtoLens.Field.field @"maybe'oneofScalar09"
maybe'oneofScalar10 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar10" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar10
  = Data.ProtoLens.Field.field @"maybe'oneofScalar10"
maybe'oneofScalar11 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar11" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar11
  = Data.ProtoLens.Field.field @"maybe'oneofScalar11"
maybe'oneofScalar12 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar12" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar12
  = Data.ProtoLens.Field.field @"maybe'oneofScalar12"
maybe'oneofScalar13 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar13" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar13
  = Data.ProtoLens.Field.field @"maybe'oneofScalar13"
maybe'oneofScalar14 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar14" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar14
  = Data.ProtoLens.Field.field @"maybe'oneofScalar14"
maybe'oneofScalar15 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'oneofScalar15" a) =>
  Lens.Family2.LensLike' f s a
maybe'oneofScalar15
  = Data.ProtoLens.Field.field @"maybe'oneofScalar15"
maybe'optionalAnother ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalAnother" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalAnother
  = Data.ProtoLens.Field.field @"maybe'optionalAnother"
maybe'optionalEnum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalEnum" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalEnum
  = Data.ProtoLens.Field.field @"maybe'optionalEnum"
maybe'optionalNested ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalNested" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalNested
  = Data.ProtoLens.Field.field @"maybe'optionalNested"
maybe'optionalScalar01 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar01" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar01
  = Data.ProtoLens.Field.field @"maybe'optionalScalar01"
maybe'optionalScalar02 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar02" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar02
  = Data.ProtoLens.Field.field @"maybe'optionalScalar02"
maybe'optionalScalar03 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar03" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar03
  = Data.ProtoLens.Field.field @"maybe'optionalScalar03"
maybe'optionalScalar04 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar04" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar04
  = Data.ProtoLens.Field.field @"maybe'optionalScalar04"
maybe'optionalScalar05 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar05" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar05
  = Data.ProtoLens.Field.field @"maybe'optionalScalar05"
maybe'optionalScalar06 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar06" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar06
  = Data.ProtoLens.Field.field @"maybe'optionalScalar06"
maybe'optionalScalar07 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar07" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar07
  = Data.ProtoLens.Field.field @"maybe'optionalScalar07"
maybe'optionalScalar08 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar08" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar08
  = Data.ProtoLens.Field.field @"maybe'optionalScalar08"
maybe'optionalScalar09 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar09" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar09
  = Data.ProtoLens.Field.field @"maybe'optionalScalar09"
maybe'optionalScalar10 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar10" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar10
  = Data.ProtoLens.Field.field @"maybe'optionalScalar10"
maybe'optionalScalar11 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar11" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar11
  = Data.ProtoLens.Field.field @"maybe'optionalScalar11"
maybe'optionalScalar12 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar12" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar12
  = Data.ProtoLens.Field.field @"maybe'optionalScalar12"
maybe'optionalScalar13 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar13" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar13
  = Data.ProtoLens.Field.field @"maybe'optionalScalar13"
maybe'optionalScalar14 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar14" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar14
  = Data.ProtoLens.Field.field @"maybe'optionalScalar14"
maybe'optionalScalar15 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'optionalScalar15" a) =>
  Lens.Family2.LensLike' f s a
maybe'optionalScalar15
  = Data.ProtoLens.Field.field @"maybe'optionalScalar15"
maybe'value ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'value" a) =>
  Lens.Family2.LensLike' f s a
maybe'value = Data.ProtoLens.Field.field @"maybe'value"
oneofAnother ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofAnother" a) =>
  Lens.Family2.LensLike' f s a
oneofAnother = Data.ProtoLens.Field.field @"oneofAnother"
oneofEnum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofEnum" a) =>
  Lens.Family2.LensLike' f s a
oneofEnum = Data.ProtoLens.Field.field @"oneofEnum"
oneofNested ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofNested" a) =>
  Lens.Family2.LensLike' f s a
oneofNested = Data.ProtoLens.Field.field @"oneofNested"
oneofScalar01 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar01" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar01 = Data.ProtoLens.Field.field @"oneofScalar01"
oneofScalar02 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar02" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar02 = Data.ProtoLens.Field.field @"oneofScalar02"
oneofScalar03 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar03" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar03 = Data.ProtoLens.Field.field @"oneofScalar03"
oneofScalar04 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar04" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar04 = Data.ProtoLens.Field.field @"oneofScalar04"
oneofScalar05 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar05" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar05 = Data.ProtoLens.Field.field @"oneofScalar05"
oneofScalar06 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar06" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar06 = Data.ProtoLens.Field.field @"oneofScalar06"
oneofScalar07 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar07" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar07 = Data.ProtoLens.Field.field @"oneofScalar07"
oneofScalar08 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar08" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar08 = Data.ProtoLens.Field.field @"oneofScalar08"
oneofScalar09 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar09" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar09 = Data.ProtoLens.Field.field @"oneofScalar09"
oneofScalar10 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar10" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar10 = Data.ProtoLens.Field.field @"oneofScalar10"
oneofScalar11 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar11" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar11 = Data.ProtoLens.Field.field @"oneofScalar11"
oneofScalar12 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar12" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar12 = Data.ProtoLens.Field.field @"oneofScalar12"
oneofScalar13 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar13" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar13 = Data.ProtoLens.Field.field @"oneofScalar13"
oneofScalar14 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar14" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar14 = Data.ProtoLens.Field.field @"oneofScalar14"
oneofScalar15 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "oneofScalar15" a) =>
  Lens.Family2.LensLike' f s a
oneofScalar15 = Data.ProtoLens.Field.field @"oneofScalar15"
optionalAnother ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalAnother" a) =>
  Lens.Family2.LensLike' f s a
optionalAnother = Data.ProtoLens.Field.field @"optionalAnother"
optionalEnum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalEnum" a) =>
  Lens.Family2.LensLike' f s a
optionalEnum = Data.ProtoLens.Field.field @"optionalEnum"
optionalNested ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalNested" a) =>
  Lens.Family2.LensLike' f s a
optionalNested = Data.ProtoLens.Field.field @"optionalNested"
optionalScalar01 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar01" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar01 = Data.ProtoLens.Field.field @"optionalScalar01"
optionalScalar02 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar02" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar02 = Data.ProtoLens.Field.field @"optionalScalar02"
optionalScalar03 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar03" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar03 = Data.ProtoLens.Field.field @"optionalScalar03"
optionalScalar04 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar04" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar04 = Data.ProtoLens.Field.field @"optionalScalar04"
optionalScalar05 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar05" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar05 = Data.ProtoLens.Field.field @"optionalScalar05"
optionalScalar06 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar06" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar06 = Data.ProtoLens.Field.field @"optionalScalar06"
optionalScalar07 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar07" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar07 = Data.ProtoLens.Field.field @"optionalScalar07"
optionalScalar08 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar08" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar08 = Data.ProtoLens.Field.field @"optionalScalar08"
optionalScalar09 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar09" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar09 = Data.ProtoLens.Field.field @"optionalScalar09"
optionalScalar10 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar10" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar10 = Data.ProtoLens.Field.field @"optionalScalar10"
optionalScalar11 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar11" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar11 = Data.ProtoLens.Field.field @"optionalScalar11"
optionalScalar12 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar12" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar12 = Data.ProtoLens.Field.field @"optionalScalar12"
optionalScalar13 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar13" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar13 = Data.ProtoLens.Field.field @"optionalScalar13"
optionalScalar14 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar14" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar14 = Data.ProtoLens.Field.field @"optionalScalar14"
optionalScalar15 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "optionalScalar15" a) =>
  Lens.Family2.LensLike' f s a
optionalScalar15 = Data.ProtoLens.Field.field @"optionalScalar15"
repeatedAnother ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedAnother" a) =>
  Lens.Family2.LensLike' f s a
repeatedAnother = Data.ProtoLens.Field.field @"repeatedAnother"
repeatedEnum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedEnum" a) =>
  Lens.Family2.LensLike' f s a
repeatedEnum = Data.ProtoLens.Field.field @"repeatedEnum"
repeatedNested ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedNested" a) =>
  Lens.Family2.LensLike' f s a
repeatedNested = Data.ProtoLens.Field.field @"repeatedNested"
repeatedScalar01 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar01" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar01 = Data.ProtoLens.Field.field @"repeatedScalar01"
repeatedScalar02 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar02" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar02 = Data.ProtoLens.Field.field @"repeatedScalar02"
repeatedScalar03 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar03" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar03 = Data.ProtoLens.Field.field @"repeatedScalar03"
repeatedScalar04 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar04" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar04 = Data.ProtoLens.Field.field @"repeatedScalar04"
repeatedScalar05 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar05" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar05 = Data.ProtoLens.Field.field @"repeatedScalar05"
repeatedScalar06 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar06" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar06 = Data.ProtoLens.Field.field @"repeatedScalar06"
repeatedScalar07 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar07" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar07 = Data.ProtoLens.Field.field @"repeatedScalar07"
repeatedScalar08 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar08" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar08 = Data.ProtoLens.Field.field @"repeatedScalar08"
repeatedScalar09 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar09" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar09 = Data.ProtoLens.Field.field @"repeatedScalar09"
repeatedScalar10 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar10" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar10 = Data.ProtoLens.Field.field @"repeatedScalar10"
repeatedScalar11 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar11" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar11 = Data.ProtoLens.Field.field @"repeatedScalar11"
repeatedScalar12 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar12" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar12 = Data.ProtoLens.Field.field @"repeatedScalar12"
repeatedScalar13 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar13" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar13 = Data.ProtoLens.Field.field @"repeatedScalar13"
repeatedScalar14 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar14" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar14 = Data.ProtoLens.Field.field @"repeatedScalar14"
repeatedScalar15 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "repeatedScalar15" a) =>
  Lens.Family2.LensLike' f s a
repeatedScalar15 = Data.ProtoLens.Field.field @"repeatedScalar15"
value ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
  Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
vec'repeatedAnother ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedAnother" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedAnother
  = Data.ProtoLens.Field.field @"vec'repeatedAnother"
vec'repeatedEnum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedEnum" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedEnum = Data.ProtoLens.Field.field @"vec'repeatedEnum"
vec'repeatedNested ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedNested" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedNested
  = Data.ProtoLens.Field.field @"vec'repeatedNested"
vec'repeatedScalar01 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar01" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar01
  = Data.ProtoLens.Field.field @"vec'repeatedScalar01"
vec'repeatedScalar02 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar02" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar02
  = Data.ProtoLens.Field.field @"vec'repeatedScalar02"
vec'repeatedScalar03 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar03" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar03
  = Data.ProtoLens.Field.field @"vec'repeatedScalar03"
vec'repeatedScalar04 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar04" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar04
  = Data.ProtoLens.Field.field @"vec'repeatedScalar04"
vec'repeatedScalar05 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar05" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar05
  = Data.ProtoLens.Field.field @"vec'repeatedScalar05"
vec'repeatedScalar06 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar06" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar06
  = Data.ProtoLens.Field.field @"vec'repeatedScalar06"
vec'repeatedScalar07 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar07" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar07
  = Data.ProtoLens.Field.field @"vec'repeatedScalar07"
vec'repeatedScalar08 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar08" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar08
  = Data.ProtoLens.Field.field @"vec'repeatedScalar08"
vec'repeatedScalar09 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar09" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar09
  = Data.ProtoLens.Field.field @"vec'repeatedScalar09"
vec'repeatedScalar10 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar10" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar10
  = Data.ProtoLens.Field.field @"vec'repeatedScalar10"
vec'repeatedScalar11 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar11" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar11
  = Data.ProtoLens.Field.field @"vec'repeatedScalar11"
vec'repeatedScalar12 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar12" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar12
  = Data.ProtoLens.Field.field @"vec'repeatedScalar12"
vec'repeatedScalar13 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar13" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar13
  = Data.ProtoLens.Field.field @"vec'repeatedScalar13"
vec'repeatedScalar14 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar14" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar14
  = Data.ProtoLens.Field.field @"vec'repeatedScalar14"
vec'repeatedScalar15 ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'repeatedScalar15" a) =>
  Lens.Family2.LensLike' f s a
vec'repeatedScalar15
  = Data.ProtoLens.Field.field @"vec'repeatedScalar15"