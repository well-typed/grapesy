{-# LANGUAGE OverloadedRecordDot #-}

module Test.OverloadedRecordDot (tests) where

import Data.ByteString qualified as Strict (ByteString)
import Data.Int
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Vector qualified as Boxed (Vector)
import Data.Vector qualified as Vector.Boxed
import Data.Vector.Unboxed qualified as Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector.Unboxed
import Data.Word

import Network.GRPC.Common.Protobuf

import Test.Tasty
import Test.Tasty.HUnit

import Proto.Spec

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.OverloadedRecordDot" [
      testCase "implicit" test_implicit
    , testCase "optional" test_optional
    , testCase "repeated" test_repeated
    , testCase "map"      test_map
    , testCase "oneof"    test_oneof
    ]

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

-- | Fields without a label ("implicit field presence")
--
-- Not sure why, but protoc generates 'Maybe' field accessors for these
-- fields but only in the case of nested messages; not for scalars or enums.
-- Interestingly, these fields are then considered to be 'Nothing' when
-- the value is 'defMessage'.
test_implicit :: Assertion
test_implicit = do
    assertEqual "defaultScalar01" (fieldDefault :: Double)                             $ exampleMessage.defaultScalar01
    assertEqual "defaultScalar02" (fieldDefault :: Float)                              $ exampleMessage.defaultScalar02
    assertEqual "defaultScalar03" (fieldDefault :: Int32)                              $ exampleMessage.defaultScalar03
    assertEqual "defaultScalar04" (fieldDefault :: Int64)                              $ exampleMessage.defaultScalar04
    assertEqual "defaultScalar05" (fieldDefault :: Word32)                             $ exampleMessage.defaultScalar05
    assertEqual "defaultScalar06" (fieldDefault :: Word64)                             $ exampleMessage.defaultScalar06
    assertEqual "defaultScalar07" (fieldDefault :: Int32)                              $ exampleMessage.defaultScalar07
    assertEqual "defaultScalar08" (fieldDefault :: Int64)                              $ exampleMessage.defaultScalar08
    assertEqual "defaultScalar09" (fieldDefault :: Word32)                             $ exampleMessage.defaultScalar09
    assertEqual "defaultScalar10" (fieldDefault :: Word64)                             $ exampleMessage.defaultScalar10
    assertEqual "defaultScalar11" (fieldDefault :: Int32)                              $ exampleMessage.defaultScalar11
    assertEqual "defaultScalar12" (fieldDefault :: Int64)                              $ exampleMessage.defaultScalar12
    assertEqual "defaultScalar13" (fieldDefault :: Bool)                               $ exampleMessage.defaultScalar13
    assertEqual "defaultScalar14" (fieldDefault :: Text)                               $ exampleMessage.defaultScalar14
    assertEqual "defaultScalar15" (fieldDefault :: Strict.ByteString)                  $ exampleMessage.defaultScalar15
    assertEqual "defaultScalar15" (fieldDefault :: Strict.ByteString)                  $ exampleMessage.defaultScalar15
    assertEqual "defaultAnother"  (defMessage   :: Proto AnotherMessage)               $ exampleMessage.defaultAnother
    assertEqual "defaultNested"   (defMessage   :: Proto ExampleMessage'NestedMessage) $ exampleMessage.defaultNested
    assertEqual "defaultEnum"     (fieldDefault :: Proto ExampleEnum)                  $ exampleMessage.defaultEnum

    assertEqual "maybe'defaultAnother"  (Nothing :: Maybe (Proto AnotherMessage))               $ exampleMessage.maybe'defaultAnother
    assertEqual "maybe'defaultNested"   (Nothing :: Maybe (Proto ExampleMessage'NestedMessage)) $ exampleMessage.maybe'defaultNested

-- | @optional@ fields
--
-- @protoc@, confusingly imho, generates the /same/ 'HasField' instances for
-- optional field as it does for non-optional fields, supplying a default
-- value if the field is absent. The actual maybe has a prefixed name.
test_optional :: Assertion
test_optional = do
    assertEqual "optionalScalar01" (fieldDefault :: Double)                             $ exampleMessage.optionalScalar01
    assertEqual "optionalScalar02" (fieldDefault :: Float)                              $ exampleMessage.optionalScalar02
    assertEqual "optionalScalar03" (fieldDefault :: Int32)                              $ exampleMessage.optionalScalar03
    assertEqual "optionalScalar04" (fieldDefault :: Int64)                              $ exampleMessage.optionalScalar04
    assertEqual "optionalScalar05" (fieldDefault :: Word32)                             $ exampleMessage.optionalScalar05
    assertEqual "optionalScalar06" (fieldDefault :: Word64)                             $ exampleMessage.optionalScalar06
    assertEqual "optionalScalar07" (fieldDefault :: Int32)                              $ exampleMessage.optionalScalar07
    assertEqual "optionalScalar08" (fieldDefault :: Int64)                              $ exampleMessage.optionalScalar08
    assertEqual "optionalScalar09" (fieldDefault :: Word32)                             $ exampleMessage.optionalScalar09
    assertEqual "optionalScalar10" (fieldDefault :: Word64)                             $ exampleMessage.optionalScalar10
    assertEqual "optionalScalar11" (fieldDefault :: Int32)                              $ exampleMessage.optionalScalar11
    assertEqual "optionalScalar12" (fieldDefault :: Int64)                              $ exampleMessage.optionalScalar12
    assertEqual "optionalScalar13" (fieldDefault :: Bool)                               $ exampleMessage.optionalScalar13
    assertEqual "optionalScalar14" (fieldDefault :: Text)                               $ exampleMessage.optionalScalar14
    assertEqual "optionalScalar15" (fieldDefault :: Strict.ByteString)                  $ exampleMessage.optionalScalar15
    assertEqual "optionalAnother"  (defMessage   :: Proto AnotherMessage)               $ exampleMessage.optionalAnother
    assertEqual "optionalNested"   (defMessage   :: Proto ExampleMessage'NestedMessage) $ exampleMessage.optionalNested
    assertEqual "optionalEnum"     (fieldDefault :: Proto ExampleEnum)                  $ exampleMessage.optionalEnum

    assertEqual "maybe'optionalScalar01" (Nothing :: Maybe Double)                               $ exampleMessage.maybe'optionalScalar01
    assertEqual "maybe'optionalScalar02" (Nothing :: Maybe Float)                                $ exampleMessage.maybe'optionalScalar02
    assertEqual "maybe'optionalScalar03" (Nothing :: Maybe Int32)                                $ exampleMessage.maybe'optionalScalar03
    assertEqual "maybe'optionalScalar04" (Nothing :: Maybe Int64)                                $ exampleMessage.maybe'optionalScalar04
    assertEqual "maybe'optionalScalar05" (Nothing :: Maybe Word32)                               $ exampleMessage.maybe'optionalScalar05
    assertEqual "maybe'optionalScalar06" (Nothing :: Maybe Word64)                               $ exampleMessage.maybe'optionalScalar06
    assertEqual "maybe'optionalScalar07" (Nothing :: Maybe Int32)                                $ exampleMessage.maybe'optionalScalar07
    assertEqual "maybe'optionalScalar08" (Nothing :: Maybe Int64)                                $ exampleMessage.maybe'optionalScalar08
    assertEqual "maybe'optionalScalar09" (Nothing :: Maybe Word32)                               $ exampleMessage.maybe'optionalScalar09
    assertEqual "maybe'optionalScalar10" (Nothing :: Maybe Word64)                               $ exampleMessage.maybe'optionalScalar10
    assertEqual "maybe'optionalScalar11" (Nothing :: Maybe Int32)                                $ exampleMessage.maybe'optionalScalar11
    assertEqual "maybe'optionalScalar12" (Nothing :: Maybe Int64)                                $ exampleMessage.maybe'optionalScalar12
    assertEqual "maybe'optionalScalar13" (Nothing :: Maybe Bool)                                 $ exampleMessage.maybe'optionalScalar13
    assertEqual "maybe'optionalScalar14" (Nothing :: Maybe Text)                                 $ exampleMessage.maybe'optionalScalar14
    assertEqual "maybe'optionalScalar15" (Nothing :: Maybe Strict.ByteString)                    $ exampleMessage.maybe'optionalScalar15
    assertEqual "maybe'optionalAnother"  (Nothing :: Maybe (Proto AnotherMessage))               $ exampleMessage.maybe'optionalAnother
    assertEqual "maybe'optionalNested"   (Nothing :: Maybe (Proto ExampleMessage'NestedMessage)) $ exampleMessage.maybe'optionalNested
    assertEqual "maybe'optionalEnum"     (Nothing :: Maybe (Proto ExampleEnum))                  $ exampleMessage.maybe'optionalEnum

-- | @repeated@ fields
test_repeated :: Assertion
test_repeated = do
    assertEqual "repeatedScalar01" ([] :: [Double])                             $ exampleMessage.repeatedScalar01
    assertEqual "repeatedScalar02" ([] :: [Float])                              $ exampleMessage.repeatedScalar02
    assertEqual "repeatedScalar03" ([] :: [Int32])                              $ exampleMessage.repeatedScalar03
    assertEqual "repeatedScalar04" ([] :: [Int64])                              $ exampleMessage.repeatedScalar04
    assertEqual "repeatedScalar05" ([] :: [Word32])                             $ exampleMessage.repeatedScalar05
    assertEqual "repeatedScalar06" ([] :: [Word64])                             $ exampleMessage.repeatedScalar06
    assertEqual "repeatedScalar07" ([] :: [Int32])                              $ exampleMessage.repeatedScalar07
    assertEqual "repeatedScalar08" ([] :: [Int64])                              $ exampleMessage.repeatedScalar08
    assertEqual "repeatedScalar09" ([] :: [Word32])                             $ exampleMessage.repeatedScalar09
    assertEqual "repeatedScalar10" ([] :: [Word64])                             $ exampleMessage.repeatedScalar10
    assertEqual "repeatedScalar11" ([] :: [Int32])                              $ exampleMessage.repeatedScalar11
    assertEqual "repeatedScalar12" ([] :: [Int64])                              $ exampleMessage.repeatedScalar12
    assertEqual "repeatedScalar13" ([] :: [Bool])                               $ exampleMessage.repeatedScalar13
    assertEqual "repeatedScalar14" ([] :: [Text])                               $ exampleMessage.repeatedScalar14
    assertEqual "repeatedScalar15" ([] :: [Strict.ByteString])                  $ exampleMessage.repeatedScalar15
    assertEqual "repeatedAnother"  ([] :: [Proto AnotherMessage])               $ exampleMessage.repeatedAnother
    assertEqual "repeatedNested"   ([] :: [Proto ExampleMessage'NestedMessage]) $ exampleMessage.repeatedNested
    assertEqual "repeatedEnum"     ([] :: [Proto ExampleEnum])                  $ exampleMessage.repeatedEnum

    assertEqual "vec'repeatedScalar01" (Vector.Unboxed.empty :: Unboxed.Vector Double)                             $ exampleMessage.vec'repeatedScalar01
    assertEqual "vec'repeatedScalar02" (Vector.Unboxed.empty :: Unboxed.Vector Float)                              $ exampleMessage.vec'repeatedScalar02
    assertEqual "vec'repeatedScalar03" (Vector.Unboxed.empty :: Unboxed.Vector Int32)                              $ exampleMessage.vec'repeatedScalar03
    assertEqual "vec'repeatedScalar04" (Vector.Unboxed.empty :: Unboxed.Vector Int64)                              $ exampleMessage.vec'repeatedScalar04
    assertEqual "vec'repeatedScalar05" (Vector.Unboxed.empty :: Unboxed.Vector Word32)                             $ exampleMessage.vec'repeatedScalar05
    assertEqual "vec'repeatedScalar06" (Vector.Unboxed.empty :: Unboxed.Vector Word64)                             $ exampleMessage.vec'repeatedScalar06
    assertEqual "vec'repeatedScalar07" (Vector.Unboxed.empty :: Unboxed.Vector Int32)                              $ exampleMessage.vec'repeatedScalar07
    assertEqual "vec'repeatedScalar08" (Vector.Unboxed.empty :: Unboxed.Vector Int64)                              $ exampleMessage.vec'repeatedScalar08
    assertEqual "vec'repeatedScalar09" (Vector.Unboxed.empty :: Unboxed.Vector Word32)                             $ exampleMessage.vec'repeatedScalar09
    assertEqual "vec'repeatedScalar10" (Vector.Unboxed.empty :: Unboxed.Vector Word64)                             $ exampleMessage.vec'repeatedScalar10
    assertEqual "vec'repeatedScalar11" (Vector.Unboxed.empty :: Unboxed.Vector Int32)                              $ exampleMessage.vec'repeatedScalar11
    assertEqual "vec'repeatedScalar12" (Vector.Unboxed.empty :: Unboxed.Vector Int64)                              $ exampleMessage.vec'repeatedScalar12
    assertEqual "vec'repeatedScalar13" (Vector.Unboxed.empty :: Unboxed.Vector Bool)                               $ exampleMessage.vec'repeatedScalar13
    assertEqual "vec'repeatedScalar14" (Vector.Boxed.empty   :: Boxed.Vector Text)                                 $ exampleMessage.vec'repeatedScalar14
    assertEqual "vec'repeatedScalar15" (Vector.Boxed.empty   :: Boxed.Vector Strict.ByteString)                    $ exampleMessage.vec'repeatedScalar15
    assertEqual "vec'repeatedAnother"  (Vector.Boxed.empty   :: Boxed.Vector (Proto AnotherMessage))               $ exampleMessage.vec'repeatedAnother
    assertEqual "vec'repeatedNested"   (Vector.Boxed.empty   :: Boxed.Vector (Proto ExampleMessage'NestedMessage)) $ exampleMessage.vec'repeatedNested
    assertEqual "vec'repeatedEnum"     (Vector.Boxed.empty   :: Boxed.Vector (Proto ExampleEnum))                  $ exampleMessage.vec'repeatedEnum

-- | @map@ fields
test_map :: Assertion
test_map = do
    assertEqual "mapScalar01" (Map.empty :: Map Text Double)                               $ exampleMessage.mapScalar01
    assertEqual "mapScalar02" (Map.empty :: Map Text Float)                                $ exampleMessage.mapScalar02
    assertEqual "mapScalar03" (Map.empty :: Map Text Int32)                                $ exampleMessage.mapScalar03
    assertEqual "mapScalar04" (Map.empty :: Map Text Int64)                                $ exampleMessage.mapScalar04
    assertEqual "mapScalar05" (Map.empty :: Map Text Word32)                               $ exampleMessage.mapScalar05
    assertEqual "mapScalar06" (Map.empty :: Map Text Word64)                               $ exampleMessage.mapScalar06
    assertEqual "mapScalar07" (Map.empty :: Map Text Int32)                                $ exampleMessage.mapScalar07
    assertEqual "mapScalar08" (Map.empty :: Map Text Int64)                                $ exampleMessage.mapScalar08
    assertEqual "mapScalar09" (Map.empty :: Map Text Word32)                               $ exampleMessage.mapScalar09
    assertEqual "mapScalar10" (Map.empty :: Map Text Word64)                               $ exampleMessage.mapScalar10
    assertEqual "mapScalar11" (Map.empty :: Map Text Int32)                                $ exampleMessage.mapScalar11
    assertEqual "mapScalar12" (Map.empty :: Map Text Int64)                                $ exampleMessage.mapScalar12
    assertEqual "mapScalar13" (Map.empty :: Map Text Bool)                                 $ exampleMessage.mapScalar13
    assertEqual "mapScalar14" (Map.empty :: Map Text Text)                                 $ exampleMessage.mapScalar14
    assertEqual "mapScalar15" (Map.empty :: Map Text Strict.ByteString)                    $ exampleMessage.mapScalar15
    assertEqual "mapAnother"  (Map.empty :: Map Text (Proto AnotherMessage))               $ exampleMessage.mapAnother
    assertEqual "mapNested"   (Map.empty :: Map Text (Proto ExampleMessage'NestedMessage)) $ exampleMessage.mapNested
    assertEqual "mapEnum"     (Map.empty :: Map Text (Proto ExampleEnum))                  $ exampleMessage.mapEnum

-- | @oneof@ fields
--
-- The fact that these fields are mutually exclusive is not at all visible from
-- this test.
test_oneof :: Assertion
test_oneof = do
    assertEqual "oneofScalar01" (fieldDefault :: Double)                             $ exampleMessage.oneofScalar01
    assertEqual "oneofScalar02" (fieldDefault :: Float)                              $ exampleMessage.oneofScalar02
    assertEqual "oneofScalar03" (fieldDefault :: Int32)                              $ exampleMessage.oneofScalar03
    assertEqual "oneofScalar04" (fieldDefault :: Int64)                              $ exampleMessage.oneofScalar04
    assertEqual "oneofScalar05" (fieldDefault :: Word32)                             $ exampleMessage.oneofScalar05
    assertEqual "oneofScalar06" (fieldDefault :: Word64)                             $ exampleMessage.oneofScalar06
    assertEqual "oneofScalar07" (fieldDefault :: Int32)                              $ exampleMessage.oneofScalar07
    assertEqual "oneofScalar08" (fieldDefault :: Int64)                              $ exampleMessage.oneofScalar08
    assertEqual "oneofScalar09" (fieldDefault :: Word32)                             $ exampleMessage.oneofScalar09
    assertEqual "oneofScalar10" (fieldDefault :: Word64)                             $ exampleMessage.oneofScalar10
    assertEqual "oneofScalar11" (fieldDefault :: Int32)                              $ exampleMessage.oneofScalar11
    assertEqual "oneofScalar12" (fieldDefault :: Int64)                              $ exampleMessage.oneofScalar12
    assertEqual "oneofScalar13" (fieldDefault :: Bool)                               $ exampleMessage.oneofScalar13
    assertEqual "oneofScalar14" (fieldDefault :: Text)                               $ exampleMessage.oneofScalar14
    assertEqual "oneofScalar15" (fieldDefault :: Strict.ByteString)                  $ exampleMessage.oneofScalar15
    assertEqual "oneofAnother"  (defMessage   :: Proto AnotherMessage)               $ exampleMessage.oneofAnother
    assertEqual "oneofNested"   (defMessage   :: Proto ExampleMessage'NestedMessage) $ exampleMessage.oneofNested
    assertEqual "oneofEnum"     (fieldDefault :: Proto ExampleEnum)                  $ exampleMessage.oneofEnum

    assertEqual "maybe'oneofScalar01" (Nothing :: Maybe Double)                               $ exampleMessage.maybe'oneofScalar01
    assertEqual "maybe'oneofScalar02" (Nothing :: Maybe Float)                                $ exampleMessage.maybe'oneofScalar02
    assertEqual "maybe'oneofScalar03" (Nothing :: Maybe Int32)                                $ exampleMessage.maybe'oneofScalar03
    assertEqual "maybe'oneofScalar04" (Nothing :: Maybe Int64)                                $ exampleMessage.maybe'oneofScalar04
    assertEqual "maybe'oneofScalar05" (Nothing :: Maybe Word32)                               $ exampleMessage.maybe'oneofScalar05
    assertEqual "maybe'oneofScalar06" (Nothing :: Maybe Word64)                               $ exampleMessage.maybe'oneofScalar06
    assertEqual "maybe'oneofScalar07" (Nothing :: Maybe Int32)                                $ exampleMessage.maybe'oneofScalar07
    assertEqual "maybe'oneofScalar08" (Nothing :: Maybe Int64)                                $ exampleMessage.maybe'oneofScalar08
    assertEqual "maybe'oneofScalar09" (Nothing :: Maybe Word32)                               $ exampleMessage.maybe'oneofScalar09
    assertEqual "maybe'oneofScalar10" (Nothing :: Maybe Word64)                               $ exampleMessage.maybe'oneofScalar10
    assertEqual "maybe'oneofScalar11" (Nothing :: Maybe Int32)                                $ exampleMessage.maybe'oneofScalar11
    assertEqual "maybe'oneofScalar12" (Nothing :: Maybe Int64)                                $ exampleMessage.maybe'oneofScalar12
    assertEqual "maybe'oneofScalar13" (Nothing :: Maybe Bool)                                 $ exampleMessage.maybe'oneofScalar13
    assertEqual "maybe'oneofScalar14" (Nothing :: Maybe Text)                                 $ exampleMessage.maybe'oneofScalar14
    assertEqual "maybe'oneofScalar15" (Nothing :: Maybe Strict.ByteString)                    $ exampleMessage.maybe'oneofScalar15
    assertEqual "maybe'oneofAnother"  (Nothing :: Maybe (Proto AnotherMessage))               $ exampleMessage.maybe'oneofAnother
    assertEqual "maybe'oneofNested"   (Nothing :: Maybe (Proto ExampleMessage'NestedMessage)) $ exampleMessage.maybe'oneofNested
    assertEqual "maybe'oneofEnum"     (Nothing :: Maybe (Proto ExampleEnum))                  $ exampleMessage.maybe'oneofEnum

    --
    -- Accessing the oneof as its own field
    --
    -- It kind of makes sense that we wrap this in @Proto@, as it has the same
    -- 'HasField' instances. Most users would probably never access this field
    -- in this way anyway.
    --

    assertEqual "maybe'exampleOneOf" (Nothing :: Maybe (Proto ExampleMessage'ExampleOneOf)) $ exampleMessage.maybe'exampleOneOf

exampleMessage :: Proto ExampleMessage
exampleMessage = defMessage