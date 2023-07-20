module Test.Prop.Serialization (tests) where

import Control.Monad.Except
import Test.Tasty
import Test.Tasty.QuickCheck

import Network.GRPC.Spec
import Data.ByteString qualified as Strict

tests :: TestTree
tests = testGroup "Test.Prop.Serialization" [
      testGroup "Roundtrip" [
          testProperty "CustomMetadata" $
            roundtrip buildCustomMetadata parseCustomMetadata
        ]
    ]

{-------------------------------------------------------------------------------
  Roundtrip tests
-------------------------------------------------------------------------------}

roundtrip :: forall a b.
     (Eq a, Show a, Show b)
  => (a -> b) -> (b -> Except String a) -> Awkward a -> Property
roundtrip there back (Awkward a) =
    counterexample (show b) $
      runExcept (back b) === Right a
  where
    b :: b
    b = there a

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

-- | Newtype wrapper for \"awkward\" 'Arbitrary' instances
--
-- In most property tests we don't explore edge cases, preferring for example
-- to use only simple header names, rather than check encoding issues. But in
-- these serialization tests the edge cases are of course important.
newtype Awkward a = Awkward { getAwkward :: a }
  deriving (Show)

awkward :: Arbitrary (Awkward a) => Gen a
awkward = getAwkward <$> arbitrary

instance Arbitrary (Awkward CustomMetadata) where
  arbitrary = Awkward <$> do
      name <- awkward
      oneof [
          BinaryHeader name <$> awkward
        , AsciiHeader  name <$> awkward
        ]

instance Arbitrary (Awkward HeaderName) where
  arbitrary = Awkward <$>
      suchThatMap (Strict.pack <$> arbitrary) safeHeaderName

instance Arbitrary (Awkward BinaryValue) where
  arbitrary = Awkward <$>
      BinaryValue . Strict.pack <$> arbitrary

instance Arbitrary (Awkward AsciiValue) where
  arbitrary = Awkward <$>
      suchThatMap (Strict.pack <$> arbitrary) safeAsciiValue

