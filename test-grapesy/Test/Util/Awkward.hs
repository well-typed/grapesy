module Test.Util.Awkward (
    Awkward(..)
    -- * Support for defining instances
  , awkward
  , arbitraryAwkward
  , shrinkAwkward
  , shrinkRegular
  ) where

import Control.Monad
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.Char (ord, chr)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word
import Test.QuickCheck
import Test.QuickCheck.Instances ()

{-------------------------------------------------------------------------------
  \"Awkward\" instances
-------------------------------------------------------------------------------}

-- | Newtype wrapper for \"awkward\" 'Arbitrary' instances
--
-- In most property tests we don't explore edge cases, preferring for example
-- to use only simple header names, rather than check encoding issues. But in
-- these serialization tests the edge cases are of course important.
newtype Awkward a = Awkward { getAwkward :: a }
  deriving stock (Show, Functor)

awkward :: Arbitrary (Awkward a) => Gen a
awkward = getAwkward <$> arbitrary

arbitraryAwkward :: Arbitrary (Awkward a) => (a -> b) -> Gen (Awkward b)
arbitraryAwkward f = Awkward . f <$> awkward

shrinkAwkward ::
     Arbitrary (Awkward a)
  => (a -> b)
  -> (b -> a)
  -> Awkward b -> [Awkward b]
shrinkAwkward f g =
      map (Awkward . f . getAwkward)
    . shrink
    . Awkward . g . getAwkward

-- | Non-awkward fields of awkward types
shrinkRegular ::
     Arbitrary a
  => (a -> b)
  -> (b -> a)
  -> Awkward b -> [Awkward b]
shrinkRegular f g = map (Awkward . f) . shrink . g . getAwkward

{-------------------------------------------------------------------------------
  Standard instances
-------------------------------------------------------------------------------}

distrib :: Functor f => f (Awkward a) -> Awkward (f a)
distrib = Awkward . fmap getAwkward

undistrib :: Functor f => Awkward (f a) -> f (Awkward a)
undistrib = fmap Awkward . getAwkward

instance Arbitrary (Awkward a) => Arbitrary (Awkward (Maybe a)) where
  arbitrary = distrib <$> arbitrary
  shrink    = map distrib . shrink . undistrib

instance Arbitrary (Awkward a) => Arbitrary (Awkward [a]) where
  arbitrary = distrib <$> arbitrary
  shrink    = map distrib . shrink . undistrib

instance Arbitrary (Awkward a) => Arbitrary (Awkward (NonEmpty a)) where
  arbitrary = distrib <$> arbitrary
  shrink    = map distrib . shrink . undistrib

instance ( Arbitrary (Awkward a)
         , Arbitrary (Awkward b)
         ) => Arbitrary (Awkward (a, b)) where
  arbitrary = fmap Awkward $ (,) <$> awkward <*> awkward
  shrink pair@(Awkward (x, y)) = concat [
        shrinkAwkward (\x' -> (x', y )) fst pair
      , shrinkAwkward (\y' -> (x , y')) snd pair
      ]

instance ( Arbitrary (Awkward k)
         , Arbitrary (Awkward v)
         , Ord k
         ) => Arbitrary (Awkward (Map k v)) where
  arbitrary = arbitraryAwkward Map.fromList
  shrink    = shrinkAwkward    Map.fromList Map.toList

instance Arbitrary (Awkward Strict.ByteString) where
  arbitrary = sized $ \sz -> Awkward <$> do
      n <- choose (0, sz)
      BS.Strict.pack <$> replicateM n genChar
    where
      -- Generate commas with relatively high probability, so that the tests
      -- for duplicate headers have enough material to work with.
      genChar :: Gen Word8
      genChar = frequency [
            (9, arbitrary)
          , (1, pure $ fromIntegral (ord ','))
          ]

  shrink =
        map (Awkward . BS.Strict.pack)
      . shrinkList shrinkChar
      . BS.Strict.unpack . getAwkward
    where
      -- shrink as 'Char'
      shrinkChar :: Word8 -> [Word8]
      shrinkChar = map (fromIntegral . ord) . shrink . (chr . fromIntegral)

instance {-# OVERLAPPING #-} Arbitrary (Awkward String) where
  arbitrary = Awkward <$> arbitrary
  shrink    = map Awkward . shrink . getAwkward

instance Arbitrary (Awkward Text) where
  arbitrary = Awkward . Text.pack . getAwkward <$> arbitrary
  shrink    = map (Awkward . Text.pack) . shrink . (Text.unpack . getAwkward)

instance Arbitrary (Awkward Double) where
  arbitrary = Awkward <$> arbitrary
  shrink    = map Awkward . shrink . getAwkward