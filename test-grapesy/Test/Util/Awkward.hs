module Test.Util.Awkward (
    Awkward(..)
    -- * Support for defining instances
  , awkward
  , arbitraryAwkward
  , shrinkAwkward
  ) where

import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
  arbitrary = Awkward <$> BS.Strict.pack <$> arbitrary
  shrink    = map (Awkward . BS.Strict.pack)
            . shrink
            . BS.Strict.unpack . getAwkward

instance {-# OVERLAPPING #-} Arbitrary (Awkward String) where
  arbitrary = Awkward <$> arbitrary
  shrink    = map Awkward . shrink . getAwkward