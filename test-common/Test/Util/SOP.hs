{-# LANGUAGE PolyKinds #-}

module Test.Util.SOP (
    -- * Indices
    Ix(..)
  , indexNP
    -- * Updates
  , Update(..)
  , updateAt
  , updateAtM
  ) where

import Data.Kind
import Data.SOP

{-------------------------------------------------------------------------------
  Indices
-------------------------------------------------------------------------------}

data Ix :: [k] -> k -> Type where
  IZero :: Ix (a ': as) a
  ISucc :: Ix as a -> Ix (a ': as) a

deriving stock instance Show (Ix as a)

indexNP :: Ix as a -> NP f as -> f a
indexNP IZero     (x :* _ ) = x
indexNP (ISucc i) (_ :* xs) = indexNP i xs

{-------------------------------------------------------------------------------
  Update
-------------------------------------------------------------------------------}

data Update :: (k -> Type) -> k -> k -> [k] -> [k] -> Type where
  UZero :: Update f a b (a ': cs) (b ': cs)
  USucc :: Update f a b bs cs -> Update f a b (a ': bs) (a ': cs)

deriving stock instance Show (Update f a b as bs)

updateAt :: (f a -> f b) -> Update f a b as bs -> NP f as -> NP f bs
updateAt f  UZero    (x :* xs) = f x :* xs
updateAt f (USucc i) (x :* xs) =   x :* updateAt f i xs

updateAtM :: forall m f a b as bs.
     Functor m
  => (f a -> m (f b)) -> Update f a b as bs -> NP f as -> m (NP f bs)
updateAtM f = go
  where
    go :: forall as' bs'. Update f a b as' bs' -> NP f as' -> m (NP f bs')
    go  UZero    (x :* xs) = ( :* xs) <$> f x
    go (USucc i) (x :* xs) = (x :*)   <$> go i xs
