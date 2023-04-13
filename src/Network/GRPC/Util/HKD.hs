{-# LANGUAGE UndecidableSuperClasses #-}

-- | Thin layer around generics-sop to support HDK types
--
-- TODO: This should be merged to generics-sop.
--
-- Intended for qualified import.
module Network.GRPC.Util.HKD (
    -- * Translate to and from generic representation
    IsHKD
  , Code
  , from
  , to
    -- * Combinators
  , hsequence
  , hsequence'
  ) where

import Data.Kind
import Data.SOP hiding (hsequence, hsequence')
import Generics.SOP hiding (Code, to, from, hsequence, hsequence')
import Generics.SOP qualified as SOP

{-------------------------------------------------------------------------------
  Code
-------------------------------------------------------------------------------}

data Skolem (a :: Type)

type Code (a :: (Type -> Type) -> Type) = StripTypeArg (SOP.Code (a Skolem))

type family StripTypeArg (code :: [[Type]]) :: [[Type]] where
  StripTypeArg '[]         = '[]
  StripTypeArg (xs ': xss) = StripTypeArg' xs ': StripTypeArg xss

type family StripTypeArg' (prod :: [Type]) :: [Type] where
  StripTypeArg' '[]       = '[]
  StripTypeArg' (x ': xs) = StripSkolem x ': StripTypeArg' xs

type family StripSkolem (x :: Type) :: Type where
  StripSkolem (Skolem x) = x

{-------------------------------------------------------------------------------
  From/to generic representation
-------------------------------------------------------------------------------}

class    x ~ f y => IsAppOf f x y
instance x ~ f y => IsAppOf f x y

class    y ~ f x => IsFlipAppOf f x y
instance y ~ f x => IsFlipAppOf f x y

class    ( Generic (a f)
         , All SListI (Code a)
         , AllZip2 (IsAppOf f) (SOP.Code (a f)) (Code a)
         , AllZip2 (IsFlipAppOf f) (Code a) (SOP.Code (a f))
         ) => AllIsAppOf a f
instance ( Generic (a f)
         , All SListI (Code a)
         , AllZip2 (IsAppOf f) (SOP.Code (a f)) (Code a)
         , AllZip2 (IsFlipAppOf f) (Code a) (SOP.Code (a f))
         ) => AllIsAppOf a f

type IsHKD a = forall f. AllIsAppOf a f

from :: forall a f. IsHKD a => a f -> SOP f (Code a)
from = SOP.htrans (Proxy @(IsAppOf f)) unI . SOP.from

to :: forall a f. IsHKD a => SOP f (Code a) -> a f
to = SOP.to . SOP.htrans (Proxy @(IsFlipAppOf f)) I

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

hsequence :: (IsHKD a, Applicative f) => a f -> f (a I)
hsequence = fmap to . SOP.hsequence . from

hsequence' :: (IsHKD a, Applicative f) => a (f :.: g) -> f (a g)
hsequence' = fmap to . SOP.hsequence' . from

