-- | Very minimal support for HKD
--
-- Intended for qualified import.
--
-- import Network.GRPC.Util.HKD (HKD, Undecorated, DecoratedWith)
-- import Network.GRPC.Util.HKD qualified as HKD
module Network.GRPC.Util.HKD (
    -- * Definition
    HKD
  , DecoratedWith
  , Undecorated
    -- * Dealing with HKD records
  , Coerce(..)
  , Traversable(..)
  , sequence
  , map
    -- * Dealing with HKD fields
  , ValidDecoration
  , pure
    -- * Error decorations
  , Checked
  , sequenceChecked
  ) where

import Prelude hiding (Traversable(..), pure, map)
import Prelude qualified

import Control.Monad.Except (MonadError, throwError)
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Undecorated                       (x :: Type)
data DecoratedWith (f :: Type -> Type) (x :: Type)

type family HKD (f :: Type -> Type) (x :: Type) :: Type

type instance HKD Undecorated       x = x
type instance HKD (DecoratedWith f) x = f x

{-------------------------------------------------------------------------------
  Dealing with HKD records
-------------------------------------------------------------------------------}

class Coerce t where
  -- | Drop decoration
  --
  -- /NOTE/: The default instance is valid only for datatypes that are morally
  -- have a "higher order representative role"; that is, the type of every field
  -- of @t (DecoratedWith Identity)@ must be representationally equal to the
  -- corresponding type of @t Undecorated@. In the typical case of
  --
  -- > data SomeRecord f = MkSomeRecord {
  -- >     field1 :: HKD f a1
  -- >   , field2 :: HKD f a2
  -- >     ..
  -- >   , fieldN :: aN
  -- >   , ..
  -- >   , fieldM :: HKD f aM
  -- >   }
  --
  -- where every field either has type @HKD f a@ or @a@ (not mentioning @f@ at
  -- all), this will automatically be the case.
  undecorate :: t (DecoratedWith Identity) -> t Undecorated
  undecorate = unsafeCoerce

  -- | Introduce trivial decoration
  --
  -- See 'undecorate' for discussion of the validity of the default definitino.
  decorate :: t Undecorated -> t (DecoratedWith Identity)
  decorate = unsafeCoerce

class Coerce t => Traversable t where
  traverse ::
       Applicative m
    => (forall a. f a -> m (g a))
    ->    t (DecoratedWith f)
    -> m (t (DecoratedWith g))

sequence ::
     (Traversable t, Applicative m)
  => t (DecoratedWith m) -> m (t Undecorated)
sequence = fmap undecorate . traverse (fmap Identity)

map ::
     Traversable t
  => (forall a. f a -> g a)
  -> t (DecoratedWith f)
  -> t (DecoratedWith g)
map f = runIdentity . traverse (Identity . f)

{-------------------------------------------------------------------------------
  Dealing with HKD fields
-------------------------------------------------------------------------------}

data IsValidDecoration (c :: (Type -> Type) -> Constraint) (f :: Type -> Type) where
  ValidUndecorated :: IsValidDecoration c Undecorated
  ValidDecoratedWith :: c f => IsValidDecoration c (DecoratedWith f)

class ValidDecoration (c :: (Type -> Type) -> Constraint) (f :: Type -> Type) where
  validDecoration :: IsValidDecoration c f

instance ValidDecoration c Undecorated where
  validDecoration = ValidUndecorated

instance c f => ValidDecoration c (DecoratedWith f) where
  validDecoration = ValidDecoratedWith

-- | Specify field value of record with unknown decoration
--
-- You may need an additional type annotation also for @a@; for example
--
-- > HKD.pure (Proxy @f) Nothing
--
-- will result in an error message such as
--
-- > Couldn't match expected type: HKD f (Maybe SomeConcreteType)
-- >             with actual type: HKD f (Maybe a0)
--
-- This is because @HKD@ is a type family in two arguments (even though in an
-- ideal world it should be defined as a type family in /one/ argument).
pure :: forall f a. ValidDecoration Applicative f => Proxy f -> a -> HKD f a
pure _ =
    case validDecoration :: IsValidDecoration Applicative f of
      ValidDecoratedWith -> Prelude.pure
      ValidUndecorated   -> id

{-------------------------------------------------------------------------------
  Error decorations
-------------------------------------------------------------------------------}

type Checked e = DecoratedWith (Either e)

sequenceChecked ::
     (MonadError e m, Traversable t)
  => t (Checked e) -> m (t Undecorated)
sequenceChecked = either throwError return . sequence

