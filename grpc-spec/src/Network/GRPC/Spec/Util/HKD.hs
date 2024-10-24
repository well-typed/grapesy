-- | Small module to support the higher-kinded data (HKD) pattern
--
-- This module is similar in spirit to libraries such as @barbies@ (and to
-- lesser degree @hkd@), but the technical details of the approach are somewhat
-- different.
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Util.HKD (HKD, Undecorated, DecoratedWith) import
-- > Network.GRPC.Spec.Util.HKD qualified as HKD
module Network.GRPC.Spec.Util.HKD (
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

-- | Marker for fields of HKD types
--
-- A common pattern for datatypes is to wrap every field in a type constructor:
--
-- > data RequestHeaders_ f = RequestHeaders {
-- >     requestTimeout     :: f (Maybe Timeout)
-- >   , requestCompression :: f (Maybe CompressionId)
-- >   , ..
-- >   }
--
-- The downside of such an approach is that if we don't need that type
-- constructor, we must instantiate @f@ to 'Identity', which results in
-- syntactic overhead.
-- (See also
-- [The Haskell Unfolder episode 14: Higher-kinded types]
-- (https://www.youtube.com/watch?v=EXgsXy1BR-0&list=PLD8gywOEY4HaG5VSrKVnHxCptlJv2GAn7&index=15).)
-- The @HKD@ family is designed to avoid this overhead:
--
-- > data RequestHeaders_ f = RequestHeaders {
-- >     requestTimeout     :: HKD f (Maybe Timeout)
-- >   , requestCompression :: HKD f (Maybe CompressionId)
-- >   , ..
-- >   }
--
-- There are then two valid choices for @f@:
--
-- * 'Undecorated': @HKD Undecorated x@ is simply equal to @x@.
--   This avoids the overhead mentioned above.
-- * 'DecoratedWith' @f@, for some @f@:
--   @HKD (DecoratedWith f) x@ is equal to @f x@.
--
-- This explicit distinction between 'Undecorated' and 'DecoratedWith' is the
-- main difference between the approach in this module and other libraries that
-- provide similar functionality.
type family HKD (f :: Type -> Type) (x :: Type) :: Type

type instance HKD Undecorated       x = x
type instance HKD (DecoratedWith f) x = f x

{-------------------------------------------------------------------------------
  Dealing with HKD records
-------------------------------------------------------------------------------}

-- | Witness the isomorphism between @Undecorated@ and @DecoratedWith Identity@.
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

-- | Higher-kinded equivalent of 'Prelude.Traversable'
class Coerce t => Traversable t where
  traverse ::
       Applicative m
    => (forall a. f a -> m (g a))
    ->    t (DecoratedWith f)
    -> m (t (DecoratedWith g))

-- | Higher-kinded equivalent of 'Prelude.sequence'
sequence ::
     (Traversable t, Applicative m)
  => t (DecoratedWith m) -> m (t Undecorated)
sequence = fmap undecorate . traverse (fmap Identity)

-- | Higher-kinded equivalent of 'Prelude.map'
map ::
     Traversable t
  => (forall a. f a -> g a)
  -> t (DecoratedWith f)
  -> t (DecoratedWith g)
map f = runIdentity . traverse (Identity . f)

{-------------------------------------------------------------------------------
  Dealing with HKD fields
-------------------------------------------------------------------------------}

data IsValidDecoration (c :: (Type -> Type) -> Constraint) f where
  ValidUndecorated :: IsValidDecoration c Undecorated
  ValidDecoratedWith :: c f => IsValidDecoration c (DecoratedWith f)

-- | Valid decorations
--
-- These are only two valid decorations (and new instances of this class cannot
-- be defined):
--
-- * @ValidDecoration c Undecorated@, for any @c@
-- * @ValidDecoration c (DecoratedWith f)@, for any @f@ satisfying @c@
class ValidDecoration (c :: (Type -> Type) -> Constraint) f where
  validDecoration :: IsValidDecoration c f
  validDecoration = undefined

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

-- | Decorate with potential errors
type Checked e = DecoratedWith (Either e)

-- | Throw all errors found in the datatype
--
-- Given a datatype decorated with potential errors, find and throw any errors;
-- if no errors are found, return the undecorated value.
sequenceChecked ::
     (MonadError e m, Traversable t)
  => t (Checked e) -> m (t Undecorated)
sequenceChecked = either throwError return . sequence

