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
  , Traversable(..)
  , sequenceThrow
    -- * Dealing with HKD fields
  , ValidDecoration
  , pure
  ) where

import Prelude hiding (Traversable(..), pure)
import Prelude qualified

import Control.Monad.Except (MonadError, throwError)
import Data.Kind
import Data.Proxy

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

class Traversable t where
  sequence :: Applicative f => t (DecoratedWith f) -> f (t Undecorated)

sequenceThrow ::
     (MonadError e m, Traversable t)
  => t (DecoratedWith (Either e))
  -> m (t Undecorated)
sequenceThrow = either throwError return . sequence

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
