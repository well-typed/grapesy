-- | Very minimal support for HKD
--
-- Intended for qualified import.
--
-- import Network.GRPC.Util.HKD (HKD, Undecorated, DecoratedWith)
-- import Network.GRPC.Util.HKD qualified as HKD
module Network.GRPC.Util.HKD (
    HKD
  , DecoratedWith
  , Undecorated
  , Traversable(..)
  ) where

import Prelude hiding (Traversable)
import Data.Kind

data Undecorated                       (x :: Type)
data DecoratedWith (f :: Type -> Type) (x :: Type)

type family HKD (f :: Type -> Type) (x :: Type) :: Type

type instance HKD Undecorated       x = x
type instance HKD (DecoratedWith f) x = f x

class Traversable t where
  sequence :: Applicative f => t (DecoratedWith f) -> f (t Undecorated)
