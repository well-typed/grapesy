-- | Partial values
--
-- Intended for unqualified import.
module Network.GRPC.Util.Partial (
    -- * Partial values
    Partial
  , fromPartial
    -- * Updates
  , PartialUpdate -- opaque
  , partialUpdates
    -- * Parser
  , PartialParser -- opaque
  , runPartialParser
  , update
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.SOP
import Data.SOP.Constraint
import Generics.SOP

{-------------------------------------------------------------------------------
  Partial values
-------------------------------------------------------------------------------}

type Partial m r = NP m (Head (Code r))

fromPartial ::
     (Applicative m, Generic r, Code r ~ '[Head (Code r)])
  => Partial m r -> m r
fromPartial = fmap to . hsequence . SOP . Z

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

newtype PartialUpdate m r a = PartialUpdate {
      applyPartialUpdate :: (m a -> m a) -> Partial m r -> Partial m r
    }

newtype UpdateNP m xs a = UpdateNP {
      updateNP :: (m a -> m a) -> NP m xs -> NP m xs
    }

shiftUpdate :: UpdateNP m xs a -> UpdateNP m (x : xs) a
shiftUpdate (UpdateNP upd) = UpdateNP $ \f (x :* xs) -> x :* upd f xs

fromUpdateNP :: UpdateNP m (Head (Code r)) a -> PartialUpdate m r a
fromUpdateNP = PartialUpdate . updateNP

partialUpdates ::
     (SListI xs, Code r ~ '[xs])
  => Proxy r -> NP (PartialUpdate m r) xs
partialUpdates _ = hmap fromUpdateNP $ go shape
  where
    go :: Shape xs -> NP (UpdateNP m xs) xs
    go ShapeNil      = Nil
    go (ShapeCons s) = UpdateNP (\f (x :* xs) -> f x :* xs)
                    :* hmap shiftUpdate (go s)

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

-- | Parser for partial values
newtype PartialParser r m a = PartialParser {
      unwrapPartialParser :: StateT (Partial m r) m a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadError e
    )

instance MonadTrans (PartialParser r) where
  lift = PartialParser . lift

runPartialParser ::
     (Monad m, Generic r, Code r ~ '[Head (Code r)])
  => Partial m r -> PartialParser r m () -> m r
runPartialParser partial =
      (>>= fromPartial)
    . flip execStateT partial
    . unwrapPartialParser

update :: Monad m => PartialUpdate m r a -> (m a -> m a) -> PartialParser r m ()
update upd f = PartialParser $ modify $ applyPartialUpdate upd f