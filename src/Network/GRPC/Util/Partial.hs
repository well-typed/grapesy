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
    -- * Utilities
    -- ** General purpose
  , expectAtLeastOne
    -- ** Parsing HTTP headers
  , expectHeaderValue
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.List (intercalate)
import Data.SOP
import Data.SOP.Constraint
import Generics.SOP
import Network.HTTP.Types qualified as HTTP
import Data.Typeable
import Data.List.NonEmpty (NonEmpty(..))

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
    , MonadError e -- inherited from @m@
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

{-------------------------------------------------------------------------------
  General utilities
-------------------------------------------------------------------------------}

expectAtLeastOne :: forall m a.
     (MonadError String m, Typeable a)
  => [a] -> m (NonEmpty a)
expectAtLeastOne (x : xs) = return (x :| xs)
expectAtLeastOne []       = throwError $ concat [
                                "Expected at least one "
                              , show (typeRep (Proxy @a))
                              ]

{-------------------------------------------------------------------------------
  Specific support for parsing headers
-------------------------------------------------------------------------------}

expectHeaderValue ::
     MonadError String m
  => HTTP.Header -> [Strict.ByteString] -> m ()
expectHeaderValue (name, actual) expected =
    unless (actual `elem` expected) $ throwError $ concat [
        "Unexpected value \""
      , BS.Strict.C8.unpack actual
      , "\" for header"
      , show name -- Show instance adds quotes
      , ". Expected "
      , intercalate " or " $
          map (\e -> "\"" ++ BS.Strict.C8.unpack e ++ "\"") expected
      , "."
      ]

