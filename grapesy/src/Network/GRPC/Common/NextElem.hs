-- | Elements in a stream (without metadata)
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Common.NextElem qualified as NextElem
--
-- "Network.GRPC.Common" (intended for unqualified import) exports
-- @NextElem(..)@, but none of the operations on 'NextElem'.
module Network.GRPC.Common.NextElem (
    NextElem(..)
    -- * API
  , mapM_
  , forM_
  , collect
  , whileNext_
  , toStreamElem
  ) where

import Prelude hiding (mapM_)

import Network.GRPC.Common.StreamElem (StreamElem(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Is there a next element in a stream?
--
-- Does not record metadata, unlike 'Network.GRPC.Common.StreamElem.StreamElem'.
data NextElem a = NoNextElem | NextElem !a
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

mapM_ :: forall m a. Monad m => (NextElem a -> m ()) -> [a] -> m ()
mapM_ f = go
  where
    go :: [a] -> m ()
    go []     = f NoNextElem
    go (x:xs) = f (NextElem x) >> go xs

forM_ :: Monad m => [a] -> (NextElem a -> m ()) -> m ()
forM_ = flip mapM_

collect :: forall m a. Monad m => m (NextElem a) -> m [a]
collect f = go []
  where
    go :: [a] -> m [a]
    go acc = do
         ma <- f
         case ma of
           NoNextElem -> return (reverse acc)
           NextElem a -> go (a:acc)

whileNext_ :: forall m a b. Monad m => m (NextElem a) -> (a -> m b) -> m ()
whileNext_ f g = go
  where
    go :: m ()
    go = do
        ma <- f
        case ma of
          NoNextElem -> return ()
          NextElem a -> g a >> go

toStreamElem :: b -> NextElem a -> StreamElem b a
toStreamElem b NoNextElem   = NoMoreElems b
toStreamElem _ (NextElem a) = StreamElem a