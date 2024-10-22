-- | Positioned elements
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Common.StreamElem qualified as StreamElem
--
-- "Network.GRPC.Common" (intended for unqualified import) exports
-- @StreamElem(..)@, but none of the operations on 'StreamElem'.
module Network.GRPC.Common.StreamElem (
    StreamElem(..)
    -- * Conversion
  , value
    -- * Iteration
    -- * Iteration
  , mapM_
  , forM_
  , whileNext_
  , collect
  , whenDefinitelyFinal
  ) where

import Prelude hiding (mapM_)

import Control.Monad.State (StateT, runStateT, lift, modify)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Tuple (swap)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | An element positioned in a stream
data StreamElem b a =
    -- | Element in the stream
    --
    -- The final element in a stream may or may not be marked as final; if it is
    -- not, we will only discover /after/ receiving the final element that it
    -- was in fact final. Moreover, we do not know ahead of time whether or not
    -- the final element will be marked.
    --
    -- When we receive an element and it is not marked final, this might
    -- therefore mean one of two things, without being able to tell which:
    --
    -- * We are dealing with a stream in which the final element is not marked.
    --
    --   In this case, the element may or may not be the final element; if it
    --   is, the next value will be 'NoMoreElems' (but waiting for the next
    --   value might mean a blocking call).
    --
    -- * We are dealing with a stream in which the final element /is/ marked.
    --
    --   In this case, this element is /not/ final (and the final element, when
    --   we receive it, will be tagged as 'Final').
    StreamElem !a

    -- | We received the final element
    --
    -- The final element is annotated with some additional information.
  | FinalElem !a !b

    -- | There are no more elements
    --
    -- This is used in two situations:
    --
    -- * The stream didn't contain any elements at all.
    -- * The final element was not marked as final.
    --   See 'StreamElem' for detailed additional discussion.
  | NoMoreElems !b
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

instance Bifunctor StreamElem where
  bimap g f (FinalElem   a b) = FinalElem   (f a) (g b)
  bimap g _ (NoMoreElems   b) = NoMoreElems       (g b)
  bimap _ f (StreamElem  a  ) = StreamElem  (f a)

instance Bifoldable StreamElem where
  bifoldMap g f (FinalElem   a b) = f a <> g b
  bifoldMap g _ (NoMoreElems   b) =        g b
  bifoldMap _ f (StreamElem  a  ) = f a

instance Bitraversable StreamElem where
  bitraverse g f (FinalElem   a b) = FinalElem   <$> f a <*> g b
  bitraverse g _ (NoMoreElems   b) = NoMoreElems <$>         g b
  bitraverse _ f (StreamElem  a  ) = StreamElem  <$> f a

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Value of the element, if one is present
--
-- Returns 'Nothing' in case of 'NoMoreElems'
--
-- Using this function loses the information whether the item was the final
-- item; this information can be recovered using 'whenDefinitelyFinal'.
value :: StreamElem b a -> Maybe a
value = \case
    StreamElem a   -> Just a
    FinalElem  a _ -> Just a
    NoMoreElems  _ -> Nothing

{-------------------------------------------------------------------------------
  Iteration
-------------------------------------------------------------------------------}

-- | Invoke the callback for each element
--
-- The final element is marked using 'FinalElem'; the callback is only invoked
-- on 'NoMoreElems' if the list is empty.
--
-- >    mapM_ f ([1,2,3], b)
-- > == do f (StreamElem 1)
-- >       f (StreamElem 2)
-- >       f (FinalElem 3 b)
-- >
-- >    mapM_ f ([], b)
-- > == do f (NoMoreElems b)
mapM_ :: forall m a b. Monad m => (StreamElem b a -> m ()) -> [a] -> b -> m ()
mapM_ f = go
  where
    go :: [a] -> b -> m ()
    go []     b = f (NoMoreElems b)
    go [a]    b = f (FinalElem a b)
    go (a:as) b = f (StreamElem a) >> go as b

-- | Like 'mapM_', but with the arguments in opposite order
forM_ :: Monad m => [a] -> b -> (StreamElem b a -> m ()) -> m ()
forM_ as b f = mapM_ f as b

-- | Invoke a function on each 'NextElem', until 'FinalElem' or 'NoMoreElems'
whileNext_ :: forall m a b. Monad m => m (StreamElem b a) -> (a -> m ()) -> m b
whileNext_ f g = go
  where
    go :: m b
    go = do
        ma <- f
        case ma of
          StreamElem  a   -> g a >> go
          FinalElem   a b -> g a >> return b
          NoMoreElems   b -> return b

-- | Invoke the callback until it returns 'NoNextElem', collecting results
collect :: forall m b a. Monad m => m (StreamElem b a) -> m ([a], b)
collect f =
    first reverse . swap <$> flip runStateT [] aux
  where
    aux :: StateT [a] m b
    aux = whileNext_ (lift f) $ modify . (:)

-- | Do we have evidence that this element is the final one?
--
-- The callback is not called on 'StreamElem'; this does /not/ mean that the
-- element was not final; see 'StreamElem' for detailed discussion.
whenDefinitelyFinal :: Applicative m => StreamElem b a -> (b -> m ()) -> m ()
whenDefinitelyFinal msg k =
    case msg of
      StreamElem  _   -> pure ()
      FinalElem   _ b -> k b
      NoMoreElems   b -> k b

