-- | Positioned elements
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Common.StreamElem qualified as StreamElem
--
-- "Network.GRPC.Common" (intended for unqualified import) exports 'StreamElem',
-- but none of the operations on 'StreamElem'.
module Network.GRPC.Common.StreamElem (
    StreamElem(..)
  , whenDefinitelyFinal
  ) where

import Prelude hiding (mapM_)

import Data.Bifunctor
import GHC.Generics qualified as GHC
import Text.Show.Pretty

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
    --   is, the next value will be 'NoMore' (but waiting for the next value
    --   might mean a blocking call).
    --
    -- * We are dealing with a stream in which the final element /is/ marked.
    --
    --   In this case, this element is /not/ final (and the final element, when
    --   we receive it, will be tagged as 'Final').
    StreamElem a

    -- | We received the final element
    --
    -- The final element is annotated with some additional information.
  | FinalElem a b

    -- | There are no more elements
    --
    -- This is used in two situations:
    --
    -- * The stream didn't contain any elements at all.
    -- * The final element was not marked as final.
    --   See 'StreamElem' for detailed additional discussion.
  | NoMoreElems b
  deriving stock (Show, Eq, Functor, Foldable, Traversable, GHC.Generic)
  deriving anyclass (PrettyVal)

instance Bifunctor StreamElem where
  bimap g f (FinalElem   a b) = FinalElem   (f a) (g b)
  bimap g _ (NoMoreElems   b) = NoMoreElems       (g b)
  bimap _ f (StreamElem  a  ) = StreamElem  (f a)

-- | Do we have evidence that this element is the final one?
--
-- A 'False' result does not mean the element is not final; see 'StreamElem' for
-- detailed discussion.
whenDefinitelyFinal :: Applicative m => StreamElem b a -> (b -> m ()) -> m ()
whenDefinitelyFinal msg k =
    case msg of
      StreamElem  _   -> pure ()
      FinalElem   _ b -> k b
      NoMoreElems   b -> k b

