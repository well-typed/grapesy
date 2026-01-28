-- | Elements in a stream (without metadata)
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Common.NextElem qualified as NextElem
--
-- "Network.GRPC.Common" (intended for unqualified import) exports
-- @NextElem(..)@, but none of the operations on 'NextElem'.
module Network.GRPC.Common.NextElem (
    -- * Conversion
    toStreamElem
    -- * Iteration
  , mapM_
  , forM_
  , whileNext_
  , collect
  ) where

import Prelude hiding (mapM_)

import Control.Monad.State (StateT, execStateT, lift, modify)

import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Spec (NextElem(..))

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

toStreamElem :: b -> NextElem a -> StreamElem b a
toStreamElem b NoNextElem   = NoMoreElems b
toStreamElem _ (NextElem a) = StreamElem a

{-------------------------------------------------------------------------------
  Iteration
-------------------------------------------------------------------------------}

-- | Invoke the callback for each element, and then once more with 'NoNextElem'
--
-- >    mapM_ f [1,2,3]
-- > == do f (NextElem 1)
-- >       f (NextElem 2)
-- >       f (NextElem 3)
-- >       f NoNextElem
mapM_ :: forall m a. Monad m => (NextElem a -> m ()) -> [a] -> m ()
mapM_ f = go
  where
    go :: [a] -> m ()
    go []     = f NoNextElem
    go (x:xs) = f (NextElem x) >> go xs

-- | Like 'mapM_', but with the arguments in opposite order
forM_ :: Monad m => [a] -> (NextElem a -> m ()) -> m ()
forM_ = flip mapM_

-- | Invoke a function on each 'NextElem', until 'NoNextElem'
--
-- See also 'collect'.
whileNext_ :: forall m a. Monad m => m (NextElem a) -> (a -> m ()) -> m ()
whileNext_ f g = go
  where
    go :: m ()
    go = do
        ma <- f
        case ma of
          NoNextElem -> return ()
          NextElem a -> g a >> go

-- | Invoke the callback until it returns 'NoNextElem', collecting results
collect :: forall m a. Monad m => m (NextElem a) -> m [a]
collect f =
    reverse <$> flip execStateT [] aux
  where
    aux :: StateT [a] m ()
    aux = whileNext_ (lift f) $ modify . (:)

