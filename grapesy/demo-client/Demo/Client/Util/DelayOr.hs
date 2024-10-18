-- | Interleave values with delay requests
--
-- Intended for qualified import.
--
-- > import Demo.Client.Util.DelayOr (DelayOr(..))
-- > import Demo.Client.Util.DelayOr qualified as DelayOr
module Demo.Client.Util.DelayOr (
    DelayOr(..)
  , mapM_
  , forM_
  , source
  ) where

import Prelude hiding (mapM_)
import Prelude qualified

import Control.Concurrent hiding (yield)
import Control.Monad.IO.Class
import Data.Conduit

data DelayOr a =
    Delay Double -- ^ Delay in seconds
  | Exec a       -- ^ Execute the specified RPC
  deriving (Show)

delayOr :: MonadIO m => (a -> m ()) -> DelayOr a -> m ()
delayOr _ (Delay d) = liftIO $ threadDelay (round (d * 1_000_000))
delayOr f (Exec a)  = f a

mapM_ :: MonadIO m => (a -> m ()) -> [DelayOr a] -> m ()
mapM_ = Prelude.mapM_ . delayOr

forM_ :: MonadIO m => [DelayOr a] -> (a -> m ()) -> m ()
forM_ = flip mapM_

source :: [DelayOr a] -> ConduitT () a IO ()
source = Prelude.mapM_ (delayOr yield)

