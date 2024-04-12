-- | A set which supports random access.
--
-- Intended for qualified import.
--
-- > import KVStore.Util.RandomAccessSet (RandomAccessSet)
-- > import KVStore.Util.RandomAccessSet qualified as RandomAccessSet
module KVStore.Util.RandomAccessSet (
    RandomAccessSet -- opaque
    -- * API
  , new
  , isEmpty
  , getRandomKey
  , add
  , remove
  ) where

import Control.Concurrent
import Data.Set (Set)
import Data.Set qualified as Set

import KVStore.Util.RandomGen qualified as Random

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype RandomAccessSet a = Wrap {
      unwrap :: MVar (Set a)
    }

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

new :: IO (RandomAccessSet a)
new = Wrap <$> newMVar Set.empty

isEmpty :: RandomAccessSet a -> IO Bool
isEmpty ras = withRAS ras $ Set.null

getRandomKey :: RandomAccessSet a -> IO a
getRandomKey ras = do
    gen  <- Random.new
    size <- withRAS ras $ Set.size
    n    <- Random.nextInt gen size
    withRAS ras $ Set.elemAt n

add :: Ord a => RandomAccessSet a -> a -> IO Bool
add ras value =
    modifyRAS ras $ \set ->
      if value `Set.member` set
        then (set, False)
        else (Set.insert value set, True)

remove :: Ord a => RandomAccessSet a -> a -> IO ()
remove ras value = modifyRAS_ ras $ Set.delete value

{-------------------------------------------------------------------------------
  Internal: wrap pure operations
-------------------------------------------------------------------------------}

withRAS :: RandomAccessSet a -> (Set a -> b) -> IO b
withRAS ras f = withMVar (unwrap ras) $ return . f

modifyRAS :: RandomAccessSet a -> (Set a -> (Set a, b)) -> IO b
modifyRAS ras f = modifyMVar (unwrap ras) $ return . f

modifyRAS_ :: RandomAccessSet a -> (Set a -> Set a) -> IO ()
modifyRAS_ ras f = modifyMVar_ (unwrap ras) $ return . f
