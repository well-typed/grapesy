-- | Simple in-memory key-value var
--
-- Intended for qualified import.
--
-- > import KVStore.Util.Store (Store)
-- > import KVStore.Util.Store qualified as Store
module KVStore.Util.Store (
    Store -- opaque
    -- * API
  , new
  , get
  , putIfAbsent
  , replace
  , remove
  ) where

import Control.Concurrent
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype Store a b = Wrap {
      unwrap :: MVar (HashMap a b)
    }

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

new :: IO (Store a b)
new = Wrap <$> newMVar HashMap.empty

-- | Get value associated with the key, if it exists
get :: Hashable a => Store a b -> a -> IO (Maybe b)
get store key = withStore store $ HashMap.lookup key

-- | Remove an key from the map
remove :: Hashable a => Store a b -> a -> IO ()
remove store key = modifyStore_ store $ HashMap.delete key

-- | Put a new key/value pair into the var
--
-- Returns 'True' on success, or 'False' if they key was already present.
putIfAbsent :: Hashable a => Store a b -> a -> b -> IO Bool
putIfAbsent store key value =
    modifyStore store $ \hm ->
      case HashMap.lookup key hm of
        Nothing -> (HashMap.insert key value hm, True)
        Just _  -> (hm, False)

-- | Replace the value associated with the key, if the key already exists
--
-- Returns 'True' if the value was successfully replaced, or 'False' if the key
-- was not present.
replace :: Hashable a => Store a b -> a -> b -> IO Bool
replace store key newValue =
    modifyStore store $ \hm ->
      case HashMap.lookup key hm of
        Nothing -> (hm, False)
        Just _  -> (HashMap.insert key newValue hm, True)

{-------------------------------------------------------------------------------
  Internal: wrap pure operations
-------------------------------------------------------------------------------}

withStore :: Store a b -> (HashMap a b -> r) -> IO r
withStore store f = withMVar (unwrap store) $ return . f

modifyStore :: Store a b -> (HashMap a b -> (HashMap a b, r)) -> IO r
modifyStore store f = modifyMVar (unwrap store) $ return . f

modifyStore_ :: Store a b -> (HashMap a b -> HashMap a b) -> IO ()
modifyStore_ store f = modifyMVar_ (unwrap store) $ return . f

