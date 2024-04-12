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
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype Store = Wrap {
      unwrap :: MVar (HashMap ByteString ByteString)
    }

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

new :: IO Store
new = Wrap <$> newMVar HashMap.empty

-- | Get value associated with the key, if it exists
get :: Store -> ByteString -> IO (Maybe ByteString)
get store key = withStore store $ HashMap.lookup key

-- | Remove an key from the map
remove :: Store -> ByteString -> IO ()
remove store key = modifyStore_ store $ HashMap.delete key

-- | Put a new key/value pair into the var
--
-- Returns 'True' on success, or 'False' if they key was already present.
putIfAbsent :: Store -> ByteString -> ByteString -> IO Bool
putIfAbsent store key value =
    modifyStore store $ \hm ->
      case HashMap.lookup key hm of
        Nothing -> (HashMap.insert key value hm, True)
        Just _  -> (hm, False)

-- | Replace the value associated with the key, if the key already exists
--
-- Returns 'True' if the value was successfully replaced, or 'False' if the key
-- was not present.
replace :: Store -> ByteString -> ByteString -> IO Bool
replace store key newValue =
    modifyStore store $ \hm ->
      case HashMap.lookup key hm of
        Nothing -> (hm, False)
        Just _  -> (HashMap.insert key newValue hm, True)

{-------------------------------------------------------------------------------
  Internal: wrap pure operations
-------------------------------------------------------------------------------}

withStore ::
     Store
  -> (HashMap ByteString ByteString -> a)
  -> IO a
withStore store f = withMVar (unwrap store) $ return . f

modifyStore ::
     Store
  -> (     HashMap ByteString ByteString
       -> (HashMap ByteString ByteString, a)
     )
  -> IO a
modifyStore store f = modifyMVar (unwrap store) $ return . f

modifyStore_ ::
     Store
  -> (    HashMap ByteString ByteString
       -> HashMap ByteString ByteString
     )
  -> IO ()
modifyStore_ store f = modifyMVar_ (unwrap store) $ return . f

