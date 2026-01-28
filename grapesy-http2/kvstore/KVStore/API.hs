-- | Serialization format agnostic layer
--
-- This allows us to the run the same client with either Protobuf or Java, and
-- similarly for the server.
module KVStore.API (
    Key(..)
  , Value(..)
  , KVStore(..)
  ) where

import Control.DeepSeq (NFData)
import Control.Monad
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.Hashable
import Data.Text.Encoding qualified as Text

import Network.GRPC.Common.JSON

{-------------------------------------------------------------------------------
  Key and value
-------------------------------------------------------------------------------}

newtype Key = Key {
       getKey :: ByteString
     }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable, NFData)
  deriving (ToJSON, FromJSON) via Base64

newtype Value = Value {
      getValue :: ByteString
    }
  deriving stock (Show, Eq, Ord)
  deriving newtype (NFData)
  deriving (ToJSON, FromJSON) via Base64

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

data KVStore = KVStore {
      create   :: (Key, Value) -> IO ()
    , retrieve :: Key -> IO Value
    , update   :: (Key, Value) -> IO ()
    , delete   :: Key -> IO ()
    }

{-------------------------------------------------------------------------------
  Auxiliary: base64 encoding
-------------------------------------------------------------------------------}

newtype Base64 = Base64 { getBase64 :: ByteString }

instance ToJSON Base64 where
  toJSON = toJSON . Text.decodeUtf8 . Base64.encode . getBase64

instance FromJSON Base64 where
  parseJSON = fmap Base64 . decode . Text.encodeUtf8 <=< parseJSON
    where
      decode :: ByteString -> Aeson.Parser ByteString
      decode = either fail return . Base64.decode
