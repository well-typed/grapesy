-- | Binary RPC
--
-- Intended for unqualified import.
module Network.GRPC.Common.Binary (
    BinaryRpc
    -- * Encoding and decoding
  , encode
  , decodeOrThrow
  , DecodeException(..)
  ) where

import Control.Monad.Catch
import Data.Binary
import Data.Binary.Get qualified as Binary
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)

import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Decoding
-------------------------------------------------------------------------------}

decodeOrThrow :: (MonadThrow m, Binary a) => Lazy.ByteString -> m a
decodeOrThrow bs =
    case decodeOrFail bs of
      Left (unconsumed, consumed, errorMessage) ->
        throwM $ DecodeException{unconsumed, consumed, errorMessage}
      Right (unconsumed, consumed, a) ->
        if BS.Lazy.null unconsumed then
          return a
        else do
          let errorMessage = "Not all bytes consumed"
          throwM $ DecodeException{unconsumed, consumed, errorMessage}

data DecodeException =
    DecodeException {
        unconsumed   :: Lazy.ByteString
      , consumed     :: Binary.ByteOffset
      , errorMessage :: String
      }
  deriving stock (Show)
  deriving anyclass (Exception)




