-- | User-facing 'Compression' API
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Compression (Compression(..))
-- > import Network.GRPC.Compression qualified as Compression
module Network.GRPC.Compression (
    -- * Definition
    Compression(..)
  , CompressionId(..)
    -- * Standard compresion schemes
  , allSupported
  , identity
  , gzip
    -- * Choosing compression algorithm
  , Preference(..)
  , UnsupportedCompression(..)
  , defaultPreference
  , forceAlgorithm
  ) where

import Control.Exception
import Data.List.NonEmpty
import Data.List.NonEmpty qualified as NE

import Network.GRPC.Spec.Compression

{-------------------------------------------------------------------------------
  Choosing compression algorithm
-------------------------------------------------------------------------------}

data Preference = Preference {
      preferOutbound :: Compression
    , preferInbound  :: NonEmpty Compression
    }
  deriving stock (Show)

-- | Exception thrown when we were unable to pick a preferred compression
data UnsupportedCompression = UnsupportedCompression {
      serverSupportedCompression :: [CompressionId]
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Default way to choose a compression algorithm
defaultPreference :: [CompressionId] -> Either UnsupportedCompression Preference
defaultPreference serverSupported =
    case NE.filter isSupported allSupported of
      []   -> Left $ UnsupportedCompression serverSupported
      c:cs -> Right $ Preference {
                  preferOutbound = c
                , preferInbound  = c :| cs
                }
  where
    isSupported :: Compression -> Bool
    isSupported = (`elem` serverSupported) . compressionId

-- | Only use the specified algorithm
forceAlgorithm ::
     Compression
  -> [CompressionId] -> Either UnsupportedCompression Preference
forceAlgorithm alg serverSupported =
    if compressionId alg `elem` serverSupported
      then Right $ Preference {
               preferOutbound = alg
             , preferInbound  = alg :| []
             }
      else Left $ UnsupportedCompression serverSupported
