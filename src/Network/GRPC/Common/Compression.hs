-- | Public 'Compression' API
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Common.Compression (Compression(..))
-- > import Network.GRPC.Common.Compression qualified as Compression
module Network.GRPC.Common.Compression (
    -- * Definition
    Compression(..)
  , CompressionId(..)
    -- * Standard compresion schemes
  , allSupported
  , identity
  , gzip
    -- * Negotation
  , Negotation(..)
  , UnsupportedCompression(..)
  , chooseFirst
  , require
  ) where

import Control.Exception
import Data.Default
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

import Network.GRPC.Spec.Compression

{-------------------------------------------------------------------------------
  Negotation
-------------------------------------------------------------------------------}

-- | Compression negotation
data Negotation = Negotation {
      -- | Which algorithms should be offered (in this order) to the server?
      offer :: NonEmpty CompressionId

      -- | Choose compression algorithm
      --
      -- We will run this only once per open connection, when the server first
      -- tells us their list of supported compression algorithms. Unless
      -- compression negotation has taken place, no compression should be used.
    , choose ::
           NonEmpty CompressionId
        -> Either UnsupportedCompression Compression
    }

-- | Exception thrown when we were unable to pick a preferred compression
data UnsupportedCompression = UnsupportedCompression {
      serverSupportedCompression :: NonEmpty CompressionId
    }
  deriving stock (Show)
  deriving anyclass (Exception)

instance Default Negotation where
  def = chooseFirst allSupported

-- | Choose the first algorithm that appears in the list of client supported
chooseFirst :: NonEmpty Compression -> Negotation
chooseFirst ourSupported = Negotation {
      offer  = fmap compressionId ourSupported
    , choose = \serverSupported ->
        let isSupported :: Compression -> Bool
            isSupported = (`elem` serverSupported) . compressionId
        in case NE.filter isSupported ourSupported of
             c:_ -> Right c
             []  -> Left $ UnsupportedCompression serverSupported
    }

-- | Insist on the specified algorithm
require :: Compression -> Negotation
require = chooseFirst . NE.singleton
