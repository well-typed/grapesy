-- | Public 'Compression' API
--
-- Intended for unqualified import.
--
-- > import Network.GRPC.Common.Compression (Compression(..))
-- > import Network.GRPC.Common.Compression qualified as Compr
module Network.GRPC.Common.Compression (
    -- * Definition
    Compression(..)
  , CompressionId(..)
    -- * Standard compression schemes
  , noCompression
  , gzip
  , deflate
  , allSupportedCompression
    -- * Negotation
  , Negotation(..)
  , getSupported
    -- ** Specific negotation strategies
  , none
  , require
  , chooseFirst
  ) where

import Data.Default
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map

import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Negotation
-------------------------------------------------------------------------------}

-- | Compression negotation
data Negotation = Negotation {
      -- | Which algorithms should be offered (in this order) to the peer?
      offer :: NonEmpty CompressionId

      -- | Choose compression algorithm
      --
      -- We will run this only once per open connection, when the server first
      -- tells us their list of supported compression algorithms. Unless
      -- compression negotation has taken place, no compression should be used.
      --
      -- This cannot fail: the least common denominator is to use no
      -- compression. This must be allowed, because the gRPC specification
      -- /anyway/ allows a per-message flag to indicate whether the message
      -- is compressed or not; thus, even if a specific compression algorithm
      -- is negotiated, there is no guarantee anything is compressed.
    , choose :: NonEmpty CompressionId -> Compression

      -- | All supported compression algorithms
    , supported :: Map CompressionId Compression
    }

-- | Map 'CompressionId' to 'Compression' for supported algorithms
getSupported :: Negotation -> CompressionId -> Maybe Compression
getSupported _     Identity = Just $ noCompression
getSupported compr cid      = Map.lookup cid (supported compr)

instance Default Negotation where
  def = chooseFirst allSupportedCompression

-- | Disable all compression
none :: Negotation
none = require noCompression

-- | Insist on the specified algorithm
require :: Compression -> Negotation
require = chooseFirst . (:| [])

-- | Choose the first algorithm that appears in the list of client supported
chooseFirst :: NonEmpty Compression -> Negotation
chooseFirst ourSupported = Negotation {
      offer =
        fmap compressionId ourSupported
    , choose = \serverSupported ->
        let isSupported :: Compression -> Bool
            isSupported = (`elem` serverSupported) . compressionId
        in case NE.filter isSupported ourSupported of
             c:_ -> c
             []  -> noCompression
    , supported =
        Map.fromList $
          map (\c -> (compressionId c, c)) (toList ourSupported)
    }
