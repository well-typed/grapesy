{-# LANGUAGE CPP #-}

-- | Public 'Compression' API
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Common.Compression (Compression(..))
-- > import Network.GRPC.Common.Compression qualified as Compr
module Network.GRPC.Common.Compression (
    -- * Definition
    Compression(..)
  , CompressionId(..)
    -- * Standard compression schemes
  , gzip
  , allSupportedCompression
    -- * Negotation
  , Negotation(..)
  , getSupported
    -- ** Specific negotation strategies
  , none
  , chooseFirst
  , only
  , insist
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
      --
      -- This should normally always include 'Identity' (see 'choose');
      -- but see 'insist'.
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
getSupported compr cid = Map.lookup cid (supported compr)

instance Default Negotation where
  def = chooseFirst allSupportedCompression

-- | Disable all compression
none :: Negotation
none = insist noCompression

-- | Choose the first algorithm that appears in the list of peer supported
--
-- Precondition: the list should include the identity.
chooseFirst :: NonEmpty Compression -> Negotation
chooseFirst ourSupported = Negotation {
      offer =
        fmap compressionId ourSupported
    , choose = \peerSupported ->
        let peerSupports :: Compression -> Bool
            peerSupports = (`elem` peerSupported) . compressionId
        in case NE.filter peerSupports ourSupported of
             c:_ -> c
             []  -> noCompression
    , supported =
        Map.fromList $
          map (\c -> (compressionId c, c)) (toList ourSupported)
    }

-- | Only use the given algorithm, if the peer supports it
only :: Compression -> Negotation
only compr = chooseFirst (compr :| [noCompression])

-- | Insist on the specified algorithm, /no matter what the peer offers/
--
-- This is dangerous: if the peer does not support the specified algorithm, it
-- will be unable to decompress any messages. Primarily used for testing.
--
-- See also 'only'.
insist :: Compression -> Negotation
insist compr = Negotation {
      offer     = compressionId compr :| []
    , choose    = \_ -> compr
    , supported = Map.singleton (compressionId compr) compr
    }
