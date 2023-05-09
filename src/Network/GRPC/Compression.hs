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
  ) where

import Network.GRPC.Spec.Compression
