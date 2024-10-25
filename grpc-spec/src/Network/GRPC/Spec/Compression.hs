{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compression
--
-- Intended for unqualified import.
module Network.GRPC.Spec.Compression (
    -- * Definition
    Compression(..)
  , allSupportedCompression
  , compressionIsIdentity
    -- ** ID
  , CompressionId(..)
  , serializeCompressionId
  , deserializeCompressionId
    -- * Specific coders
  , noCompression
  , gzip
  , deflate
#ifdef SNAPPY
  , snappy
#endif
  ) where

import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib qualified as Deflate
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String
import GHC.Generics (Generic)

#ifdef SNAPPY
import Codec.Compression.SnappyC.Framed qualified as Snappy
#endif

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Compression scheme
data Compression = Compression {
      -- | Compression identifier
      compressionId :: CompressionId

      -- | Compress
    , compress :: Lazy.ByteString -> Lazy.ByteString

      -- | Decompress
      --
      -- TODO: <https://github.com/well-typed/grapesy/issues/57>.
      -- We need to deal with decompression failures.
    , decompress :: Lazy.ByteString -> Lazy.ByteString

      -- | Uncompressed size threshold
      --
      -- Compression is only useful for uncompressed data sizes satisfying this
      -- predicate.
    , uncompressedSizeThreshold :: Int64 -> Bool
    }

instance Show Compression where
  show Compression{compressionId} = "<Compression " ++ show compressionId ++ ">"

-- | All supported compression algorithms supported
--
-- The order of this list is important: algorithms listed earlier are preferred
-- over algorithms listed later.
allSupportedCompression :: NonEmpty Compression
allSupportedCompression =
    gzip :|
      [ deflate
#ifdef SNAPPY
      , snappy
#endif
      , noCompression
      ]

{-------------------------------------------------------------------------------
  Compression ID
-------------------------------------------------------------------------------}

-- | Compression ID
--
-- The gRPC specification defines
--
-- > Content-Coding → "identity" / "gzip" / "deflate" / "snappy" / {custom}
data CompressionId =
    Identity
  | GZip
  | Deflate
  | Snappy
  | Custom String
  deriving stock (Eq, Ord, Generic)

-- | Serialize compression ID
serializeCompressionId :: CompressionId -> Strict.ByteString
serializeCompressionId Identity   = "identity"
serializeCompressionId GZip       = "gzip"
serializeCompressionId Deflate    = "deflate"
serializeCompressionId Snappy     = "snappy"
serializeCompressionId (Custom i) = BS.Strict.UTF8.fromString i

-- | Parse compression ID
deserializeCompressionId :: Strict.ByteString -> CompressionId
deserializeCompressionId "identity" = Identity
deserializeCompressionId "gzip"     = GZip
deserializeCompressionId "deflate"  = Deflate
deserializeCompressionId "snappy"   = Snappy
deserializeCompressionId i          = Custom (BS.Strict.UTF8.toString i)

instance Show CompressionId where
  show = BS.Strict.UTF8.toString . serializeCompressionId

instance IsString CompressionId where
  fromString = deserializeCompressionId . BS.Strict.UTF8.fromString

compressionIsIdentity :: Compression -> Bool
compressionIsIdentity = (== Identity) . compressionId

{-------------------------------------------------------------------------------
  Compression algorithms
-------------------------------------------------------------------------------}

-- | Disable compression (referred to as @identity@ in the gRPC spec)
noCompression :: Compression
noCompression = Compression {
      compressionId             = Identity
    , compress                  = id
    , decompress                = id
    , uncompressedSizeThreshold = const False
    }

-- | @gzip@
gzip :: Compression
gzip = Compression {
      compressionId = GZip
    , compress      = GZip.compress
    , decompress    = GZip.decompress

      -- gzip only achieves a compression ratio of 8:7 for messages of at least
      -- 27 bytes.
    , uncompressedSizeThreshold = (>= 27)
    }

-- | @zlib@ (aka @deflate@) compression
--
-- Note: The gRPC spec calls this "deflate", but it is /not/ raw deflate
-- format. The expected format (at least by the python server) is just zlib
-- (which is an envelope holding the deflate data).
deflate :: Compression
deflate = Compression {
      compressionId = Deflate
    , compress      = Deflate.compress
    , decompress    = Deflate.decompress

      -- zlib deflate only achieves a compression ratio of 8:7 for messages of
      -- at least 13 bytes.
    , uncompressedSizeThreshold = (>= 13)
    }

#ifdef SNAPPY
-- | Snappy compression
snappy :: Compression
snappy = Compression {
      compressionId = Snappy
    , compress      = Snappy.compress
    , decompress    = Snappy.decompress

      -- snappy only achieves a compression ratio of 8:7 for messages of at
      -- least 28 bytes.
    , uncompressedSizeThreshold = (>= 28)
    }
#endif