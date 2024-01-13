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
  ) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8
import Data.List.NonEmpty (NonEmpty(..))
import Data.String
import GHC.Generics qualified as GHC
import Text.Show.Pretty

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
    , decompress :: Lazy.ByteString -> Lazy.ByteString
    }

instance Show Compression where
  show Compression{compressionId} = "<Compression " ++ show compressionId ++ ">"

-- | All compression algorithms supported by @grapesy@
--
-- The order of this list is important: algorithms listed earlier are preferred
-- over algorithms listed later.
allSupportedCompression :: NonEmpty Compression
allSupportedCompression = gzip :| [noCompression]

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
  deriving stock (Eq, Ord, GHC.Generic)
  deriving anyclass (PrettyVal)

serializeCompressionId :: CompressionId -> Strict.ByteString
serializeCompressionId Identity   = "identity"
serializeCompressionId GZip       = "gzip"
serializeCompressionId Deflate    = "deflate"
serializeCompressionId Snappy     = "snappy"
serializeCompressionId (Custom i) = BS.Strict.UTF8.fromString i

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

noCompression :: Compression
noCompression = Compression {
      compressionId = Identity
    , compress      = id
    , decompress    = id
    }

-- TODO: We should deal with errors during decompression
gzip :: Compression
gzip = Compression {
      compressionId = GZip
    , compress      = GZip.compress
    , decompress    = GZip.decompress
    }