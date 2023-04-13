{-# LANGUAGE OverloadedStrings #-}

-- | Compression
--
-- Intended for qualified import.
module Network.GRPC.Spec.Compression (
    -- * Definition
    Compression(..)
  , allSupported
  , isIdentity
    -- ** ID
  , CompressionId(..)
  , serializeId
  , deserializeId
    -- * Specific coders
  , identity
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.UTF8 qualified as BS.UTF8
import Data.List.NonEmpty (NonEmpty(..))
import Data.String

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Compression scheme
--
-- The gRPC spec refers to this as \"encoding\", but \"encode\" and \"decode\"
-- are overloaded terms, whereas 'compress' and 'decompress' are unambiguous.
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

allSupported :: NonEmpty Compression
allSupported = identity :| []

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
  | Custom Strict.ByteString
  deriving (Eq, Ord)

serializeId :: CompressionId -> Strict.ByteString
serializeId Identity   = "identity"
serializeId GZip       = "gzip"
serializeId Deflate    = "deflate"
serializeId Snappy     = "snappy"
serializeId (Custom i) = i

deserializeId :: Strict.ByteString -> CompressionId
deserializeId "identity" = Identity
deserializeId "gzip"     = GZip
deserializeId "deflate"  = Deflate
deserializeId "snappy"   = Snappy
deserializeId i          = Custom i

instance Show CompressionId where
  show = BS.UTF8.toString . serializeId

instance IsString CompressionId where
  fromString = deserializeId . BS.UTF8.fromString

isIdentity :: Compression -> Bool
isIdentity = (== Identity) . compressionId

{-------------------------------------------------------------------------------
  Identity
-------------------------------------------------------------------------------}

identity :: Compression
identity = Compression {
      compressionId = Identity
    , compress      = id
    , decompress    = id
    }