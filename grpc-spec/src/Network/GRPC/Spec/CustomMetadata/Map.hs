{-# LANGUAGE OverloadedStrings #-}

-- | Map of t'CustomMetadata', handling joining values
module Network.GRPC.Spec.CustomMetadata.Map (
    CustomMetadataMap -- opaque
    -- * Conversion
  , customMetadataMapFromList
  , customMetadataMapToList
    -- * Construction
  , customMetadataMapInsert
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import Network.GRPC.Spec.CustomMetadata.Raw

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Map from header names to values
--
-- The gRPC spec mandates
--
-- > Custom-Metadata header order is not guaranteed to be preserved except for
-- > values with duplicate header names. Duplicate header names may have their
-- > values joined with "," as the delimiter and be considered semantically
-- > equivalent.
--
-- Internally we don't allow for these duplicates, but instead join the headers
-- as mandated by the spec.
newtype CustomMetadataMap = CustomMetadataMap {
      getCustomMetadataMap :: Map HeaderName Strict.ByteString
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Dealing with duplicates
-------------------------------------------------------------------------------}

instance Monoid CustomMetadataMap where
  mempty = CustomMetadataMap Map.empty

instance Semigroup CustomMetadataMap where
  CustomMetadataMap x <> CustomMetadataMap y = CustomMetadataMap $
      Map.unionWith joinHeaderValue x y

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Construct t'CustomMetadataMap', joining duplicates
customMetadataMapFromList :: [CustomMetadata] -> CustomMetadataMap
customMetadataMapFromList =
      CustomMetadataMap
    . Map.fromListWith joinHeaderValue
    . map unpairCustomMetadata

-- | Flatten t'CustomMetadataMap' to a list
--
-- Precondition: the map must be valid.
customMetadataMapToList :: CustomMetadataMap -> [CustomMetadata]
customMetadataMapToList mds =
      map pairCustomMetadata
    . Map.toList
    . getCustomMetadataMap
    $ mds

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Insert value into t'CustomMetadataMap'
--
-- If a header with the same name already exists, the value is appended to
-- (the end of) the existing value.
customMetadataMapInsert ::
     CustomMetadata
  -> CustomMetadataMap -> CustomMetadataMap
customMetadataMapInsert (CustomMetadata name value) (CustomMetadataMap mds) =
    CustomMetadataMap $
      Map.insertWith (flip joinHeaderValue) name value mds

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

joinHeaderValue :: Strict.ByteString -> Strict.ByteString -> Strict.ByteString
joinHeaderValue x y = x <> "," <> y

unpairCustomMetadata :: CustomMetadata -> (HeaderName, Strict.ByteString)
unpairCustomMetadata (CustomMetadata name value) = (name, value)

pairCustomMetadata :: (HeaderName, Strict.ByteString) -> CustomMetadata
pairCustomMetadata md =
    fromMaybe (error $ "invalid " ++ show md) $
      uncurry safeCustomMetadata md
