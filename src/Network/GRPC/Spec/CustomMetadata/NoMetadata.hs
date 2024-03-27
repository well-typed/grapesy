module Network.GRPC.Spec.CustomMetadata.NoMetadata (
    NoMetadata(..)
  ) where

-- | Indicate the absence of custom metadata
data NoMetadata = NoMetadata
  deriving stock (Show, Eq)

