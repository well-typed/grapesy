module Network.GRPC.Spec.CustomMetadata.NoMetadata (
    NoMetadata(..)
  ) where

import Control.Monad.Catch
import Data.Default.Class

import Network.GRPC.Spec.CustomMetadata.Typed

-- | Indicate the absence of custom metadata
data NoMetadata = NoMetadata
  deriving stock (Show, Eq)

instance Default NoMetadata where
  def = NoMetadata

instance BuildMetadata NoMetadata where
  buildMetadata NoMetadata = []

instance ParseMetadata NoMetadata where
  parseMetadata []   = return NoMetadata
  parseMetadata hdrs = throwM $ UnexpectedMetadata hdrs

instance StaticMetadata NoMetadata where
  metadataHeaderNames _ = []
