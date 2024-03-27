module Network.GRPC.Spec.CustomMetadata.NoMetadata (
    NoMetadata(..)
  , UnexpectedMetadata(..)
  ) where

import Control.Exception
import Control.Monad.Catch
import Data.Default

import Network.GRPC.Spec.CustomMetadata.Raw
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

data UnexpectedMetadata = UnexpectedMetadata [CustomMetadata]
  deriving stock (Show)
  deriving anyclass (Exception)

