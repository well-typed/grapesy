-- | Common functionality for working with Protobuf
module Network.GRPC.Common.Protobuf (
    Protobuf
  , Proto(..)
  , getProto

    -- * Re-exports
    -- ** "Data.Function"
  , (&)
    -- ** "Control.Lens"
  , (.~)
  , (^.)
  , (%~)
    -- ** "Data.ProtoLens"
  , StreamingType(..)
  , HasField(..)
  , FieldDefault(..)
  , Message(defMessage)
  ) where

import Control.Lens ((.~), (^.), (%~))
import Data.Function ((&))
import Data.ProtoLens.Field (HasField(..))
import Data.ProtoLens.Labels () -- provides instances for OverloadedLabels
import Data.ProtoLens.Message (FieldDefault(..), Message(defMessage))

import Network.GRPC.Spec
