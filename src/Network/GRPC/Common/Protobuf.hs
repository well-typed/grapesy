-- | Common functionality for working with Protobuf
module Network.GRPC.Common.Protobuf (
    Protobuf

    -- * Re-exports
    -- ** "Data.Function"
  , (&)
    -- ** "Control.Lens"
  , (.~)
  , (^.)
    -- ** "Data.ProtoLens"
  , HasField(..)
  , defMessage
  ) where

import Control.Lens ((.~), (^.))
import Data.Function ((&))
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Field (HasField(..))
import Data.ProtoLens.Labels () -- provides instances for OverloadedLabels

import Network.GRPC.Spec (Protobuf)
