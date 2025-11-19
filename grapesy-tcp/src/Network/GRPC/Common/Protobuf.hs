module Network.GRPC.Common.Protobuf (

    -- ** "Data.ProtoLens"
    StreamingType(..)
  , HasField(..)
  , FieldDefault(..)
  , Message(defMessage)
) where

import Data.ProtoLens.Field (HasField(..), field)
import Data.ProtoLens.Message (FieldDefault(..), Message(defMessage))
import Network.GRPC.Spec (StreamingType (..))
