-- | Support for the protobuf @Any@ type
--
-- Official docs at <https://protobuf.dev/programming-guides/proto3/#any>.
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Common.Protobuf.Any (Any)
-- > import Network.GRPC.Common.Protobuf.Any qualified as Any
module Network.GRPC.Common.Protobuf.Any (
    Any

    -- * Packing and unpacking
  , UnpackError(..)
  , pack
  , unpack
  ) where

import Network.GRPC.Util.Imports

import Data.ProtoLens.Any (Any, UnpackError(..))
import Data.ProtoLens.Any qualified as Any
import Data.ProtoLens.Message (Message)

{-------------------------------------------------------------------------------
  Pack and unpack
-------------------------------------------------------------------------------}

pack :: Message a => Proto a -> Proto Any
pack = Proto . Any.pack . getProto

unpack :: Message a => Proto Any -> Either UnpackError (Proto a)
unpack = second Proto .  Any.unpack . getProto