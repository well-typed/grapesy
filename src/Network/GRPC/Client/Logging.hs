-- | Log messages emitted by the client
--
-- These are for debugging only.
module Network.GRPC.Client.Logging (
    LogMsg(..)
  , LogRPC(..)
  ) where

import Data.ByteString.Lazy qualified as Lazy

import Network.GRPC.Spec (IsFinal)
import Network.GRPC.Spec.RPC (IsRPC, Input)
import Network.GRPC.Spec.HTTP2.LengthPrefixed (MessagePrefix)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data LogMsg =
    forall rpc. IsRPC rpc => LogRPC rpc (LogRPC rpc)

deriving instance Show LogMsg

data LogRPC rpc =
    WaitingForMessage
  | SendInput (Maybe (Input rpc)) IsFinal
  | WaitingForResponseChunk (Maybe MessagePrefix) Lazy.ByteString

deriving instance IsRPC rpc => Show (LogRPC rpc)
