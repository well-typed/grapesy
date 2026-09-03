-- | Like "Network.GRPC.Client.StreamType.IO", but with an implicit 'Connection'
--
-- These functions are useful in a monad stack in which the 'Connection' object
-- is implicitly available; you must provide an instance of 'CanCallRPC'.
module Network.GRPC.Client.StreamType.CanCallRPC (
    CanCallRPC(..)
    -- * Running client handlers
  , nonStreaming
  , clientStreaming
  , clientStreaming_
  , serverStreaming
  , biDiStreaming
  ) where

import Network.GRPC.Client.StreamType

