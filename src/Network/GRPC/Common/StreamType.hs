-- | Handlers for specific communication patterns
--
-- See 'for a d ~scussion of Protobuf versus other
-- serialization formats in relation to specific communication patterns.
--
-- It is important to realize that these wrappers are simple and are provided
-- for convenience only; if they do not provide the functionality you require,
-- using the more general interface from "Network.GRPC.Client" or
-- "Network.GRPC.Server" is not much more difficult.
module Network.GRPC.Common.StreamType (
    -- * Abstraction
    StreamingType(..)
  , SupportsStreamingType
  , HasStreamingType(..)
    -- * Handlers
  , HandlerFor
  , NonStreamingHandler(..)
  , ClientStreamingHandler(..)
  , ServerStreamingHandler(..)
  , BiDiStreamingHandler(..)
    -- * Execution
  , nonStreaming
  , clientStreaming
  , serverStreaming
  , biDiStreaming
    -- * Construction
  , mkNonStreaming
  , mkClientStreaming
  , mkServerStreaming
  , mkBiDiStreaming
  ) where

import Network.GRPC.Spec
