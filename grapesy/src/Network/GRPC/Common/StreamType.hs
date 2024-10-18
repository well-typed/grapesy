-- | Handlers for specific communication patterns
--
-- The
-- [gRPC specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
-- does not distinguish between different kinds of communication patterns;
-- this is done in the
-- [Protobuf specification](https://protobuf.dev/reference/protobuf/proto3-spec/#service_definition).
-- Nonetheless, these streaming types can be applied to other serialization
-- formats also.
--
-- It is important to realize that these wrappers are simple and are provided
-- for convenience only; if they do not provide the functionality you require,
-- using the more general interface from "Network.GRPC.Client" or
-- "Network.GRPC.Server" is not much more difficult.
--
-- See "Network.GRPC.Client" for additional discussion.
module Network.GRPC.Common.StreamType (
    -- * Abstraction
    StreamingType(..)
  , SupportsStreamingType
  , HasStreamingType(..)
  ) where

import Network.GRPC.Spec
