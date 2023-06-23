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
    -- * Communication patterns
    StreamingType(..)
  , SupportsStreamingType
  , HasStreamingType(..)
    -- * Handler types
  , NonStreamingHandler(..)
  , ClientStreamingHandler(..)
  , ServerStreamingHandler(..)
  , BiDiStreamingHandler(..)
  , HandlerFor
    -- ** Destructors
  , nonStreaming
  , clientStreaming
  , serverStreaming
  , biDiStreaming
    -- ** Constructors
  , mkNonStreaming
  , mkClientStreaming
  , mkServerStreaming
  , mkBiDiStreaming
  ) where

import Data.Kind
import Network.GRPC.Common.StreamElem (StreamElem)
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.RedundantConstraint

-- Borrow protolens 'StreamingType' (but this module is not Protobuf specific)
import Data.ProtoLens.Service.Types (StreamingType(..))

{-------------------------------------------------------------------------------
  Communication patterns
-------------------------------------------------------------------------------}

-- | This RPC supports the given streaming type
--
-- This is a weaker condition than 'HasStreamingType', which maps each RPC to
-- a /specific/ streaming type. Some (non-Protobuf) RPCs however may support
-- more than one streaming type.
class SupportsStreamingType rpc (styp :: StreamingType)

class SupportsStreamingType rpc (RpcStreamingType rpc)
   => HasStreamingType rpc where
  -- | Streaming type supported by this RPC
  --
  -- The
  -- [gRPC specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
  -- does not distinguish between different kinds of communication patterns;
  -- this is done in the
  -- [Protobuf specification](https://protobuf.dev/reference/protobuf/proto3-spec/#service_definition).
  -- Nonetheless, these streaming types can be applied to other serialization
  -- formats also.
  type RpcStreamingType rpc :: StreamingType

{-------------------------------------------------------------------------------
  Handler types

  Users don't typically work with these types directly; however, they serve
  as as specification of the commonality between the client and the server.
-------------------------------------------------------------------------------}

newtype NonStreamingHandler m rpc = UnsafeNonStreamingHandler (
         Input rpc
      -> m (Output rpc)
    )

newtype ClientStreamingHandler m rpc = UnsafeClientStreamingHandler (
         m (StreamElem () (Input rpc))
      -> m (Output rpc)
    )

newtype ServerStreamingHandler m rpc = UnsafeServerStreamingHandler (
         Input rpc
      -> (Output rpc -> m ())
      -> m ()
    )

newtype BiDiStreamingHandler m rpc = UnsafeBiDiStreamingHandler (
         m (StreamElem () (Input rpc))
      -> (Output rpc -> m ())
      -> m ()
    )

-- | Match 'StreamingType' to handler type
--
-- This is occassionally useful to improve type inference. Users are not
-- expected to need to use this in their own code.
type family HandlerFor (typ :: StreamingType) :: (Type -> Type) -> Type -> Type where
  HandlerFor NonStreaming    = NonStreamingHandler
  HandlerFor ClientStreaming = ClientStreamingHandler
  HandlerFor ServerStreaming = ServerStreamingHandler
  HandlerFor BiDiStreaming   = BiDiStreamingHandler

{-------------------------------------------------------------------------------
  Destructors

  These are just the record field accessors, but with an additional constraint
  on them that ensures that the right type of handler is used with the right
  type of RPC.
-------------------------------------------------------------------------------}

-- | Make a non-streaming RPC
--
-- Example usage:
--
-- > features <-
-- >   nonStreaming (rpc @(Protobuf RouteGuide "getFeature") conn) point
-- > logMsg features
nonStreaming :: forall rpc m.
     SupportsStreamingType rpc NonStreaming
  => NonStreamingHandler m rpc
  -> Input rpc
  -> m (Output rpc)
nonStreaming (UnsafeNonStreamingHandler h) = h
  where
    _ = addConstraint @(SupportsStreamingType rpc NonStreaming)

-- | Make a client-side streaming RPC
--
-- Example usage:
--
-- > summary <-
-- >   clientStreaming (rpc @(Protobuf RouteGuide "recordRoute") conn) getPoint
clientStreaming :: forall rpc m.
     SupportsStreamingType rpc ClientStreaming
  => ClientStreamingHandler m rpc
  -> m (StreamElem () (Input rpc))
  -> m (Output rpc)
clientStreaming (UnsafeClientStreamingHandler h) = h
  where
    _ = addConstraint @(SupportsStreamingType rpc ClientStreaming)

-- | Make a server-side streaming RPC
--
-- Example usage:
--
-- > serverStreaming (rpc @(Protobuf RouteGuide "listFeatures") conn) rect $
-- >   logMsg
serverStreaming :: forall rpc m.
     SupportsStreamingType rpc ServerStreaming
  => ServerStreamingHandler m rpc
  -> Input rpc
  -> (Output rpc -> m ())
  -> m ()
serverStreaming (UnsafeServerStreamingHandler h) = h
  where
    _ = addConstraint @(SupportsStreamingType rpc ServerStreaming)

-- | Make a bidirectional RPC
--
-- Example usage:
--
-- > biDiStreaming (rpc @(Protobuf RouteGuide "routeChat") conn) getNote $
-- >   logMsg
biDiStreaming :: forall rpc m.
     SupportsStreamingType rpc BiDiStreaming
  => BiDiStreamingHandler m rpc
  -> m (StreamElem () (Input rpc))
  -> (Output rpc -> m ())
  -> m ()
biDiStreaming (UnsafeBiDiStreamingHandler h) = h
  where
    _ = addConstraint @(SupportsStreamingType rpc BiDiStreaming)

{-------------------------------------------------------------------------------
  Constructors

  These are just the newtype constructors, but with an additional constraint,
  similar to the destructors above.
-------------------------------------------------------------------------------}

mkNonStreaming :: forall m rpc.
       SupportsStreamingType rpc NonStreaming
    => (    Input rpc
         -> m (Output rpc)
       )
    -> NonStreamingHandler m rpc
mkNonStreaming = UnsafeNonStreamingHandler
  where
    _ = addConstraint @(SupportsStreamingType rpc NonStreaming)

mkClientStreaming :: forall m rpc.
       SupportsStreamingType rpc ClientStreaming
    => (    m (StreamElem () (Input rpc))
         -> m (Output rpc)
       )
    -> ClientStreamingHandler m rpc
mkClientStreaming = UnsafeClientStreamingHandler
  where
    _ = addConstraint @(SupportsStreamingType rpc ClientStreaming)

mkServerStreaming :: forall m rpc.
       SupportsStreamingType rpc ServerStreaming
    => (    Input rpc
         -> (Output rpc -> m ())
         -> m ()
       )
    -> ServerStreamingHandler m rpc
mkServerStreaming = UnsafeServerStreamingHandler
  where
    _ = addConstraint @(SupportsStreamingType rpc ServerStreaming)

mkBiDiStreaming :: forall m rpc.
       SupportsStreamingType rpc BiDiStreaming
    => (    m (StreamElem () (Input rpc))
         -> (Output rpc -> m ())
         -> m ()
       )
    -> BiDiStreamingHandler m rpc
mkBiDiStreaming = UnsafeBiDiStreamingHandler
  where
    _ = addConstraint @(SupportsStreamingType rpc BiDiStreaming)
