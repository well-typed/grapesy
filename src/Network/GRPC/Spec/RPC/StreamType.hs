-- | Streaming types
module Network.GRPC.Spec.RPC.StreamType (
    -- * Communication patterns
    StreamingType(..)
  , SupportsStreamingType
  , HasStreamingType(..)
    -- ** Handlers
  , NonStreamingHandler(..)
  , ClientStreamingHandler(..)
  , ServerStreamingHandler(..)
  , BiDiStreamingHandler(..)
  , HandlerFor
    -- ** Execution
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

-- Borrow protolens 'StreamingType' (but this module is not Protobuf specific)
import Data.ProtoLens.Service.Types (StreamingType(..))

import Network.GRPC.Common.StreamElem
import Network.GRPC.Spec.CustomMetadata.NoMetadata
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.RedundantConstraint

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
  type RpcStreamingType rpc :: StreamingType

{-------------------------------------------------------------------------------
  Handler types
-------------------------------------------------------------------------------}

newtype NonStreamingHandler m rpc = UnsafeNonStreamingHandler (
         Input rpc
      -> m (Output rpc)
    )

newtype ClientStreamingHandler m rpc = UnsafeClientStreamingHandler (
         m (StreamElem NoMetadata (Input rpc))
      -> m (Output rpc)
    )

newtype ServerStreamingHandler m rpc = UnsafeServerStreamingHandler (
         Input rpc
      -> (Output rpc -> m ())
      -> m ()
    )

newtype BiDiStreamingHandler m rpc = UnsafeBiDiStreamingHandler (
         m (StreamElem NoMetadata (Input rpc))
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

nonStreaming :: forall rpc m.
     SupportsStreamingType rpc NonStreaming
  => NonStreamingHandler m rpc
  -> Input rpc
  -> m (Output rpc)
nonStreaming (UnsafeNonStreamingHandler h) = h
  where
    _ = addConstraint @(SupportsStreamingType rpc NonStreaming)

clientStreaming :: forall rpc m.
     SupportsStreamingType rpc ClientStreaming
  => ClientStreamingHandler m rpc
  -> m (StreamElem NoMetadata (Input rpc))
  -> m (Output rpc)
clientStreaming (UnsafeClientStreamingHandler h) = h
  where
    _ = addConstraint @(SupportsStreamingType rpc ClientStreaming)

serverStreaming :: forall rpc m.
     SupportsStreamingType rpc ServerStreaming
  => ServerStreamingHandler m rpc
  -> Input rpc
  -> (Output rpc -> m ())
  -> m ()
serverStreaming (UnsafeServerStreamingHandler h) = h
  where
    _ = addConstraint @(SupportsStreamingType rpc ServerStreaming)

biDiStreaming :: forall rpc m.
     SupportsStreamingType rpc BiDiStreaming
  => BiDiStreamingHandler m rpc
  -> m (StreamElem NoMetadata (Input rpc))
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
    => (    m (StreamElem NoMetadata (Input rpc))
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
    => (    m (StreamElem NoMetadata (Input rpc))
         -> (Output rpc -> m ())
         -> m ()
       )
    -> BiDiStreamingHandler m rpc
mkBiDiStreaming = UnsafeBiDiStreamingHandler
  where
    _ = addConstraint @(SupportsStreamingType rpc BiDiStreaming)
