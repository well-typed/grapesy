-- | Execute handlers for specific communication patterns
--
-- See also "Network.GRPC.Common.StreamType" as well as
-- "Network.GRPC.Client.StreamType.CanCallRPC".
module Network.GRPC.Client.StreamType.IO (
    module Network.GRPC.Client.StreamType
    -- * Execution
  , nonStreaming
  , clientStreaming
  , serverStreaming
  , biDiStreaming
  ) where

import Control.Monad.Reader

import Network.GRPC.Client
import Network.GRPC.Common.StreamElem
import Network.GRPC.Common.StreamType
import Network.GRPC.Spec (IsRPC(..), NoMetadata)
import Network.GRPC.Spec qualified as Spec
import Network.GRPC.Client.StreamType hiding (CanCallRPC(..))

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

-- | Make a non-streaming RPC
--
-- Example usage:
--
-- > features <-
-- >   nonStreaming conn (rpc @(Protobuf RouteGuide "getFeature")) point
-- > logMsg features
nonStreaming :: forall rpc m.
     SupportsStreamingType rpc NonStreaming
  => Connection
  -> NonStreamingHandler (ReaderT Connection m) rpc
  -> Input rpc
  -> m (Output rpc)
nonStreaming conn h input =
    flip runReaderT conn $
      Spec.nonStreaming h input

-- | Make a client-side streaming RPC
--
-- Example usage:
--
-- > summary <-
-- >   clientStreaming conn (rpc @(Protobuf RouteGuide "recordRoute")) getPoint
clientStreaming :: forall rpc m.
     (SupportsStreamingType rpc ClientStreaming, Monad m)
  => Connection
  -> ClientStreamingHandler (ReaderT Connection m) rpc
  -> m (StreamElem NoMetadata (Input rpc))
  -> m (Output rpc)
clientStreaming conn h produceInput =
    flip runReaderT conn $
      Spec.clientStreaming h (lift produceInput)

-- | Make a server-side streaming RPC
--
-- Example usage:
--
-- > serverStreaming conn (rpc @(Protobuf RouteGuide "listFeatures")) rect $
-- >   logMsg
serverStreaming :: forall rpc m.
     (SupportsStreamingType rpc ServerStreaming, Monad m)
  => Connection
  -> ServerStreamingHandler (ReaderT Connection m) rpc
  -> Input rpc
  -> (Output rpc -> m ())
  -> m ()
serverStreaming conn h input processOutput =
    flip runReaderT conn $
      Spec.serverStreaming h input (lift . processOutput)

-- | Make a bidirectional RPC
--
-- Example usage:
--
-- > biDiStreaming conn (rpc @(Protobuf RouteGuide "routeChat")) getNote $
-- >   logMsg
biDiStreaming :: forall rpc m.
     (SupportsStreamingType rpc BiDiStreaming, Monad m)
  => Connection
  -> BiDiStreamingHandler (ReaderT Connection m) rpc
  -> m (StreamElem NoMetadata (Input rpc))
  -> (Output rpc -> m ())
  -> m ()
biDiStreaming conn h produceInput processOutput =
    flip runReaderT conn $
      Spec.biDiStreaming h (lift produceInput) (lift . processOutput)