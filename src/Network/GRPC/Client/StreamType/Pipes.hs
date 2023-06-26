-- | [pipes](https://hackage.haskell.org/package/pipes) interface
--
-- This module is primarily intended as a proof of concept, illustrating how the
-- library could interact with streaming interfaces.
module Network.GRPC.Client.StreamType.Pipes (
    clientStreaming
  , serverStreaming
  , biDiStreaming
  ) where

import Data.ProtoLens.Service.Types
import Data.Proxy
import Pipes hiding (Proxy)
import Pipes.Safe

import Network.GRPC.Client
import Network.GRPC.Client.StreamType
import Network.GRPC.Common.StreamType (SupportsStreamingType)
import Network.GRPC.Common.StreamType qualified as StreamType
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.RedundantConstraint

{-------------------------------------------------------------------------------
  Pipes for different kinds of streaming types (communication patterns)
-------------------------------------------------------------------------------}

clientStreaming :: forall rpc.
     IsRPC rpc
  => SupportsStreamingType rpc ClientStreaming
  => Connection
  -> CallParams
  -> Proxy rpc
  -> Consumer' (StreamElem () (Input rpc)) (SafeT IO) (Output rpc)
clientStreaming conn params proxy =
    StreamType.clientStreaming
      (rpcWith conn params proxy)
      await

serverStreaming :: forall rpc.
     IsRPC rpc
  => SupportsStreamingType rpc ServerStreaming
  => Connection
  -> CallParams
  -> Proxy rpc
  -> Input rpc
  -> Producer' (Output rpc) (SafeT IO) ()
serverStreaming conn params proxy input =
   StreamType.serverStreaming
     (rpcWith conn params proxy)
     input
     yield

-- | Bidirectional streaming
--
-- This function is of a slightly different nature as 'clientStreaming' and
-- 'serverStreaming': since we need a resource (the RPC call) in /both/ pipes,
-- it's more challenging to allocate it /inside/ the pipe. Perhaps with a
-- careful design this is solvable, but for now we simply allocate it on the
-- outside, and use \"with\" style here.
--
-- TODO: Another thing to consider here whether @pipes-concurrency@ might offer
-- some insights here. However, for now this entire module is just meant as
-- an illustration of how you could integrate @grapesy@ with a streaming lib.
biDiStreaming :: forall rpc a.
     IsRPC rpc
  => SupportsStreamingType rpc BiDiStreaming
  => Connection
  -> CallParams
  -> Proxy rpc
  -> (    Consumer' (StreamElem () (Input rpc)) IO ()
       -> Producer' (Output rpc) IO ()
       -> IO a
     )
  -> IO a
biDiStreaming conn params proxy k =
    withRPC conn params proxy $ \call ->
      k (sendAllInputs call await) (void $ recvAllOutputs call yield)
  where
    _ = addConstraint @(SupportsStreamingType rpc BiDiStreaming)
