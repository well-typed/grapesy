-- | [pipes](https://hackage.haskell.org/package/pipes) interface
--
-- This module is primarily intended as a proof of concept, illustrating how the
-- library could interact with streaming interfaces.
module Network.GRPC.Client.StreamType.Pipes (
    clientStreaming
  , serverStreaming
  , biDiStreaming
  ) where

import Control.Monad.Reader
import Data.ProtoLens.Service.Types
import Pipes hiding (Proxy)
import Pipes.Safe

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO qualified as IO
import Network.GRPC.Common
import Network.GRPC.Common.StreamType
import Network.GRPC.Util.RedundantConstraint

{-------------------------------------------------------------------------------
  Pipes for different kinds of streaming types (communication patterns)
-------------------------------------------------------------------------------}

clientStreaming ::
     ( SupportsStreamingType rpc ClientStreaming
     , consumer ~ Consumer (StreamElem NoMetadata (Input rpc)) (SafeT IO)
     )
  => Connection
  -> ClientStreamingHandler (ReaderT Connection consumer) rpc
  -> consumer (Output rpc)
clientStreaming conn h = IO.clientStreaming conn h await

serverStreaming ::
     ( SupportsStreamingType rpc ServerStreaming
     , producer ~ Producer (Output rpc) (SafeT IO)
     )
  => Connection
  -> ServerStreamingHandler (ReaderT Connection producer) rpc
  -> Input rpc -> producer ()
serverStreaming conn h input = IO.serverStreaming conn h input yield

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
     SupportsClientRpc rpc
  => SupportsStreamingType rpc BiDiStreaming
  => Connection
  -> CallParams
  -> Proxy rpc
  -> (    Consumer' (StreamElem NoMetadata (Input rpc)) IO ()
       -> Producer' (Output rpc) IO ()
       -> IO a
     )
  -> IO a
biDiStreaming conn params proxy k =
    withRPC conn params proxy $ \call ->
      k (sendAllInputs call await) (void $ recvAllOutputs call yield)
  where
    _ = addConstraint @(SupportsStreamingType rpc BiDiStreaming)
