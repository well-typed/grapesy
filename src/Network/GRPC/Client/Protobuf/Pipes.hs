-- | [pipes](https://hackage.haskell.org/package/pipes) interface
--
-- This module is primarily intended as a proof of concept, illustrating how the
-- library could interact with streaming interfaces.
module Network.GRPC.Client.Protobuf.Pipes (
    RPC(..)
  , clientStreaming
  , serverStreaming
  , biDiStreaming
  ) where

import Data.ProtoLens.Service.Types
import Data.Proxy
import Pipes
import Pipes.Safe

import Network.GRPC.Client
import Network.GRPC.Client.Protobuf (rpcWith)
import Network.GRPC.Client.Protobuf qualified as Protobuf
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Protobuf
import Network.GRPC.Util.RedundantConstraint

{-------------------------------------------------------------------------------
  Pipes for Protobuf communication patterns
-------------------------------------------------------------------------------}

clientStreaming :: forall serv meth.
     IsRPC (RPC serv meth)
  => MethodStreamingType serv meth ~ ClientStreaming
  => Connection
  -> CallParams
  -> RPC serv meth
  -> Consumer'
       (StreamElem () (MethodInput serv meth))
       (SafeT IO)
       (MethodOutput serv meth)
clientStreaming call params _proxy =
    Protobuf.clientStreaming
      (rpcWith @serv @meth call params)
      await

serverStreaming :: forall serv meth.
     IsRPC (RPC serv meth)
  => MethodStreamingType serv meth ~ ServerStreaming
  => Connection
  -> CallParams
  -> RPC serv meth
  -> MethodInput serv meth
  -> Producer' (MethodOutput serv meth) (SafeT IO) ()
serverStreaming conn params _proxy input =
   Protobuf.serverStreaming
     (rpcWith @serv @meth conn params)
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
biDiStreaming :: forall serv meth a.
     IsRPC (RPC serv meth)
  => MethodStreamingType serv meth ~ BiDiStreaming
  => Connection
  -> CallParams
  -> RPC serv meth
  -> (    Consumer' (StreamElem () (MethodInput serv meth)) IO ()
       -> Producer' (MethodOutput serv meth) IO ()
       -> IO a
     )
  -> IO a
biDiStreaming conn params proxy k =
    withRPC conn params proxy $ \call ->
      k (sendAllInputs call await) (void $ recvAllOutputs call yield)
  where
    _ = addConstraint $ Proxy @(MethodStreamingType serv meth ~ BiDiStreaming)
