-- | [pipes](https://hackage.haskell.org/package/pipes) interface
--
-- This module is primarily intended as a proof of concept, illustrating how the
-- library could interact with streaming interfaces.
module Network.GRPC.Protobuf.Pipes (
    RPC(..)
  , clientStreaming
  , serverStreaming
  , biDiStreaming
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.ProtoLens.Service.Types
import Data.Proxy
import Pipes
import Pipes.Safe

import Network.GRPC.Client
import Network.GRPC.Protobuf (expectedOutput, expectedTrailers)
import Network.GRPC.Protobuf.RequireStreamingType
import Network.GRPC.Spec
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Protobuf

{-------------------------------------------------------------------------------
  Pipes for Protobuf communication patterns
-------------------------------------------------------------------------------}

clientStreaming :: forall s m.
     IsRPC (RPC s m)
  => MethodStreamingType s m ~ ClientStreaming
  => Connection
  -> RequestMeta
  -> RPC s m
  -> Consumer'
       (IsFinal, Input (RPC s m))
       (SafeT IO)
       (Output (RPC s m), Trailers)
clientStreaming conn meta rpc =
    requireStreamingType rpc (Proxy @ClientStreaming) $
      withRPC conn meta rpc $ \call -> do
        sendAll call
        liftIO $ do
          output   <- atomically $ recvOutput call
                               >>= either (expectedOutput rpc) return
          trailers <- atomically $ recvOutput call
                               >>= either return (expectedTrailers rpc)
          return (output, trailers)

serverStreaming :: forall s m.
     IsRPC (RPC s m)
  => MethodStreamingType s m ~ ServerStreaming
  => Connection
  -> RequestMeta
  -> RPC s m
  -> Input (RPC s m)
  -> Producer' (Output (RPC s m)) (SafeT IO) Trailers
serverStreaming conn meta rpc input =
    requireStreamingType rpc (Proxy @ServerStreaming) $
      withRPC conn meta rpc $ \call -> aux call
  where
    aux ::
         OpenCall (RPC s m)
      -> Producer' (Output (RPC s m)) (SafeT IO) Trailers
    aux call = do
        liftIO $ atomically $ sendInput call Final $ Just input
        recvAll call

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
biDiStreaming :: forall s m a.
     IsRPC (RPC s m)
  => MethodStreamingType s m ~ BiDiStreaming
  => Connection
  -> RequestMeta
  -> RPC s m
  -> (    Consumer' (IsFinal, Input (RPC s m)) IO ()
       -> Producer' (Output (RPC s m)) IO Trailers
       -> IO a
     )
  -> IO a
biDiStreaming conn meta rpc k =
    requireStreamingType rpc (Proxy @BiDiStreaming) $
      withRPC conn meta rpc $ \call ->
        k (sendAll call) (recvAll call)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

sendAll :: forall f s m.
     MonadIO f
  => OpenCall (RPC s m)
  -> Consumer' (IsFinal, Input (RPC s m)) f ()
sendAll call = loop
  where
    loop :: Consumer' (IsFinal, Input (RPC s m)) f ()
    loop = do
        (isFinal, input) <- await
        liftIO $ atomically $ sendInput call isFinal $ Just input
        unless (isFinal == Final) loop

recvAll :: forall f s m.
     MonadIO f
  => OpenCall (RPC s m)
  -> Producer' (Output (RPC s m)) f Trailers
recvAll call = loop
  where
    loop :: Producer' (Output (RPC s m)) f Trailers
    loop = do
        mOutput <- liftIO $ atomically $ recvOutput call
        case mOutput of
          Left  trailers -> return trailers
          Right output   -> yield output >> loop