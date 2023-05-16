module Network.GRPC.Server.Protobuf (
    RPC(..)

    -- * Handlers
  , MethodHandler(..)
  , MethodHandlers(..)
  , ServiceHandler(..)
  , ServiceHandlers(..)
  , protobufHandlers
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.ProtoLens.Service.Types

import Network.GRPC.Server
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Protobuf

{-------------------------------------------------------------------------------
  Typed handlers
-------------------------------------------------------------------------------}

data MethodHandler s m where
  HandleNonStreaming ::
       ( IsRPC (RPC s m)
       , MethodStreamingType s m ~ NonStreaming
       )
    => (   Input (RPC s m)
        -> IO (Output (RPC s m), Trailers)
       )
    -> MethodHandler s m

  HandleClientStreaming ::
       ( IsRPC (RPC s m)
       , MethodStreamingType s m ~ ClientStreaming
       )
    => IO ()
    -> MethodHandler s m

  HandleServerStreaming ::
       ( IsRPC (RPC s m)
       , MethodStreamingType s m ~ ServerStreaming
       )
    => (    Input (RPC s m)
         -> (Output (RPC s m) -> STM ())
         -> IO Trailers
       )
    -> MethodHandler s m

  HandleBiDiStreaming ::
       ( IsRPC (RPC s m)
       , MethodStreamingType s m ~ BiDiStreaming
       )
    => IO ()
    -> MethodHandler s m

data MethodHandlers s ms where
  AllMethodsHandled :: MethodHandlers s '[]
  HandleMethod ::
       MethodHandler  s m
    -> MethodHandlers s ms
    -> MethodHandlers s (m : ms)

data ServiceHandler s where
  ServiceHandler :: MethodHandlers s (ServiceMethods s) -> ServiceHandler s

data ServiceHandlers ss where
  AllServicesHandled :: ServiceHandlers '[]
  HandleService ::
       ServiceHandler  s
    -> ServiceHandlers ss
    -> ServiceHandlers (s : ss)

protobufHandlers :: ServiceHandlers ss -> [RpcHandler]
protobufHandlers = goServices []
  where
    goServices :: [[RpcHandler]] -> ServiceHandlers ss -> [RpcHandler]
    goServices acc AllServicesHandled   = concat acc
    goServices acc (HandleService s ss) = goServices (goService s : acc) ss

    goService :: ServiceHandler s -> [RpcHandler]
    goService (ServiceHandler ms) = goMethods [] ms

    goMethods :: [RpcHandler] -> MethodHandlers s ms -> [RpcHandler]
    goMethods acc AllMethodsHandled   = acc
    goMethods acc (HandleMethod m ms) = goMethods (protobufHandler m : acc) ms

protobufHandler :: forall s m. MethodHandler s m -> RpcHandler
protobufHandler (HandleNonStreaming h) = RpcHandler (RPC @s @m) $ \_call ->
    -- TODO: We should do the translation, once 'Call' is flushed out
    -- TODO: If the one input we get is not marked final, should we send
    -- a RST_STREAM?
    void $ h undefined
protobufHandler (HandleClientStreaming h) = RpcHandler (RPC @s @m) $ \_call ->
    h
protobufHandler (HandleServerStreaming h) = RpcHandler (RPC @s @m) $ \_call ->
    void $ h undefined undefined
protobufHandler (HandleBiDiStreaming h) = RpcHandler (RPC @s @m) $ \_call ->
    h
