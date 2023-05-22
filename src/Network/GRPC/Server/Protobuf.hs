module Network.GRPC.Server.Protobuf (
    RPC(..)

    -- * Services
  , Methods(..)
  , MethodsOf(..)
  , Services(..)
  , protobufServices
  )
  where

import Data.ProtoLens.Service.Types

import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Protobuf

{-------------------------------------------------------------------------------
  Services
-------------------------------------------------------------------------------}

data Methods serv ms where
  NoMoreMethods :: Methods serv '[]

  -- | Define handler for the next method of the service
  --
  -- Example usage:
  --
  -- > handleGreeter :: MethodsOf Greeter
  -- > handleGreeter = Methods $
  -- >       Method handleSayHello
  -- >     $ Method handleSayHelloStreamReply
  -- >     $ NoMoreMethods
  --
  -- It is advisable to provide the type annotation
  -- (@MethodsOf Greeter@)
  -- to aid type inference.
  Method :: forall serv m ms.
       ( IsRPC (RPC serv m)
       , IsStreamingType (MethodStreamingType serv m)
       )
    => Handler IO serv m
    -> Methods serv ms
    -> Methods serv (m ': ms)

newtype MethodsOf serv = Methods {
      getMethods :: Methods serv (ServiceMethods serv)
    }

data Services ss where
  NoMoreServices :: Services '[]

  -- | Add supported service
  --
  -- Example usage:
  --
  -- > services ::
  -- > services =
  -- >       Service handleGreeter
  -- >     $ Service handleRouteGuide
  -- >     $ NoMoreServices
  --
  -- It is advisable to add the type annotation
  -- (@Services '[Greeter, RouteGuide]@)
  -- to add type inference.
  Service ::
       MethodsOf s
    -> Services ss
    -> Services (s : ss)

{-------------------------------------------------------------------------------
  Translate all handlers for a set of services
-------------------------------------------------------------------------------}

protobufServices :: Services ss -> [RpcHandler]
protobufServices = concat . go
  where
    go :: Services ss -> [[RpcHandler]]
    go NoMoreServices = []
    go (Service s ss) = protobufMethods s : go ss

protobufMethods :: forall serv. MethodsOf serv -> [RpcHandler]
protobufMethods = go . getMethods
  where
    go :: Methods serv ms -> [RpcHandler]
    go NoMoreMethods     = []
    go ms@(Method h ms') = go' ms h : go ms'

    go' :: forall m ms.
         ( IsRPC (RPC serv m)
         , IsStreamingType (MethodStreamingType serv m)
         )
      => Methods serv (m : ms) -> Handler IO serv m -> RpcHandler
    go' _proxy = protobufMethod (RPC @serv @m)

protobufMethod :: forall serv meth.
     ( IsRPC (RPC serv meth)
     , IsStreamingType (MethodStreamingType serv meth)
     )
  => RPC serv meth -> Handler IO serv meth -> RpcHandler
protobufMethod rpc h =
    case isStreamingType @(MethodStreamingType serv meth) of
      SNonStreaming    -> nonStreaming    rpc h
      SClientStreaming -> clientStreaming rpc h
      SServerStreaming -> serverStreaming rpc h
      SBiDiStreaming   -> biDiStreaming   rpc h

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

nonStreaming :: forall serv meth.
     ( IsRPC (RPC serv meth)
     , MethodStreamingType serv meth ~ NonStreaming
     )
  => RPC serv meth -> Handler IO serv meth -> RpcHandler
nonStreaming rpc h = defRpcHandler rpc $ \call -> do
    inp <- recvOnlyInput call
    sendOnlyOutput call =<< h inp

clientStreaming :: forall serv meth.
     ( IsRPC (RPC serv meth)
     , MethodStreamingType serv meth ~ ClientStreaming
     )
  => RPC serv meth -> Handler IO serv meth -> RpcHandler
clientStreaming rpc h = defRpcHandler rpc $ \call ->
    sendOnlyOutput call =<< h (recvNextInput call)

serverStreaming :: forall serv meth.
     ( IsRPC (RPC serv meth)
     , MethodStreamingType serv meth ~ ServerStreaming
     )
  => RPC serv meth -> Handler IO serv meth -> RpcHandler
serverStreaming rpc h = defRpcHandler rpc $ \call -> do
    inp <- recvOnlyInput call
    sendTrailers call =<< h inp (sendNextOutput call)

biDiStreaming :: forall serv meth.
     ( IsRPC (RPC serv meth)
     , MethodStreamingType serv meth ~ BiDiStreaming
     )
  => RPC serv meth -> Handler IO serv meth -> RpcHandler
biDiStreaming rpc h = defRpcHandler rpc $ \call ->
    sendTrailers call =<< h (recvNextInput call) (sendNextOutput call)
