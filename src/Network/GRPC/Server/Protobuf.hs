module Network.GRPC.Server.Protobuf (
    RPC(..)

    -- * Individual handlers
  , HandlerFor
  , HandlerForStreamingType

    -- * Services
  , Methods(..)
  , MethodsOf(..)
  , Services(..)
  , protobufServices
  )
  where

import Control.Concurrent.STM
import Control.Monad
import Data.Kind
import Data.ProtoLens.Service.Types

import Network.GRPC.Server
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Protobuf

{-------------------------------------------------------------------------------
  Singleton for 'StreamingType'
-------------------------------------------------------------------------------}

data SStreamingType :: StreamingType -> Type where
  SNonStreaming    :: SStreamingType 'NonStreaming
  SClientStreaming :: SStreamingType 'ClientStreaming
  SServerStreaming :: SStreamingType 'ServerStreaming
  SBiDiStreaming   :: SStreamingType 'BiDiStreaming

class IsStreamingType (t :: StreamingType) where
  isStreamingType :: SStreamingType t

instance IsStreamingType NonStreaming    where isStreamingType = SNonStreaming
instance IsStreamingType ClientStreaming where isStreamingType = SClientStreaming
instance IsStreamingType ServerStreaming where isStreamingType = SServerStreaming
instance IsStreamingType BiDiStreaming   where isStreamingType = SBiDiStreaming

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

type family HandlerFor s m where
  HandlerFor s m = HandlerForStreamingType s m (MethodStreamingType s m)

type family HandlerForStreamingType s m typ where
  HandlerForStreamingType s m NonStreaming =
       Input (RPC s m)
    -> IO (Output (RPC s m), Trailers)
  HandlerForStreamingType s m ClientStreaming =
      IO () -- TODO: Placeholder
  HandlerForStreamingType s m ServerStreaming =
       Input (RPC s m)
    -> (Output (RPC s m) -> STM ())
    -> IO Trailers
  HandlerForStreamingType s m BiDiStreaming =
      IO () -- TODO: Placeholder

{-------------------------------------------------------------------------------
  Services
-------------------------------------------------------------------------------}

data Methods s ms where
  NoMoreMethods :: Methods s '[]

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
  Method :: forall m s ms.
       ( IsRPC (RPC s m)
       , IsStreamingType (MethodStreamingType s m)
       )
    => HandlerFor s m
    -> Methods s ms
    -> Methods s (m ': ms)

newtype MethodsOf s = Methods {
      getMethods :: Methods s (ServiceMethods s)
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

protobufMethods :: forall s. MethodsOf s -> [RpcHandler]
protobufMethods = go . getMethods
  where
    go :: Methods s ms -> [RpcHandler]
    go NoMoreMethods     = []
    go ms@(Method h ms') = go' ms h : go ms'

    go' :: forall m ms.
         ( IsRPC (RPC s m)
         , IsStreamingType (MethodStreamingType s m)
         )
      => Methods s (m : ms) -> HandlerFor s m -> RpcHandler
    go' _proxy = protobufMethod (RPC @s @m)

protobufMethod :: forall s m.
     ( IsRPC (RPC s m)
     , IsStreamingType (MethodStreamingType s m)
     )
  => RPC s m -> HandlerFor s m -> RpcHandler
protobufMethod rpc h =
    case isStreamingType @(MethodStreamingType s m) of
      SNonStreaming    -> nonStreaming    rpc h
      SClientStreaming -> clientStreaming rpc h
      SServerStreaming -> serverStreaming rpc h
      SBiDiStreaming   -> biDiStreaming   rpc h

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

nonStreaming :: forall s m.
     ( IsRPC (RPC s m)
     , MethodStreamingType s m ~ NonStreaming
     )
  => RPC s m
  -> HandlerFor s m
  -> RpcHandler
nonStreaming rpc h = RpcHandler rpc $ \_call ->
    -- TODO: We should do the translation, once 'Call' is flushed out
    -- TODO: If the one input we get is not marked final, should we send
    -- a RST_STREAM?
    void $ h undefined

clientStreaming :: forall s m.
     ( IsRPC (RPC s m)
     , MethodStreamingType s m ~ ClientStreaming
     )
  => RPC s m
  -> HandlerFor s m
  -> RpcHandler
clientStreaming rpc h = RpcHandler rpc $ \_call ->
    h

serverStreaming :: forall s m.
     ( IsRPC (RPC s m)
     , MethodStreamingType s m ~ ServerStreaming
     )
  => RPC s m
  -> HandlerFor s m
  -> RpcHandler
serverStreaming rpc h = RpcHandler rpc $ \_call ->
    void $ h undefined undefined

biDiStreaming :: forall s m.
     ( IsRPC (RPC s m)
     , MethodStreamingType s m ~ BiDiStreaming
     )
  => RPC s m
  -> HandlerFor s m
  -> RpcHandler
biDiStreaming rpc h = RpcHandler rpc $ \_call ->
    h
