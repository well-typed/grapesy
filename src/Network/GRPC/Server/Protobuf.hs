module Network.GRPC.Server.Protobuf (
    RPC(..)

    -- * Services
  , Methods(..)
  , MethodsOf
  , Services(..)
  , protobufServices

    -- * Handlers
  , NonStreamingHandler    -- opaque
  , ClientStreamingHandler -- opaque
  , ServerStreamingHandler -- opaque
  , BiDiStreamingHandler   -- opaque
    -- ** Execution
  , RunHandler(..)
    -- ** Construction
  , nonStreaming
  , clientStreaming
  , serverStreaming
  , biDiStreaming
  ) where

import Control.Monad.IO.Class
import Data.ProtoLens.Service.Types

import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Protobuf

{-------------------------------------------------------------------------------
  Services
-------------------------------------------------------------------------------}

-- | Declare handlers for all methods of a service
--
-- Example usage:
--
-- > handlers :: MethodsOf IO Greeter
-- > handlers = Methods $
-- >       Method sayHello
-- >     $ Method sayHelloStreamReply
-- >     $ NoMoreMethods
--
-- Taking advantage of type inference: as long as you give a signature to the
-- definition of handlers, the types of the handlers can be inferred:
--
-- > handlers :: MethodsOf IO Greeter
-- > handlers =
-- >      Method _sayHello
-- >    $ Method _sayHelloStreamReply
-- >    $ NoMoreMethods
--
-- will result in two holes of the correct type:
--
-- > _sayHello            :: NonStreamingHandler    IO Greeter "sayHello"
-- > _sayHelloStreamReply :: ServerStreamingHandler IO Greeter "sayHelloStreamReply"
data Methods m serv meths where
  NoMoreMethods :: Methods m serv '[]

  Method ::
       ( IsRPC (RPC serv meth)
       , h ~ HandlerFor (MethodStreamingType serv meth)
       , RunHandler h
       )
    => h m serv meth
    -> Methods m serv meths
    -> Methods m serv (meth ': meths)

type MethodsOf m serv = Methods m serv (ServiceMethods serv)

-- | Declare handlers for all services supported by a server
--
-- Example usage:
--
-- > services :: [Feature] -> Services IO '[Greeter, RouteGuide]
-- > services db =
-- >       Service Greeter.handlers
-- >     $ Service (RouteGuide.handlers db)
-- >     $ NoMoreServices
data Services m servs where
  NoMoreServices :: Services m '[]

  Service ::
       MethodsOf m serv
    -> Services m servs
    -> Services m (serv : servs)

{-------------------------------------------------------------------------------
  Translate all handlers for a set of services
-------------------------------------------------------------------------------}

protobufServices :: forall m ss.
     MonadIO m
  => Services m ss -> [RpcHandler m]
protobufServices = concat . go
  where
    go :: Services m ss' -> [[RpcHandler m]]
    go NoMoreServices = []
    go (Service s ss) = protobufMethods s : go ss

protobufMethods :: forall m serv.
     MonadIO m
  => MethodsOf m serv -> [RpcHandler m]
protobufMethods = go
  where
    go :: Methods m serv meths -> [RpcHandler m]
    go NoMoreMethods = []
    go (Method m ms) = runHandler m : go ms

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

class RunHandler h where
  runHandler' ::
       (MonadIO m, IsRPC (RPC serv meth))
    => RPC serv meth -> h m serv meth -> RpcHandler m

runHandler :: forall m serv meth h.
     (MonadIO m, IsRPC (RPC serv meth), RunHandler h)
  => h m serv meth -> RpcHandler m
runHandler = runHandler' (RPC @serv @meth)

instance RunHandler NonStreamingHandler where
  runHandler' rpc (NonStreamingHandler h) =
    defRpcHandler rpc $ \call -> do
      inp <- liftIO $ recvOnlyInput call
      out <- h inp
      liftIO $ sendOnlyOutput call out

instance RunHandler ClientStreamingHandler where
  runHandler' rpc (ClientStreamingHandler h) =
    defRpcHandler rpc $ \call -> do
      out <- h (liftIO $ recvNextInput call)
      liftIO $ sendOnlyOutput call out

instance RunHandler ServerStreamingHandler where
  runHandler' rpc (ServerStreamingHandler h) =
    defRpcHandler rpc $ \call -> do
      inp      <- liftIO $ recvOnlyInput call
      trailers <- h inp (liftIO . sendNextOutput call)
      liftIO $ sendTrailers call trailers

instance RunHandler BiDiStreamingHandler where
  runHandler' rpc (BiDiStreamingHandler h) =
    defRpcHandler rpc $ \call -> do
      trailers <- h (liftIO $ recvNextInput  call)
                    (liftIO . sendNextOutput call)
      liftIO $ sendTrailers call trailers

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

nonStreaming :: forall m serv meth.
     (    MethodInput serv meth
       -> m (MethodOutput serv meth, [CustomMetadata])
     )
  -> NonStreamingHandler m serv meth
nonStreaming = NonStreamingHandler

clientStreaming :: forall m serv meth.
     (    m (StreamElem () (MethodInput serv meth))
       -> m (MethodOutput serv meth, [CustomMetadata])
     )
  -> ClientStreamingHandler m serv meth
clientStreaming = ClientStreamingHandler

serverStreaming :: forall m serv meth.
     (    MethodInput serv meth
       -> (MethodOutput serv meth -> m ()) -> m [CustomMetadata]
     )
  -> ServerStreamingHandler m serv meth
serverStreaming = ServerStreamingHandler

biDiStreaming :: forall m serv meth.
     (    m (StreamElem () (MethodInput serv meth))
       -> (MethodOutput serv meth -> m ()) -> m [CustomMetadata]
     )
  -> BiDiStreamingHandler m serv meth
biDiStreaming = BiDiStreamingHandler

