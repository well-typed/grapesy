module Network.GRPC.Server.StreamType (
    -- * Construct 'RpcHandler' from streaming type specific handler
    StreamingRpcHandler(..)
  , streamingRpcHandler'
    -- * Constructors
  , mkNonStreaming
  , mkClientStreaming
  , mkServerStreaming
  , mkBiDiStreaming
    -- * Server API
  , Methods(..)
  , Services(..)
  , fromMethods
  , fromServices
  ) where

import Control.Monad.IO.Class
import Data.Default
import Data.Kind
import Data.Proxy

import Network.GRPC.Common.StreamType
import Network.GRPC.Server
import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Construct 'RpcHandler'
-------------------------------------------------------------------------------}

class StreamingRpcHandler h where
  -- | Construct 'RpcHandler' from streaming type specific handler
  --
  -- Most applications will probably not need to call this function directly,
  -- instead relying on 'fromMethods'/'fromServices'. If however you want to
  -- construct a list of 'RpcHandler's manually, without a type-level
  -- specification of the server's API, you can use 'streamingRpcHandler'.
  streamingRpcHandler ::
        (MonadIO m, SupportsServerRpc rpc)
     => Proxy rpc -> h m rpc -> RpcHandler m

-- | Proxy-free wrapper around 'streamingRpcHandler'
--
-- This can be used if the streaming type is clear from context.
streamingRpcHandler' :: forall m rpc h.
       (MonadIO m, SupportsServerRpc rpc, StreamingRpcHandler h)
    => h m rpc -> RpcHandler m
streamingRpcHandler' = streamingRpcHandler (Proxy @rpc)

instance StreamingRpcHandler NonStreamingHandler where
  streamingRpcHandler proxy (UnsafeNonStreamingHandler h) =
    mkRpcHandler proxy $ \call -> do
      inp <- liftIO $ recvFinalInput call
      out <- h inp
      liftIO $ sendFinalOutput call (out, def)

instance StreamingRpcHandler ClientStreamingHandler where
  streamingRpcHandler proxy (UnsafeClientStreamingHandler h) =
    mkRpcHandler proxy $ \call -> do
      out <- h (liftIO $ recvInput call)
      liftIO $ sendFinalOutput call (out, def)

instance StreamingRpcHandler ServerStreamingHandler where
  streamingRpcHandler proxy (UnsafeServerStreamingHandler h) =
    mkRpcHandler proxy $ \call -> do
      inp <- liftIO $ recvFinalInput call
      h inp (liftIO . sendNextOutput call)
      liftIO $ sendTrailers call def

instance StreamingRpcHandler BiDiStreamingHandler where
  streamingRpcHandler proxy (UnsafeBiDiStreamingHandler h) =
    mkRpcHandler proxy $ \call -> do
      h (liftIO $ recvInput call)
        (liftIO . sendNextOutput call)
      liftIO $ sendTrailers call def

{-------------------------------------------------------------------------------
  Methods
-------------------------------------------------------------------------------}

-- | Declare handlers for a set of RPCs
--
-- See also 'fromMethods'.
--
-- == Example usage
--
-- Example that provides methods for the gRPC
-- [RouteGuide](https://grpc.io/docs/languages/python/basics/) API:
--
-- > handlers :: [Feature] -> Methods IO (ProtobufMethodsOf RouteGuide)
-- > handlers db =
-- >      Method (mkNonStreaming    $ getFeature   db)
-- >    $ Method (mkServerStreaming $ listFeatures db)
-- >    $ Method (mkClientStreaming $ recordRoute  db)
-- >    $ Method (mkBiDiStreaming   $ routeChat    db)
-- >    $ NoMoreMethods
--
-- It is also possibly to define your own list of RPC, instead of computing it
-- using @ProtobufMethodsOf@ (indeed, there is no need to use Protobuf at all).
--
-- == Taking advantage of type inference
--
-- Provided that you give a top-level type annotation, then type inference can
-- guide this definition:
--
-- > handlers :: [Feature] -> Methods IO (ProtobufMethodsOf RouteGuide)
-- > handlers db =
-- >      _methods
--
-- This will reveal that we need four methods:
--
-- > _methods :: Methods IO '[
-- >    RPC RouteGuide "getFeature"
-- >  , RPC RouteGuide "listFeatures"
-- >  , RPC RouteGuide "recordRoute"
-- >  , RPC RouteGuide "routeChat"
-- >  ]
--
-- We can use this to make a skeleton definition, with holes for each handler:
--
-- > handlers :: [Feature] -> Methods IO (ProtobufMethodsOf RouteGuide)
-- > handlers db =
-- >       Method _getFeature
-- >     $ Method _listFeatures
-- >     $ Method _recordRoute
-- >     $ Method _routeChar
-- >     $ NoMoreMethods
--
-- This will reveal types such as this:
--
-- > _getFeature :: NonStreamingHandler IO (RPC RouteGuide "getFeature")
--
-- Finally, if we then refine the skeleton to
--
-- > Method (mkNonStreaming $ _getFeature)
--
-- ghc will tell us
--
-- > _getFeature :: Point -> IO Feature
data Methods (m :: Type -> Type) (rpcs :: [Type]) where
  -- | All methods of the service handled
  NoMoreMethods :: Methods m '[]

  Method ::
       ( SupportsServerRpc rpc
       , StreamingRpcHandler h
       , h ~ HandlerFor (RpcStreamingType rpc)
       )
    => h m rpc
    -> Methods m rpcs
    -> Methods m (rpc ': rpcs)

  RawMethod ::
       RpcHandler m
    -> Methods m rpcs
    -> Methods m (rpc ': rpcs)

  UnsupportedMethod ::
       Methods m rpcs
    -> Methods m (rpc ': rpcs)

-- | Declare handlers for a set of services
--
-- See also 'fromServices'.
--
-- Example usage:
--
-- > services :: [Feature] -> Services IO (ProtobufServices '[Greeter, RouteGuide])
-- > services db =
-- >       Service Greeter.handlers
-- >     $ Service (RouteGuide.handlers db)
-- >     $ NoMoreServices
data Services m (servs :: [[Type]]) where
  NoMoreServices :: Services m '[]

  Service ::
       Methods m serv
    -> Services m servs
    -> Services m (serv : servs)

fromMethods :: forall m rpcs.
     MonadIO m
  => Methods m rpcs -> [RpcHandler m]
fromMethods = go
  where
    go :: Methods m rpcs' -> [RpcHandler m]
    go NoMoreMethods          = []
    go (Method h ms)          = streamingRpcHandler' h : go ms
    go (RawMethod m ms)       = m                      : go ms
    go (UnsupportedMethod ms) =                          go ms

fromServices :: forall m servs.
     MonadIO m
  => Services m servs -> [RpcHandler m]
fromServices = concat . go
  where
    go :: Services m servs' -> [[RpcHandler m]]
    go NoMoreServices = []
    go (Service s ss) = fromMethods s : go ss

