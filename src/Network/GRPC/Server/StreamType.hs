{-# LANGUAGE FunctionalDependencies #-}
module Network.GRPC.Server.StreamType (
    -- * Construct 'RpcHandler' from streaming type specific handler
    StreamingRpcHandler(..)
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
    -- * Varargs API
  , simpleMethods
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

class StreamingRpcHandler (h :: (Type -> Type) -> k -> Type) where
  -- | Construct 'RpcHandler' from streaming type specific handler
  --
  -- Most applications will probably not need to call this function directly,
  -- instead relying on 'fromMethods'/'fromServices'. If however you want to
  -- construct a list of 'RpcHandler's manually, without a type-level
  -- specification of the server's API, you can use 'streamingRpcHandler'.
  streamingRpcHandler :: forall (rpc :: k) m.
        ( SupportsServerRpc rpc
        , Default (ResponseInitialMetadata rpc)
        , Default (ResponseTrailingMetadata rpc)
        , MonadIO m
        )
     => h m rpc -> RpcHandler m rpc

instance StreamingRpcHandler NonStreamingHandler where
  streamingRpcHandler (UnsafeNonStreamingHandler h) =
    mkRpcHandler $ \call -> do
      inp <- liftIO $ recvFinalInput call
      out <- h inp
      liftIO $ sendFinalOutput call (out, def)

instance StreamingRpcHandler ClientStreamingHandler where
  streamingRpcHandler (UnsafeClientStreamingHandler h) =
    mkRpcHandler $ \call -> do
      out <- h (liftIO $ recvInput call)
      liftIO $ sendFinalOutput call (out, def)

instance StreamingRpcHandler ServerStreamingHandler where
  streamingRpcHandler (UnsafeServerStreamingHandler h) =
    mkRpcHandler $ \call -> do
      inp <- liftIO $ recvFinalInput call
      h inp (liftIO . sendNextOutput call)
      liftIO $ sendTrailers call def

instance StreamingRpcHandler BiDiStreamingHandler where
  streamingRpcHandler (UnsafeBiDiStreamingHandler h) =
    mkRpcHandler $ \call -> do
      h (liftIO $ recvInput call)
        (liftIO . sendNextOutput call)
      liftIO $ sendTrailers call def

{-------------------------------------------------------------------------------
  Methods
-------------------------------------------------------------------------------}

-- | Declare handlers for a set of RPCs
--
-- See also 'simpleMethods' for an alternative API if you only need the 'Method'
-- constructor.
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
-- > handlers db = _methods
--
-- This will reveal that we need four methods:
--
-- > _methods :: Methods IO '[
-- >     Protobuf RouteGuide "getFeature"
-- >   , Protobuf RouteGuide "listFeatures"
-- >   , Protobuf RouteGuide "recordRoute"
-- >   , Protobuf RouteGuide "routeChat"
-- >   ]
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
-- > _getFeature :: NonStreamingHandler IO (Protobuf RouteGuide "getFeature")
--
-- Finally, if we then refine the skeleton to
--
-- > Method (mkNonStreaming $ _getFeature)
--
-- ghc will tell us
--
-- > _getFeature :: Point -> IO Feature
data Methods (m :: Type -> Type) (rpcs :: [k]) where
  -- | All methods of the service handled
  NoMoreMethods :: Methods m '[]

  -- | Define the next method of the service, inferring the streaming type
  --
  -- In the most common case (Protobuf), the streaming type can be inferred
  -- from the method. In this case, it is convenient to use 'Method', as type
  -- inference will tell you what kind of handler you need to define (see also
  -- the example above).
  Method ::
       ( StreamingRpcHandler h
       , SupportsServerRpc rpc
       , Default (ResponseInitialMetadata rpc)
       , Default (ResponseTrailingMetadata rpc)
       , h ~ HandlerFor (RpcStreamingType rpc)
       )
    => h m rpc
    -> Methods m rpcs
    -> Methods m (rpc ': rpcs)

  -- | Like 'Method', but with a user-defined streaming type
  --
  -- This is useful for 'IsRPC' instances where the streaming type cannot be
  -- inferred from the @rpc@ method.
  MethodOfStreamingType ::
       ( StreamingRpcHandler h
       , SupportsServerRpc rpc
       , Default (ResponseInitialMetadata rpc)
       , Default (ResponseTrailingMetadata rpc)
       , SupportsStreamingType rpc styp
       , h ~ HandlerFor styp
       )
    => Proxy (styp :: StreamingType)
    -> h m rpc
    -> Methods m rpcs
    -> Methods m (rpc ': rpcs)

  -- | Define a method that uses uses the raw (core) API instead
  --
  -- This is useful when the communication pattern does not fall neatly into the
  -- four streaming types (non-streaming, client-side streaming, server-side
  -- streaming, or bidirectional streaming), or when you need access to lower
  -- level features such as request or response metadata, compression options,
  -- etc.
  RawMethod ::
       SupportsServerRpc rpc
    => RpcHandler m rpc
    -> Methods m rpcs
    -> Methods m (rpc ': rpcs)

  -- | Declare that this particular @rpc@ method is not supported by this server
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
data Services m (servs :: [[k]]) where
  NoMoreServices :: Services m '[]

  Service ::
       Methods m serv
    -> Services m servs
    -> Services m (serv : servs)

fromMethods :: forall m rpcs.
     MonadIO m
  => Methods m rpcs -> [SomeRpcHandler m]
fromMethods = go
  where
    go :: Methods m rpcs' -> [SomeRpcHandler m]
    go NoMoreMethods                  = []
    go (Method h ms)                  = someRpcHandler (streamingRpcHandler h) : go ms
    go (MethodOfStreamingType _ h ms) = someRpcHandler (streamingRpcHandler h) : go ms
    go (RawMethod m ms)               = someRpcHandler m                       : go ms
    go (UnsupportedMethod ms)         =                                          go ms

fromServices :: forall m servs.
     MonadIO m
  => Services m servs -> [SomeRpcHandler m]
fromServices = concat . go
  where
    go :: Services m servs' -> [[SomeRpcHandler m]]
    go NoMoreServices = []
    go (Service s ss) = fromMethods s : go ss

{-------------------------------------------------------------------------------
  Varargs API
-------------------------------------------------------------------------------}

class SimpleMethods m (rpcs :: [k]) (rpcs' :: [k]) a | a -> m rpcs rpcs' where
  simpleMethods' :: (Methods m rpcs -> Methods m rpcs') -> a

instance SimpleMethods m '[] rpcs (Methods m rpcs) where
  simpleMethods' f = f NoMoreMethods

instance
  ( -- Requirements inherited from the 'Method' constructor
    StreamingRpcHandler h
  , SupportsServerRpc rpc
  , Default (ResponseInitialMetadata rpc)
  , Default (ResponseTrailingMetadata rpc)
  , h ~ HandlerFor (RpcStreamingType rpc)
    -- Requirements for the vararg construction
  , b ~ h m rpc
  , SimpleMethods m rpcs rpcs' a
  ) => SimpleMethods m (rpc ': rpcs) rpcs' (b -> a) where
  simpleMethods' f h = simpleMethods' (f . Method h)

-- | Alternative way to construct 'Methods'
--
-- == Example usage
--
-- Listing the handlers for the gRPC routeguide server using 'Methods' directly
-- looks like this:
--
-- >   Method (mkNonStreaming    $ getFeature   db)
-- > $ Method (mkServerStreaming $ listFeatures db)
-- > $ Method (mkClientStreaming $ recordRoute  db)
-- > $ Method (mkBiDiStreaming   $ routeChat    db)
-- > $ NoMoreMethods
--
-- Since we only use 'Method' here, we can instead write this as
--
-- > simpleMethods
-- >   (mkNonStreaming    $ getFeature   db)
-- >   (mkServerStreaming $ listFeatures db)
-- >   (mkClientStreaming $ recordRoute  db)
-- >   (mkBiDiStreaming   $ routeChat    db)
--
-- Which API you prefer is mostly just a matter of taste.
simpleMethods :: SimpleMethods m rpcs rpcs a => a
simpleMethods = simpleMethods' id
