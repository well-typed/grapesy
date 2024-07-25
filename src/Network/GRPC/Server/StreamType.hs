{-# LANGUAGE FunctionalDependencies #-}

-- | Server handlers
module Network.GRPC.Server.StreamType (
    -- * Handler type
    ServerHandler'(..)
  , ServerHandler
    -- * Construct server handler
  , mkNonStreaming
  , mkClientStreaming
  , mkServerStreaming
  , mkBiDiStreaming
    -- * Server API
  , Methods(..)
  , Services(..)
  , fromMethod
  , fromMethods
  , fromServices
    -- ** Hoisting
  , hoistMethods
  , hoistServices
    -- * Varargs API
  , simpleMethods
  ) where

import Control.Monad.IO.Class
import Data.Default
import Data.Kind

import Network.GRPC.Common
import Network.GRPC.Common.NextElem qualified as NextElem
import Network.GRPC.Server
import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Construct server handler

  It may sometimes be useful to use explicit type applications with these
  functions, which is why the @rpc@ type variable is always first.
-------------------------------------------------------------------------------}

mkNonStreaming :: forall rpc m.
     SupportsStreamingType rpc NonStreaming
  => (    Input rpc
       -> m (Output rpc)
     )
  -> ServerHandler' NonStreaming m rpc
mkNonStreaming = ServerHandler

mkClientStreaming :: forall rpc m.
     SupportsStreamingType rpc ClientStreaming
  => (    IO (NextElem (Input rpc))
       -> m (Output rpc)
     )
  -> ServerHandler' ClientStreaming m rpc
mkClientStreaming = ServerHandler

mkServerStreaming :: forall rpc m.
     SupportsStreamingType rpc ServerStreaming
  => (    Input rpc
       -> (NextElem (Output rpc) -> IO ())
       -> m ()
     )
  -> ServerHandler' ServerStreaming m rpc
mkServerStreaming = ServerHandler

mkBiDiStreaming :: forall rpc m.
     SupportsStreamingType rpc BiDiStreaming
  => (    IO (NextElem (Input rpc))
       -> (NextElem (Output rpc) -> IO ())
       -> m ()
     )
  -> ServerHandler' BiDiStreaming m rpc
mkBiDiStreaming = ServerHandler . uncurry

{-------------------------------------------------------------------------------
  Run server handler (used internally only)
-------------------------------------------------------------------------------}

nonStreaming ::
     ServerHandler' NonStreaming m rpc
  -> Input rpc
  -> m (Output rpc)
nonStreaming (ServerHandler h) = h

clientStreaming ::
     ServerHandler' ClientStreaming m rpc
  -> IO (NextElem (Input rpc))
  -> m (Output rpc)
clientStreaming (ServerHandler h) = h

serverStreaming ::
     ServerHandler' ServerStreaming m rpc
  -> Input rpc
  -> (NextElem (Output rpc) -> IO ())
  -> m ()
serverStreaming (ServerHandler h) = h

biDiStreaming ::
    ServerHandler' BiDiStreaming m rpc
 -> IO (NextElem (Input rpc))
 -> (NextElem (Output rpc) -> IO ())
 -> m ()
biDiStreaming (ServerHandler h) = curry h

{-------------------------------------------------------------------------------
  Construct 'RpcHandler'
-------------------------------------------------------------------------------}

class FromStreamingHandler (styp :: StreamingType) where
  -- | Construct 'RpcHandler' from streaming type specific handler
  --
  -- Most applications will probably not need to call this function directly,
  -- instead relying on 'fromMethods'/'fromServices'. If however you want to
  -- construct a list of 'RpcHandler's manually, without a type-level
  -- specification of the server's API, you can use 'fromStreamingHandler'.
  fromStreamingHandler :: forall k (rpc :: k) m.
        ( SupportsServerRpc rpc
        , Default (ResponseInitialMetadata rpc)
        , Default (ResponseTrailingMetadata rpc)
        , MonadIO m
        )
     => ServerHandler' styp m rpc -> RpcHandler m rpc

instance FromStreamingHandler NonStreaming where
  fromStreamingHandler h = mkRpcHandler $ \call -> do
      inp <- liftIO $ recvFinalInput call
      out <- nonStreaming h inp
      liftIO $ sendFinalOutput call (out, def)

instance FromStreamingHandler ClientStreaming where
  fromStreamingHandler h = mkRpcHandler $ \call -> do
      out <- clientStreaming h (liftIO $ recvNextInputElem call)
      liftIO $ sendFinalOutput call (out, def)

instance FromStreamingHandler ServerStreaming where
  fromStreamingHandler h = mkRpcHandler $ \call -> do
      inp <- liftIO $ recvFinalInput call
      serverStreaming h inp (liftIO . sendOutput call . fromNextElem call)

instance FromStreamingHandler BiDiStreaming where
  fromStreamingHandler h = mkRpcHandler $ \call -> do
      biDiStreaming h
        (liftIO $ recvNextInputElem call)
        (liftIO . sendOutput call . fromNextElem call)

{-------------------------------------------------------------------------------
  Internal: dealing with metadata
-------------------------------------------------------------------------------}

fromNextElem ::
     Default (ResponseTrailingMetadata rpc)
  => proxy rpc
  -> NextElem out
  -> StreamElem (ResponseTrailingMetadata rpc) out
fromNextElem _ = NextElem.toStreamElem def

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
       ( SupportsServerRpc rpc
       , Default (ResponseInitialMetadata rpc)
       , Default (ResponseTrailingMetadata rpc)
       , SupportsStreamingType rpc styp
       )
    => ServerHandler' styp m rpc
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

-- | Construct 'SomeRpcHandler' from a streaming handler
--
-- Most users will not need to call this function, but it can occassionally be
-- useful when using the lower-level API. Depending on usage you may need to
-- provide a type argument to fix the @rpc@, for example
--
-- > Server.fromMethod @EmptyCall $ ServerHandler $ \(_ ::Empty) ->
-- >   return (defMessage :: Empty)
fromMethod :: forall rpc styp m.
     ( SupportsServerRpc rpc
     , ValidStreamingType styp
     , Default (ResponseInitialMetadata rpc)
     , Default (ResponseTrailingMetadata rpc)
     , MonadIO m
     )
  => ServerHandler' styp m rpc -> SomeRpcHandler m
fromMethod =
    case validStreamingType (Proxy @styp) of
      SNonStreaming    -> someRpcHandler . fromStreamingHandler
      SClientStreaming -> someRpcHandler . fromStreamingHandler
      SServerStreaming -> someRpcHandler . fromStreamingHandler
      SBiDiStreaming   -> someRpcHandler . fromStreamingHandler

fromMethods :: forall m rpcs.
     MonadIO m
  => Methods m rpcs -> [SomeRpcHandler m]
fromMethods = go
  where
    go :: Methods m rpcs' -> [SomeRpcHandler m]
    go NoMoreMethods          = []
    go (Method m ms)          = fromMethod     m : go ms
    go (RawMethod m ms)       = someRpcHandler m : go ms
    go (UnsupportedMethod ms) =                    go ms

fromServices :: forall m servs.
     MonadIO m
  => Services m servs -> [SomeRpcHandler m]
fromServices = concat . go
  where
    go :: Services m servs' -> [[SomeRpcHandler m]]
    go NoMoreServices = []
    go (Service s ss) = fromMethods s : go ss

{-------------------------------------------------------------------------------
  Hoisting
-------------------------------------------------------------------------------}

hoistMethods :: forall m n rpcs.
     (forall a. m a -> n a)
  -> Methods m rpcs
  -> Methods n rpcs
hoistMethods f = go
  where
    go :: forall rpcs'. Methods m rpcs' -> Methods n rpcs'
    go NoMoreMethods          = NoMoreMethods
    go (Method          h ms) = Method (hoistServerHandler f h) (go ms)
    go (RawMethod       h ms) = RawMethod (hoistRpcHandler f h) (go ms)
    go (UnsupportedMethod ms) = UnsupportedMethod (go ms)

hoistServices :: forall m n servs.
     (forall a. m a -> n a)
  -> Services m servs
  -> Services n servs
hoistServices f = go
  where
    go :: forall servs'. Services m servs' -> Services n servs'
    go NoMoreServices = NoMoreServices
    go (Service s ss) = Service (hoistMethods f s) (go ss)

{-------------------------------------------------------------------------------
  Varargs API
-------------------------------------------------------------------------------}

class SimpleMethods m (rpcs :: [k]) (rpcs' :: [k]) a | a -> m rpcs rpcs' where
  simpleMethods' :: (Methods m rpcs -> Methods m rpcs') -> a

instance SimpleMethods m '[] rpcs (Methods m rpcs) where
  simpleMethods' f = f NoMoreMethods

instance
    ( -- Requirements inherited from the 'Method' constructor
      SupportsServerRpc rpc
    , Default (ResponseInitialMetadata rpc)
    , Default (ResponseTrailingMetadata rpc)
    , SupportsStreamingType rpc (RpcStreamingType rpc)
      -- Requirements for the vararg construction
    , b ~ ServerHandler' (RpcStreamingType rpc) m rpc
    , SimpleMethods m rpcs rpcs' a
    ) => SimpleMethods m (rpc : rpcs) rpcs' (b -> a) where
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
