-- | Client handlers
--
-- This is an internal module only; see "Network.GRPC.Client.StreamType.IO"
-- for the main public module.
module Network.GRPC.Client.StreamType (
    -- * Handler type
    ClientHandler' -- opaque
  , ClientHandler
    -- * Run client handlers (part of the public API)
  , nonStreaming
  , clientStreaming
  , clientStreaming_
  , serverStreaming
  , biDiStreaming
    -- * Obtain handler for a specific type
  , CanCallRPC(..)
  , rpc
  , rpcWith
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Proxy

import Network.GRPC.Client.Call
import Network.GRPC.Client.Connection
import Network.GRPC.Common
import Network.GRPC.Common.NextElem qualified as NextElem
import Network.GRPC.Common.StreamType
import Network.GRPC.Spec

{------------------------------------------------------------------------------
  Constructing client handlers (used internally only)
-------------------------------------------------------------------------------}

mkNonStreaming :: forall rpc m.
     SupportsStreamingType rpc NonStreaming
  => (    Input rpc
       -> m (Output rpc)
     )
  -> ClientHandler' NonStreaming m rpc
mkNonStreaming h = ClientHandler $
    h

mkClientStreaming :: forall rpc m.
     SupportsStreamingType rpc ClientStreaming
  => ( forall r.
          (    (NextElem (Input rpc) -> IO ())
             -> m r
          )
       -> m (Output rpc, r)
     )
  -> ClientHandler' ClientStreaming m rpc
mkClientStreaming h = ClientHandler $
    Negative h

mkServerStreaming :: forall rpc m.
     (SupportsStreamingType rpc ServerStreaming, Functor m)
  => ( forall r.
            Input rpc
         -> (    IO (NextElem (Output rpc))
              -> m r
            )
         -> m r
     )
  -> ClientHandler' ServerStreaming m rpc
mkServerStreaming h = ClientHandler $ \input ->
    Negative $ \k -> ((),) <$> h input k

mkBiDiStreaming :: forall rpc m.
     (SupportsStreamingType rpc BiDiStreaming, Functor m)
  => ( forall r.
           (    (NextElem (Input rpc) -> IO ())
             -> IO (NextElem (Output rpc))
             -> m r
           )
        -> m r
     )
  -> ClientHandler' BiDiStreaming m rpc
mkBiDiStreaming h = ClientHandler $
    Negative $ \k -> fmap ((),) $ h (curry k)

{-------------------------------------------------------------------------------
  Running client handlers
-------------------------------------------------------------------------------}

-- | Execute non-streaming handler in any monad stack
nonStreaming :: forall rpc m.
     ClientHandler' NonStreaming m rpc
  -> Input rpc
  -> m (Output rpc)
nonStreaming (ClientHandler h) = h

-- | Generalization of 'clientStreaming_' with an additional result
clientStreaming :: forall rpc m r.
     ClientHandler' ClientStreaming m rpc
  -> (  (NextElem (Input rpc) -> IO ())
       -> m r
     )
  -> m (Output rpc, r)
clientStreaming (ClientHandler h) k = runNegative h k

-- | Execute client-side streaming handler in any monad stack
clientStreaming_ :: forall rpc m.
     Functor m
  => ClientHandler' ClientStreaming m rpc
  -> (    (NextElem (Input rpc) -> IO ())
       -> m ()
     )
  -> m (Output rpc)
clientStreaming_ h k = fst <$> clientStreaming h k

-- | Execute server-side streaming handler in any monad stack
serverStreaming :: forall rpc m r.
     Functor m
  => ClientHandler' ServerStreaming m rpc
  -> Input rpc
  -> (    IO (NextElem (Output rpc))
       -> m r
     )
  -> m r
serverStreaming (ClientHandler h) input k = snd <$> runNegative (h input) k

-- | Execute bidirectional streaming handler in any monad stack
biDiStreaming :: forall rpc m r.
     Functor m
  => ClientHandler' BiDiStreaming m rpc
  -> (    (NextElem (Input rpc) -> IO ())
       -> IO (NextElem (Output rpc))
       -> m r
     )
  -> m r
biDiStreaming (ClientHandler h) k = fmap snd $ runNegative h $ uncurry k

{-------------------------------------------------------------------------------
  CanCallRPC
-------------------------------------------------------------------------------}

-- | Monads in which we make RPC calls
--
-- In order to be able to make an RPC call, we need
--
-- * 'MonadIO' (obviously)
-- * 'MonadMask' in order to ensure that the RPC call is terminated cleanly
-- * Access to the 'Connection' to the server
class (MonadIO m, MonadMask m) => CanCallRPC m where
  getConnection :: m Connection

instance (MonadIO m, MonadMask m) => CanCallRPC (ReaderT Connection m) where
  getConnection = ask

{-------------------------------------------------------------------------------
  Obtain handler for specific RPC call
-------------------------------------------------------------------------------}

class MkStreamingHandler (styp :: StreamingType) where
  mkStreamingHandler ::
       ( CanCallRPC m
       , SupportsClientRpc rpc
       , SupportsStreamingType rpc styp
       )
    => CallParams rpc -> ClientHandler' styp m rpc

-- | Construct RPC handler
--
-- This has an ambiguous type, and is intended to be called using a type
-- application indicating the @rpc@ method to call, such as
--
-- > rpc @Ping
--
-- provided that @Ping@ is some type with an 'IsRPC' instance. In some cases
-- it may also be needed to provide a streaming type:
--
-- > rpc @Ping @NonStreaming
--
-- though in most cases the streaming type should be clear from the context or
-- from the choice of @rpc@.
--
-- See 'Network.GRPC.Client.StreamType.IO.nonStreaming' and co for examples.
-- See also 'rpcWith'.
rpc :: forall rpc styp m.
     ( CanCallRPC m
     , SupportsClientRpc rpc
     , SupportsStreamingType rpc styp
     , Default (RequestMetadata rpc)
     )
  => ClientHandler' styp m rpc
rpc = rpcWith def

-- | Generalization of 'rpc' with custom 'CallParams'
rpcWith :: forall rpc styp m.
     ( CanCallRPC m
     , SupportsClientRpc rpc
     , SupportsStreamingType rpc styp
     )
  => CallParams rpc -> ClientHandler' styp m rpc
rpcWith =
    case validStreamingType (Proxy @styp) of
      SNonStreaming    -> mkStreamingHandler
      SClientStreaming -> mkStreamingHandler
      SServerStreaming -> mkStreamingHandler
      SBiDiStreaming   -> mkStreamingHandler

instance MkStreamingHandler NonStreaming where
  mkStreamingHandler :: forall rpc m.
       ( CanCallRPC m
       , SupportsClientRpc rpc
       , SupportsStreamingType rpc NonStreaming
       )
    => CallParams rpc -> ClientHandler' NonStreaming m rpc
  mkStreamingHandler params = mkNonStreaming $ \input -> do
      conn <- getConnection
      withRPC conn params (Proxy @rpc) $ \call -> do
        sendFinalInput call input
        (output, _trailers) <- recvFinalOutput call
        return output

instance MkStreamingHandler ClientStreaming where
  mkStreamingHandler :: forall rpc m.
       ( CanCallRPC m
       , SupportsClientRpc rpc
       , SupportsStreamingType rpc ClientStreaming
       )
    => CallParams rpc -> ClientHandler' ClientStreaming m rpc
  mkStreamingHandler params = mkClientStreaming $ \k -> do
      conn <- getConnection
      withRPC conn params (Proxy @rpc) $ \call -> do
        r <- k (sendInput call . fromNextElem)
        (output, _trailers) <- recvFinalOutput call
        return (output, r)

instance MkStreamingHandler ServerStreaming where
  mkStreamingHandler :: forall rpc m.
       ( CanCallRPC m
       , SupportsClientRpc rpc
       , SupportsStreamingType rpc ServerStreaming
       )
    => CallParams rpc -> ClientHandler' ServerStreaming m rpc
  mkStreamingHandler params = mkServerStreaming $ \input k -> do
      conn <- getConnection
      withRPC conn params (Proxy @rpc) $ \call -> do
        sendFinalInput call input
        k (recvNextOutputElem call)

instance MkStreamingHandler BiDiStreaming where
  mkStreamingHandler :: forall rpc m.
       ( CanCallRPC m
       , SupportsClientRpc rpc
       , SupportsStreamingType rpc BiDiStreaming
       )
    => CallParams rpc -> ClientHandler' BiDiStreaming m rpc
  mkStreamingHandler params = mkBiDiStreaming $ \k -> do
      conn <- getConnection
      withRPC conn params (Proxy @rpc) $ \call ->
        k (sendInput call . fromNextElem)
          (recvNextOutputElem call)

{-------------------------------------------------------------------------------
  Internal: dealing with metadata
-------------------------------------------------------------------------------}

fromNextElem :: NextElem inp -> StreamElem NoMetadata inp
fromNextElem = NextElem.toStreamElem NoMetadata
