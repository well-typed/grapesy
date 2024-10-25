-- | Streaming types
module Network.GRPC.Spec.RPC.StreamType (
    StreamingType(..)
    -- * Link RPCs to streaming types
  , SupportsStreamingType
  , HasStreamingType(..)
    -- * Handler type definition
  , NextElem(..)
  , Send
  , Recv
  , Positive
  , Negative(..)
  , HandlerRole(..)
  , Handler
    -- * Handler newtype wrappers
  , ServerHandler'(..)
  , ServerHandler
  , ClientHandler'(..)
  , ClientHandler
    -- * Singleton
  , SStreamingType(..)
  , ValidStreamingType(..)
    -- * Hoisting
  , hoistServerHandler
  ) where

-- Borrow protolens 'StreamingType' (but this module is not Protobuf specific)
import Data.ProtoLens.Service.Types (StreamingType(..))

import Data.Kind
import Data.Proxy

import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  Link RPCs to streaming types
-------------------------------------------------------------------------------}

-- | This RPC supports the given streaming type
--
-- This is a weaker condition than 'HasStreamingType': some (non-Protobuf) RPCs
-- may support more than one streaming type.
class ValidStreamingType styp
   => SupportsStreamingType rpc (styp :: StreamingType)

-- | /The/ streaming type supported by this RPC

-- This is a stronger condition than 'SupportsStreamingType': we associate the
-- RPC with one /specific/ streaming type.
class SupportsStreamingType rpc (RpcStreamingType rpc)
   => HasStreamingType rpc where
  -- | The (single) streaming type supported by this RPC
  type RpcStreamingType rpc :: StreamingType

{-------------------------------------------------------------------------------
  Internal: preliminaries

  'NextElem' does not allow to mark the final streaming element as final /as/ we
  send it; instead, we must use 'NoMoreElems' (which will correspond to an
  additional empty HTTP data frame); similarly, on the input side we cannot
  detect that an element is final as we receive it. This does not matter in the
  vast majority of cases; for the rare case where it does matter, user code can
  simply use the core API instead of the streaming API.

  It /is/ important that we mark the final message as final for /non-streaming/
  cases; some servers get confused when this is not the case. However, this is
  fine: in the non-streaming case we take care of on behalf of the user, and
  don't use 'NextElem' at all.
-------------------------------------------------------------------------------}

-- | Is there a next element in a stream?
data NextElem a = NoNextElem | NextElem !a
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

-- | Send a value
type Send a = NextElem a -> IO ()

-- | Receive a value
--
-- 'Nothing' indicates no more values. Calling this function again after
-- receiving 'Nothing' is a bug.
type Recv a = IO (NextElem a)

-- | Positive use of @a@
type Positive m a b = a -> m b

-- | Negative use of @a@
newtype Negative m a b = Negative {
      runNegative :: forall r. (a -> m r) -> m (b, r)
    }

{-------------------------------------------------------------------------------
  Handler

  The handler definitions are carefully designed to make the duality between the
  server and the client obvious.
-------------------------------------------------------------------------------}

-- | Handler role
data HandlerRole =
    Server  -- ^ Deal with an incoming request
  | Client  -- ^ Initiate an outgoing request

-- | Type of a handler
type family Handler (r :: HandlerRole) (s :: StreamingType) m (rpc :: k) where
  Handler Server NonStreaming    m rpc = Input rpc -> m (Output rpc)
  Handler Client NonStreaming    m rpc = Input rpc -> m (Output rpc)

  Handler Server ClientStreaming m rpc = Positive m (Recv (Input rpc)) (Output rpc)
  Handler Client ClientStreaming m rpc = Negative m (Send (Input rpc)) (Output rpc)

  Handler Server ServerStreaming m rpc = Input rpc -> Positive m (Send (Output rpc)) ()
  Handler Client ServerStreaming m rpc = Input rpc -> Negative m (Recv (Output rpc)) ()

  Handler Server BiDiStreaming   m rpc = Positive m (Recv (Input rpc), Send (Output rpc)) ()
  Handler Client BiDiStreaming   m rpc = Negative m (Send (Input rpc), Recv (Output rpc)) ()

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

-- | Wrapper around @Handler Server@ to avoid ambiguous types
data ServerHandler' (styp :: StreamingType) m (rpc :: k) where
  ServerHandler ::
       SupportsStreamingType rpc styp
    => Handler Server styp m rpc
    -> ServerHandler' styp m rpc

-- | Wrapper around @Handler Client@ to avoid ambiguous types
data ClientHandler' (s :: StreamingType) m (rpc :: k) where
  ClientHandler ::
       SupportsStreamingType rpc styp
    => Handler Client styp m rpc
    -> ClientHandler' styp m rpc

-- | Alias for 'ServerHandler'' with the streaming type determined by the @rpc@
type ServerHandler m rpc = ServerHandler' (RpcStreamingType rpc) m rpc

-- | Alias for 'ClientHandler'' with the streaming type determined by the @rpc@
type ClientHandler m rpc = ClientHandler' (RpcStreamingType rpc) m rpc

{-------------------------------------------------------------------------------
  Singleton
-------------------------------------------------------------------------------}

-- | Singleton for 'StreamingType'
data SStreamingType :: StreamingType -> Type where
  SNonStreaming    :: SStreamingType NonStreaming
  SClientStreaming :: SStreamingType ClientStreaming
  SServerStreaming :: SStreamingType ServerStreaming
  SBiDiStreaming   :: SStreamingType BiDiStreaming

-- | Valid streaming types
class ValidStreamingType (styp :: StreamingType) where
  -- | Obtain singleton
  validStreamingType :: Proxy styp -> SStreamingType styp

instance ValidStreamingType NonStreaming    where validStreamingType _ = SNonStreaming
instance ValidStreamingType ClientStreaming where validStreamingType _ = SClientStreaming
instance ValidStreamingType ServerStreaming where validStreamingType _ = SServerStreaming
instance ValidStreamingType BiDiStreaming   where validStreamingType _ = SBiDiStreaming

{-------------------------------------------------------------------------------
  Hoisting
-------------------------------------------------------------------------------}

class HoistServerHandler styp where
  hoistServerHandler' ::
       (forall a. m a -> n a)
    -> ServerHandler' styp m rpc
    -> ServerHandler' styp n rpc

instance HoistServerHandler NonStreaming where
  hoistServerHandler' f (ServerHandler h) = ServerHandler $ \inp ->
      f $ h inp

instance HoistServerHandler ClientStreaming where
  hoistServerHandler' f (ServerHandler h) = ServerHandler $ \recv ->
      f $ h recv

instance HoistServerHandler ServerStreaming where
  hoistServerHandler' f (ServerHandler h) = ServerHandler $ \inp send ->
      f $ h inp send

instance HoistServerHandler BiDiStreaming where
  hoistServerHandler' f (ServerHandler h) = ServerHandler $ \(recv, send) ->
      f $ h (recv, send)

-- | Hoist server handler from one monad to another
hoistServerHandler :: forall styp m n rpc.
     ValidStreamingType styp
  => (forall a. m a -> n a)
  -> ServerHandler' styp m rpc
  -> ServerHandler' styp n rpc
hoistServerHandler f =
    case validStreamingType (Proxy @styp) of
      SNonStreaming    -> hoistServerHandler' f
      SClientStreaming -> hoistServerHandler' f
      SServerStreaming -> hoistServerHandler' f
      SBiDiStreaming   -> hoistServerHandler' f
