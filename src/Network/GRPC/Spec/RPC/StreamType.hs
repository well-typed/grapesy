-- | Streaming types
--
-- TODO: Check haddocks
module Network.GRPC.Spec.RPC.StreamType (
    StreamingType(..)
    -- * Link RPCs to streaming types
  , SupportsStreamingType
  , HasStreamingType(..)
    -- * Handler type definition
    -- These are not used directly in grapesy's public API.
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
  ) where

-- Borrow protolens 'StreamingType' (but this module is not Protobuf specific)
import Data.ProtoLens.Service.Types (StreamingType(..))

import Data.Kind
import Data.Proxy
import Network.GRPC.Common.NextElem (NextElem)
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

  The @StreamType@ API does not support metadata, and we use 'NextElem' rather
  than the more general 'StreamElem' to provide the user with a simpler
  interface. This means that we cannot mark the final streaming element as final
  as we send it, but must instead use 'NoMoreElems' (which will correspond to an
  additional empty HTTP data frame); similarly, on the input side we cannot
  detect that an element is final as we receive it. This does not matter in the
  vast majority of cases; for the rare case where it does matter, user code can
  simply use the core API instead of the streaming API.

  NOTE: it /is/ important that we mark the final message as final for
  /non-streaming/ cases; some servers get confused when this is not the case.
  However, this is still fine: in the non-streaming case we /can/ use
  'FinalElem' as this is something we take care of on behalf of the user.
-------------------------------------------------------------------------------}

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

data ServerHandler' (styp :: StreamingType) m (rpc :: k) where
  ServerHandler ::
       SupportsStreamingType rpc styp
    => Handler Server styp m rpc
    -> ServerHandler' styp m rpc

data ClientHandler' (s :: StreamingType) m (rpc :: k) where
  ClientHandler ::
       SupportsStreamingType rpc styp
    => Handler Client styp m rpc
    -> ClientHandler' styp m rpc

type ServerHandler m rpc = ServerHandler' (RpcStreamingType rpc) m rpc
type ClientHandler m rpc = ClientHandler' (RpcStreamingType rpc) m rpc

{-------------------------------------------------------------------------------
  Singleton
-------------------------------------------------------------------------------}

data SStreamingType :: StreamingType -> Type where
  SNonStreaming    :: SStreamingType NonStreaming
  SClientStreaming :: SStreamingType ClientStreaming
  SServerStreaming :: SStreamingType ServerStreaming
  SBiDiStreaming   :: SStreamingType BiDiStreaming

class ValidStreamingType (styp :: StreamingType) where
  -- | Obtain singleton
  validStreamingType :: Proxy styp -> SStreamingType styp

instance ValidStreamingType NonStreaming    where validStreamingType _ = SNonStreaming
instance ValidStreamingType ClientStreaming where validStreamingType _ = SClientStreaming
instance ValidStreamingType ServerStreaming where validStreamingType _ = SServerStreaming
instance ValidStreamingType BiDiStreaming   where validStreamingType _ = SBiDiStreaming