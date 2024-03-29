-- | RPC handlers
--
-- This is intended for qualified import, although a few functions are
-- re-exported as part of the public "Network.GRPC.Server" API and are named
-- accordingly.
--
-- This module is independent from HTTP2 specifics.
--
-- > import Network.GRPC.Server.Handler (RpcHandler(..))
-- > import Network.GRPC.Server.Handler qualified as Handler
module Network.GRPC.Server.Handler (
    RpcHandler(..)
    -- * Construction
  , mkRpcHandler
  , mkRpcHandlerNoInitialMetadata
    -- * Query
  , path
    -- * Collection of handlers
  , Map -- opaque
  , constructMap
  , lookup
  , keys
  ) where

import Prelude hiding (lookup)

import Control.Monad.IO.Class
import Data.Default
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy

import Network.GRPC.Server.Call
import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Handlers

  This is essentially an untyped interface; for a typed layer, see
  "Network.GRPC.Server.Protobuf".
-------------------------------------------------------------------------------}

-- | Handler for an RPC request
--
-- To construct an 'RpcHandler', you have two options:
--
-- * Use the \"raw\" API by calling 'mkRpcHandler'; this gives you full control
--   over the interaction with the client.
-- * Use the API from "Network.GRPC.Server.StreamType" to define handlers that
--   use the Protobuf stream types. This API is more convenient, and can be used
--   to guarantee at compile-time  that you have a handler for every method of
--   the services you support, but provides less flexibility (although it offers
--   an \"escape\" to the full API through
--   'Network.GRPC.Server.StreamType.RawMethod').
--
-- __Note on cancellation.__ The GRPC spec allows clients to \"cancel\" a
-- request (<https://grpc.io/docs/guides/cancellation/>). This does not
-- correspond to any specific message being sent across the network; instead,
-- the client simply disappears. The spec is quite clear that it is the
-- responsibility of the handler /itself/ to monitor for this. In @grapesy@ this
-- works as follows:
--
-- * Handlers are /not/ terminated when a client disappears. This allows the
--   handler to finish what it's doing, and terminate cleanly.
-- * When a handler tries to receive a message from the client ('recvInput'), or
--   send a message to the client ('sendOutput'), and the client disappeared,
--   this will result in a 'Network.GRPC.Server.ClientDisconnected' exception,
--   which the handler can catch and deal with.
--
-- Cancellation is always at the request of the /client/. If the /handler/
-- terminates early (that is, before sending the final output and trailers), a
-- 'Network.GRPC.Server.HandlerTerminated' exception will be raised and sent to
-- the client as 'GrpcException' with 'GrpcUnknown' error code.
data RpcHandler m = forall rpc. SupportsServerRpc rpc => RpcHandler {
      -- | Handler proper
      runRpcHandler :: Call rpc -> m ()
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Constructor for 'RpcHandler'
--
-- When the handler sends its first message to the client, @grapesy@ must first
-- send the initial metadata (of type @ResponseInitialMetadata@) to the client.
-- This metadata can be updated at any point before that first message (for
-- example, after receiving some messages from the client) by calling
-- 'setResponseInitialMetadata'. If this function is never called, however, then
-- we need a default value; 'mkRpcHandler' therefore calls
-- 'setResponseInitialMetadata' once before the handler proper, relying on the
-- 'Default' instance.
--
-- For RPCs where a sensible default does not exist (perhaps the initial
-- response metadata needs the request metadata from the client, or even some
-- messages from the client), you can use 'mkRpcHandlerNoInitialMetadata'.
mkRpcHandler ::
     ( SupportsServerRpc rpc
     , Default (ResponseInitialMetadata rpc)
     , MonadIO m
     )
  => Proxy rpc -> (Call rpc -> m ()) -> RpcHandler m
mkRpcHandler _ k = RpcHandler $ \call -> do
    liftIO $ setResponseInitialMetadata call def
    k call

-- | Variant on 'mkRpcHandler' that does not call 'setResponseInitialMetadata'
--
-- You /must/ call 'setResponseInitialMetadata' before sending the first
-- message. See 'mkRpcHandler' for additional discussion.
mkRpcHandlerNoInitialMetadata ::
     SupportsServerRpc rpc
  => Proxy rpc
  -> (Call rpc -> m ())
  -> RpcHandler m
mkRpcHandlerNoInitialMetadata _ k = RpcHandler $ \call -> do
    k call

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

path :: forall m. RpcHandler m -> Path
path RpcHandler{runRpcHandler} = aux runRpcHandler
  where
    aux :: forall rpc. IsRPC rpc => (Call rpc -> m ()) -> Path
    aux _ = rpcPath (Proxy @rpc)

{-------------------------------------------------------------------------------
  Collection of handlers
-------------------------------------------------------------------------------}

newtype Map m = Map {
      getMap :: HashMap Path (RpcHandler m)
    }

constructMap :: [RpcHandler m] -> Map m
constructMap = Map . HashMap.fromList . map (\h -> (path h, h))

lookup :: Path -> Map m -> Maybe (RpcHandler m)
lookup p = HashMap.lookup p . getMap

keys :: Map m -> [Path]
keys = HashMap.keys . getMap