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
    -- * Query
  , path
    -- * Collection of handlers
  , Map -- opaque
  , constructMap
  , lookup
  , keys
  ) where

import Prelude hiding (lookup)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy
import Data.Typeable

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
-- TODO: What if the /handler/ wants to terminate?
data RpcHandler m = forall rpc. IsRPC rpc => RpcHandler {
      -- | Handler proper
      runRpcHandler :: Call rpc -> m ()
    }

instance Show (RpcHandler m) where
  show RpcHandler{runRpcHandler} = aux runRpcHandler
    where
      aux :: forall rpc. IsRPC rpc => (Call rpc -> m ()) -> String
      aux _ = "<RpcHandler " ++ show (typeRep (Proxy @rpc)) ++ ">"

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Constructor for 'RpcHandler'
mkRpcHandler :: IsRPC rpc => Proxy rpc -> (Call rpc -> m ()) -> RpcHandler m
mkRpcHandler _ = RpcHandler

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