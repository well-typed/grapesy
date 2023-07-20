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
