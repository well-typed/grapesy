-- | Collection of handlers
--
-- This is not part of grapesy's public API.
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Server.HandlerMap (HandlerMap)
-- > import Network.GRPC.Server.HandlerMap qualified as HandlerMap
module Network.GRPC.Server.HandlerMap (
    -- * Definition
    HandlerMap -- opaque
    -- * Construction
  , fromList
    -- * Query
  , lookup
  , keys
  ) where

import Prelude hiding (lookup)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy

import Network.GRPC.Server.Call
import Network.GRPC.Spec
import Network.GRPC.Server.Handler

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype HandlerMap m = HandlerMap {
      getMap :: HashMap Path (RpcHandler m)
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromList :: [RpcHandler m] -> HandlerMap m
fromList = HandlerMap . HashMap.fromList . map (\h -> (path h, h))

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

lookup :: Path -> HandlerMap m -> Maybe (RpcHandler m)
lookup p = HashMap.lookup p . getMap

keys :: HandlerMap m -> [Path]
keys = HashMap.keys . getMap

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

path :: forall m. RpcHandler m -> Path
path RpcHandler{runRpcHandler} = aux runRpcHandler
  where
    aux :: forall rpc. IsRPC rpc => (Call rpc -> m ()) -> Path
    aux _ = rpcPath (Proxy @rpc)

