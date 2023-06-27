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
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.PseudoHeaders
import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  Handlers

  This is essentially an untyped interface; for a typed layer, see
  "Network.GRPC.Server.Protobuf".
-------------------------------------------------------------------------------}

data RpcHandler m = forall rpc. IsRPC rpc => RpcHandler {
      -- | Response metadata
      --
      -- Since this metadata must be returned in the initial headers (before
      -- communication with the client), the only context that is available is
      -- the metadata that the client included in their request (and,
      -- implicitly, the pseudo-headers, as they determine the handler).
      --
      -- The handler can return additional metadata in the trailers at the /end/
      -- of the communication; see 'sendOutput'.
      handlerMetadata :: [CustomMetadata] -> m [CustomMetadata]

      -- | Handler proper
    , handlerRun :: Call rpc -> m ()
    }

instance Show (RpcHandler m) where
  show RpcHandler{handlerRun} = aux handlerRun
    where
      aux :: forall rpc. IsRPC rpc => (Call rpc -> m ()) -> String
      aux _ = "<RpcHandler " ++ show (typeRep (Proxy @rpc)) ++ ">"

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Default RPC handler
mkRpcHandler ::
     (Monad m, IsRPC rpc)
  => Proxy rpc -> (Call rpc -> m ()) -> RpcHandler m
mkRpcHandler _ handlerRun = RpcHandler{
      handlerRun
    , handlerMetadata = \_ -> return []
    }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

path :: forall m. RpcHandler m -> Path
path RpcHandler{handlerRun} = aux handlerRun
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