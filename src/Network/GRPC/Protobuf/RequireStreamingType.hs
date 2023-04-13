-- This module exists only to expose a /single/ function for which we disable
-- this warning:
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Network.GRPC.Protobuf.RequireStreamingType (
    requireStreamingType
  ) where

import Data.Proxy
import Data.ProtoLens.Service.Types

import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Protobuf

-- | Add type-level constraint on the 'MethodStreamingType'
--
-- This function can be used in other modules to add a 'MethodStreamingType'
-- constraint without causing ghc to warn about redundant constraints.
requireStreamingType ::
     ( IsRPC (RPC s m)
     , MethodStreamingType s m ~ typ
     )
  => RPC s m -> Proxy (typ :: StreamingType) -> a -> a
requireStreamingType _ _ = id