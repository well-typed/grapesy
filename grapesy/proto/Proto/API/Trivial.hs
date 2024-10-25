{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API.Trivial
  ( -- * Trivial RPC
    Trivial
  , Trivial'
  ) where

import Network.GRPC.Common
import Network.GRPC.Common.Binary

{-------------------------------------------------------------------------------
  Trivial RPC
-------------------------------------------------------------------------------}

type Trivial    = RawRpc "trivial" "trivial"
type Trivial' s = RawRpc "trivial" s

type instance RequestMetadata          (Trivial' s) = NoMetadata
type instance ResponseInitialMetadata  (Trivial' s) = NoMetadata
type instance ResponseTrailingMetadata (Trivial' s) = NoMetadata
