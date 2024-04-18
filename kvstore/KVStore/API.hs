{-# OPTIONS_GHC -Wno-orphans #-}

module KVStore.API (
    -- * API
    Create
  , Delete
  , Retrieve
  , Update

    -- * Re-exports
  , module Proto.Kvstore
  ) where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Kvstore

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

type Create   = Protobuf KeyValueService "create"
type Delete   = Protobuf KeyValueService "delete"
type Retrieve = Protobuf KeyValueService "retrieve"
type Update   = Protobuf KeyValueService "update"

{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

type instance RequestMetadata          (Protobuf KeyValueService meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf KeyValueService meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf KeyValueService meth) = NoMetadata

