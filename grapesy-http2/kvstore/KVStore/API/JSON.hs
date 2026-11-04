{-# OPTIONS_GHC -Wno-orphans #-}

module KVStore.API.JSON (client, server) where

import Network.GRPC.Client (rpc)
import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.StreamType.IO qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.JSON
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.StreamType qualified as Server

import KVStore.API
import KVStore.Util.Profiling

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

type KeyValueService = "io.grpc.KeyValueService"

type Create   = JsonRpc KeyValueService "Create"
type Delete   = JsonRpc KeyValueService "Delete"
type Retrieve = JsonRpc KeyValueService "Retrieve"
type Update   = JsonRpc KeyValueService "Update"

type instance Input  Create = JsonObject '[ '("key", Required Key), '("value", Required Value) ]
type instance Output Create = JsonObject '[ ]

type instance Input  Retrieve = JsonObject '[ '("key", Required Key) ]
type instance Output Retrieve = JsonObject '[ '("value", Required Value) ]

type instance Input  Update = JsonObject '[ '("key", Required Key), '("value", Required Value) ]
type instance Output Update = JsonObject '[ ]

type instance Input  Delete = JsonObject '[ '("key", Required Key) ]
type instance Output Delete = JsonObject '[ ]

{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

type instance RequestMetadata          (JsonRpc KeyValueService meth) = NoMetadata
type instance ResponseInitialMetadata  (JsonRpc KeyValueService meth) = NoMetadata
type instance ResponseTrailingMetadata (JsonRpc KeyValueService meth) = NoMetadata

{-------------------------------------------------------------------------------
  Client
-------------------------------------------------------------------------------}

client :: Client.Connection -> KVStore
client conn = KVStore {
      create   = fmap to0 . Client.nonStreaming conn (rpc @Create)   . from2
    , delete   = fmap to0 . Client.nonStreaming conn (rpc @Delete)   . from1
    , retrieve = fmap to1 . Client.nonStreaming conn (rpc @Retrieve) . from1
    , update   = fmap to0 . Client.nonStreaming conn (rpc @Update)   . from2
    }

{-------------------------------------------------------------------------------
  Server

  Unlike in the Protobuf case, we don't have a type-level description of the
  full API here.
-------------------------------------------------------------------------------}

server :: KVStore -> [Server.SomeRpcHandler IO]
server kvstore = [
      Server.fromMethod @Create   $ markNonStreaming "create"   $ fmap from0 . create   kvstore . to2
    , Server.fromMethod @Delete   $ markNonStreaming "delete"   $ fmap from0 . delete   kvstore . to1
    , Server.fromMethod @Retrieve $ markNonStreaming "retrieve" $ fmap from1 . retrieve kvstore . to1
    , Server.fromMethod @Update   $ markNonStreaming "update"   $ fmap from0 . update   kvstore . to2
    ]

{-------------------------------------------------------------------------------
  Marshalling
-------------------------------------------------------------------------------}

to0 :: JsonObject '[] -> ()
to0 JsonObject = ()

from0 :: () -> JsonObject '[]
from0 () = JsonObject

to1 :: JsonObject '[ '(a, Required x) ] -> x
to1 (Required x :* JsonObject) = x

from1 :: x -> JsonObject '[ '(a, Required x) ]
from1 x = Required x :* JsonObject

to2 :: JsonObject '[ '(a, Required x), '(b, Required y) ] -> (x, y)
to2 (Required key :* Required value :* JsonObject) = (key, value)

from2 :: (x, y) -> JsonObject '[ '(a, Required x), '(b, Required y) ]
from2 (key, value) = Required key :* Required value :* JsonObject
