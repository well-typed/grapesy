{-# OPTIONS_GHC -Wno-orphans #-}

module KVStore.API.Protobuf (client, server) where

import Data.ProtoLens.Labels ()

import Network.GRPC.Client (rpc)
import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.StreamType.IO qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.StreamType (Services(..), Methods(..))
import Network.GRPC.Server.StreamType qualified as Server
import Network.GRPC.Server.Protobuf qualified as Server

import Proto.Kvstore

import KVStore.API
import KVStore.Util.Profiling

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

{-------------------------------------------------------------------------------
  Client
-------------------------------------------------------------------------------}

client :: Client.Connection -> KVStore
client conn = KVStore {
      create   = fmap fromCreateResponse   . Client.nonStreaming conn (rpc @Create)   . createRequest
    , retrieve = fmap fromRetrieveResponse . Client.nonStreaming conn (rpc @Retrieve) . retrieveRequest
    , update   = fmap fromUpdateResponse   . Client.nonStreaming conn (rpc @Update)   . updateRequest
    , delete   = fmap fromDeleteResponse   . Client.nonStreaming conn (rpc @Delete)   . deleteRequest
    }

{-------------------------------------------------------------------------------
  Server
-------------------------------------------------------------------------------}

server :: KVStore -> [Server.SomeRpcHandler IO]
server kvstore =
    Server.fromServices $ Service methods NoMoreServices
  where
   methods :: Methods IO (Server.ProtobufMethodsOf KeyValueService)
   methods =
         Method (markNonStreaming "create"   $ fmap createResponse   . create   kvstore . fromCreateRequest)
       $ Method (markNonStreaming "delete"   $ fmap deleteResponse   . delete   kvstore . fromDeleteRequest)
       $ Method (markNonStreaming "retrieve" $ fmap retrieveResponse . retrieve kvstore . fromRetrieveRequest)
       $ Method (markNonStreaming "update"   $ fmap updateResponse   . update   kvstore . fromUpdateRequest)
       $ NoMoreMethods

{-------------------------------------------------------------------------------
  Marshalling
-------------------------------------------------------------------------------}

createRequest :: (Key, Value) -> Input Create
createRequest (Key key, Value value)  =
    defMessage
      & #key   .~ key
      & #value .~ value

fromCreateRequest :: Input Create -> (Key, Value)
fromCreateRequest req = (
      Key   $ req ^. #key
    , Value $ req ^. #value
    )

createResponse :: () -> Output Create
createResponse _ = defMessage

fromCreateResponse :: Output Create -> ()
fromCreateResponse _ = ()

retrieveRequest :: Key -> Input Retrieve
retrieveRequest (Key key) = defMessage & #key .~ key

fromRetrieveRequest :: Input Retrieve -> Key
fromRetrieveRequest req = Key $ req ^. #key

retrieveResponse :: Value -> Output Retrieve
retrieveResponse (Value value) = defMessage & #value .~ value

fromRetrieveResponse :: Output Retrieve -> Value
fromRetrieveResponse resp = Value $ resp ^. #value

updateRequest :: (Key, Value) -> Input Update
updateRequest (Key key, Value value) =
    defMessage
      & #key   .~ key
      & #value .~ value

fromUpdateRequest :: Input Update -> (Key, Value)
fromUpdateRequest req = (
      Key   $ req ^. #key
    , Value $ req ^. #value
    )

updateResponse :: () -> Output Update
updateResponse _ = defMessage

fromUpdateResponse :: Output Update -> ()
fromUpdateResponse _ = ()

deleteRequest :: Key -> Input Delete
deleteRequest (Key key) = defMessage & #key .~ key

fromDeleteRequest :: Input Delete -> Key
fromDeleteRequest req = Key $ req ^. #key

deleteResponse :: () -> Output Delete
deleteResponse _ = defMessage

fromDeleteResponse :: Output Delete -> ()
fromDeleteResponse _ = ()
