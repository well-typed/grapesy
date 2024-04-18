module KVStore.Server (withKeyValueServer) where

import Control.Concurrent (threadDelay)

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import KVStore.API
import KVStore.Cmdline
import KVStore.Util.Store (Store)
import KVStore.Util.Store qualified as Store

{-------------------------------------------------------------------------------
  Service definition
-------------------------------------------------------------------------------}

methodsKeyValueService ::
    Cmdline
  -> Store
  -> Methods IO (ProtobufMethodsOf KeyValueService)
methodsKeyValueService cmdline store =
      Method (mkNonStreaming $ create   cmdline store)
    $ Method (mkNonStreaming $ delete   cmdline store)
    $ Method (mkNonStreaming $ retrieve cmdline store)
    $ Method (mkNonStreaming $ update   cmdline store)
    $ NoMoreMethods

services ::
    Cmdline
  -> Store
  -> Services IO (ProtobufServices '[KeyValueService])
services cmdline store =
      Service (methodsKeyValueService cmdline store)
    $ NoMoreServices

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

create :: Cmdline -> Store -> CreateRequest -> IO CreateResponse
create cmdline store req = do
    simulateWork cmdline writeDelayMillis
    inserted <- Store.putIfAbsent store (req ^. #key) (req ^. #value)
    if inserted
      then return defMessage
      else throwGrpcError GrpcAlreadyExists

update :: Cmdline -> Store -> UpdateRequest -> IO UpdateResponse
update cmdline store req = do
    simulateWork cmdline writeDelayMillis
    replaced <- Store.replace store (req ^. #key) (req ^. #value)
    if replaced
      then return defMessage
      else throwGrpcError GrpcNotFound

retrieve :: Cmdline -> Store -> RetrieveRequest -> IO RetrieveResponse
retrieve cmdline store req = do
    simulateWork cmdline readDelayMillis
    mValue <- Store.get store (req ^. #key)
    case mValue of
      Just value -> return $ defMessage & #value .~ value
      Nothing    -> throwGrpcError GrpcNotFound

delete :: Cmdline -> Store -> DeleteRequest -> IO DeleteResponse
delete cmdline store req = do
    simulateWork cmdline writeDelayMillis
    Store.remove store (req ^. #key)
    return defMessage

{-------------------------------------------------------------------------------
  Delays
-------------------------------------------------------------------------------}

simulateWork :: Cmdline -> Int -> IO ()
simulateWork Cmdline{cmdSimulateWork} n
  | cmdSimulateWork = threadDelay (n * 1_000)
  | otherwise       = return ()

readDelayMillis, writeDelayMillis :: Int
readDelayMillis  = 10
writeDelayMillis = 50

{-------------------------------------------------------------------------------
  Server proper
-------------------------------------------------------------------------------}

withKeyValueServer :: Cmdline -> (RunningServer -> IO ()) -> IO ()
withKeyValueServer cmdline k = do
    store <- Store.new
    server <- mkGrpcServer params $ fromServices (services cmdline store)
    forkServer config server k
  where
    config :: ServerConfig
    config = ServerConfig {
          serverInsecure = Just $ InsecureConfig Nothing defaultInsecurePort
        , serverSecure   = Nothing
        }

    params :: ServerParams
    params = def {
          -- The Java benchmark does not use compression (unclear if the Java
          -- implementation supports compression at all; the compression Interop
          -- tests are also disabled for Java). For a fair comparison, we
          -- therefore disable compression here also.
          serverCompression = Compr.none
        }
