module Main (main) where

import Control.Exception
import Control.Monad
import Control.Tracer
import Data.Default
import Data.Proxy

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common.Binary
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Binary
import Network.GRPC.Server.Run qualified as Server
import Network.GRPC.Server.StreamType

import Test.Stress.Cmdline

{-------------------------------------------------------------------------------
  Barebones stress test

  TODOs:

  - Test with TLS
  - Test with various kinds of compression
  - Test with Protobuf
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline <- getCmdline
    case cmdRole cmdline of
      Client test -> runClient test
      Server      -> runServer

{-------------------------------------------------------------------------------
  Client
-------------------------------------------------------------------------------}

runClient :: Test -> IO ()
runClient test =
    Client.withConnection params server $ \conn -> do
      case test of
        ManyShortLived ->
          clientManyShortLived conn
  where
    params :: Client.ConnParams
    params = Client.ConnParams {
          connTracer         = nullTracer
        , connCompression    = Compression.none
        , connDefaultTimeout = Nothing
        }

    server :: Client.Server
    server = Client.ServerInsecure $ Client.Authority {
          authorityHost = "localhost"
        , authorityPort = 50051
        }

{-------------------------------------------------------------------------------
  Server
-------------------------------------------------------------------------------}

runServer :: IO ()
runServer =
    Server.withServer params handlers $ Server.runServer config
  where
    params :: Server.ServerParams
    params = Server.ServerParams {
          serverTracer      = nullTracer
        , serverCompression = Compression.none
        }

    config :: Server.ServerConfig
    config = Server.ServerConfig {
          serverTracer   = nullTracer
        , serverInsecure = Just Server.InsecureConfig {
              insecureHost = Nothing
            , insecurePort = "50051"
            }
        , serverSecure   = Nothing
        }

    handlers :: [Server.RpcHandler IO]
    handlers = [
          streamingRpcHandler (Proxy @ManyShortLived) $
            Binary.mkNonStreaming serverManyShortLived
       ]

{-------------------------------------------------------------------------------
  Test: manyshortlived
-------------------------------------------------------------------------------}

clientManyShortLived :: Client.Connection -> IO ()
clientManyShortLived conn =
    forM_ [1 .. 1_000] $ \(i :: Word) -> do
      Client.withRPC conn def (Proxy @ManyShortLived) $ \call -> do
        Binary.sendFinalInput call i
        n <- fst <$> Binary.recvFinalOutput call
        unless (n == succ i) $
          throwIO . userError $ concat [
              "Unexpected " ++ show n ++ "; "
            , "expected " ++ show (succ i)
            ]

serverManyShortLived :: Word -> IO Word
serverManyShortLived = return . succ

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

type ManyShortLived = BinaryRpc "stresstest" "manyshortlived"
