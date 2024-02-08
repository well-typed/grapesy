module Main (main) where

import Control.Exception
import Control.Monad
import Data.Proxy

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Server.Binary qualified as Binary
import Network.GRPC.Server.StreamType

import Test.Stress.Cmdline
import Test.Util.ClientServer
import Control.Tracer

{-------------------------------------------------------------------------------
  Barebones stress test

  Unlike the regular test suite, here we run the client and the server in
  separate processes, so that we can run each with their own set of RTS flags.

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
    runTestClient def nullTracer $ \withConn -> withConn $ \conn ->
      case test of
        ManyShortLived ->
          clientManyShortLived conn

{-------------------------------------------------------------------------------
  Server
-------------------------------------------------------------------------------}

runServer :: IO ()
runServer = do
    serverHandlerLock <- newServerHandlerLock
    runTestServer
      def
      nullTracer
      serverHandlerLock
      [
        streamingRpcHandler (Proxy @ManyShortLived) $
          Binary.mkNonStreaming serverManyShortLived
      ]

{-------------------------------------------------------------------------------
  Test: manyshortlived
-------------------------------------------------------------------------------}

clientManyShortLived :: Client.Connection -> IO ()
clientManyShortLived conn =
    forM_ [1 .. 5] $ \(i :: Word) -> do
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
