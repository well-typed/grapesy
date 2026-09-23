-- | Key-value client/server example
--
-- See <docs/kvstore.md> for details.
module Main (main) where

import Control.Concurrent

import Network.GRPC.Server.Run (waitServer)

import KVStore.Client
import KVStore.Cmdline
import KVStore.Server

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline <- getCmdline
    case cmdMode cmdline of
      Client ->
        runKeyValueClient cmdline
      Server ->
        withKeyValueServer cmdline $ waitServer
      Benchmark -> do
        withKeyValueServer cmdline $ \_server -> do
          -- Give the server a chance to start
          --
          -- (We don't want to enable automatic reconnects, as that might throw
          -- off the benchmarking.)
          threadDelay 1_000_000
          runKeyValueClient cmdline
