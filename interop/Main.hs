module Main where

import System.IO

import Interop.Client.Ping
import Interop.Cmdline
import Interop.Server (server)

{-------------------------------------------------------------------------------
  Top-level application driver
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    -- Ensure we see server output when running inside docker
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    cmdline <- getCmdline
    case cmdMode cmdline of
      Server -> server cmdline
      Client -> fail "client not yet implemented"
      Ping   -> ping cmdline
