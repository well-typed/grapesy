module Main where

import System.IO

import Interop.Client (client)
import Interop.Client.Ping (ping)
import Interop.Cmdline
import Interop.SelfTest (selfTest)
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
      Server   -> server   cmdline
      Client   -> client   cmdline
      Ping     -> ping     cmdline
      SelfTest -> selfTest cmdline

