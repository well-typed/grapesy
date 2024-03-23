module Main where

import System.IO

import Interop.Client (runInteropClient)
import Interop.Client.Ping (ping)
import Interop.Cmdline
import Interop.SelfTest (selfTest)
import Interop.Server (runInteropServer)

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
      Server   -> runInteropServer cmdline
      Client   -> runInteropClient cmdline
      Ping     -> ping cmdline
      SelfTest -> selfTest cmdline

