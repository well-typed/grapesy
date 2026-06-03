module Main (main) where

import GHC.Conc (setUncaughtExceptionHandler)
import System.IO

import Interop.Client (runInteropClient)
import Interop.Client.Ping (ping)
import Interop.Cmdline
import Interop.SelfTest (selfTest)
import Interop.Server (runInteropServer)
import Interop.Util.Exceptions

{-------------------------------------------------------------------------------
  Top-level application driver
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    setUncaughtExceptionHandler uncaughtExceptionHandler

    -- Ensure we see server output when running inside docker
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    cmdline <- getCmdline
    case cmdMode cmdline of
      Server   -> runInteropServer cmdline
      Client   -> runInteropClient cmdline
      Ping     -> ping cmdline
      SelfTest -> selfTest cmdline
