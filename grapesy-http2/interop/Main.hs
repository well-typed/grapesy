module Main (main) where

import GHC.Conc (setUncaughtExceptionHandler)
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
    setUncaughtExceptionHandler $ \err -> do
      hPutStrLn stderr $ "Uncaught exception: " ++ show err
      hFlush stderr

    -- Ensure we see server output when running inside docker
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    cmdline <- getCmdline
    case cmdMode cmdline of
      Server   -> runInteropServer cmdline
      Client   -> runInteropClient cmdline
      Ping     -> ping cmdline
      SelfTest -> selfTest cmdline
