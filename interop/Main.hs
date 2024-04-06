module Main where

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
    -- TODO: <https://github.com/well-typed/grapesy/issues/123>
    --
    -- Under some circumstances we can see a "Thread killed" exception when a
    -- test fails. By setting this handler, we can at least confirm that this
    -- exception is coming from an uncaught exception in a thread.
    setUncaughtExceptionHandler $ \err ->
      hPutStrLn stderr $ "Uncaught exception: " ++ show err

    -- Ensure we see server output when running inside docker
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    cmdline <- getCmdline
    case cmdMode cmdline of
      Server   -> runInteropServer cmdline
      Client   -> runInteropClient cmdline
      Ping     -> ping cmdline
      SelfTest -> selfTest cmdline
