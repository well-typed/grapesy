{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Exception
import GHC.Conc (setUncaughtExceptionHandler)
import System.IO.Temp (writeSystemTempFile)
import Text.Show.Pretty (dumpStr)

#if defined(PROFILING) && MIN_VERSION_base(4,20,0)
import Control.Exception.Backtrace
#endif

import Test.Stress.Client
import Test.Stress.Cmdline
import Test.Stress.Driver
import Test.Stress.Common (say)
import Test.Stress.Server

{-------------------------------------------------------------------------------
  Stress tests

  Unlike the regular test suite, we support running the client and the server in
  separate processes, so that we can run each with their own set of RTS flags.
-------------------------------------------------------------------------------}

main :: IO ()
main = do
#if defined(PROFILING) && MIN_VERSION_base(4,20,0)
    setBacktraceMechanismState CostCentreBacktrace True
#endif

    -- Parse command-line options
    cmdline@Cmdline{..} <- getCmdline
    say (optsTracing cmdGlobalOpts) $
      "parsed command-line options: " ++ show cmdline

    setUncaughtExceptionHandler $ handleUncaughtExceptions cmdline

    case cmdRole of
      Client{..} ->
        client
          (optsTracing cmdGlobalOpts)
          (unwrapNotPretty <$> clientSecurity)
          clientServerPort
          (unwrapNotPretty <$> clientCompression)
          clientConnects
      Server{..} ->
        server
          (optsTracing cmdGlobalOpts)
          serverConfig
      Driver{..} ->
        driver
          (optsTracing cmdGlobalOpts)
          driverGenCharts
          driverWorkingDir
          driverDuration

handleUncaughtExceptions :: Cmdline -> SomeException -> IO ()
handleUncaughtExceptions cmdline e = do
    fp <- writeSystemTempFile "test-stress" $ unlines [
        dumpStr cmdline
      , displayException e
      ]
    putStrLn $ "Abnormal termination. See " ++ show fp

