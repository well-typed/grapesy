{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import GHC.Conc (setUncaughtExceptionHandler)
import GHC.Conc.Sync (threadLabel)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
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
    mainThread <- myThreadId

    lookupEnv "GITHUB_ACTIONS" >>= \case
      Just "true" -> do
        putStrLn "Not running stress tests on GitHub Actions"
        exitSuccess
      _           -> return ()

#if defined(PROFILING) && MIN_VERSION_base(4,20,0)
    setBacktraceMechanismState CostCentreBacktrace True
#endif

    -- Parse command-line options
    cmdline@Cmdline{..} <- getCmdline
    say (optsTracing cmdGlobalOpts) $
      "parsed command-line options: " ++ show cmdline

    setUncaughtExceptionHandler $ handleUncaughtExceptions mainThread cmdline

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

handleUncaughtExceptions :: ThreadId -> Cmdline -> SomeException -> IO ()
handleUncaughtExceptions mainThread cmdline e = do
    myId    <- myThreadId
    myLabel <- threadLabel myId

    let isMainThread :: Bool
        isMainThread = myId == mainThread

        header :: String
        header =
            case (isMainThread, myLabel) of
              (True, _)        -> "main thread"
              (False, Just l)  -> "aux thread " ++ show myId ++ ": " ++ l
              (False, Nothing) -> "aux thread " ++ show myId

    fp <- writeSystemTempFile "test-stress" $ unlines [
        header
      , dumpStr cmdline
        -- TODO: for ghc >= 9.12, use displayExceptionWithInfo
      , displayException e
      ]
    putStrLn $ "Uncaught exception in " ++ header ++ ". See " ++ show fp

