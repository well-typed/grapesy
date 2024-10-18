module Main (main) where

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
    -- Parse command-line options
    cmdline@Cmdline{..} <- getCmdline
    say (optsTracing cmdGlobalOpts) $
      "parsed command-line options: " ++ show cmdline

    case cmdRole of
      Client{..} ->
        client
          (optsTracing cmdGlobalOpts)
          clientSecurity
          clientServerPort
          clientCompression
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
