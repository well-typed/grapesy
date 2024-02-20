module Main (main) where

import Test.Stress.Client
import Test.Stress.Cmdline
import Test.Stress.Server

{-------------------------------------------------------------------------------
  Barebones stress test

  Unlike the regular test suite, here we run the client and the server in
  separate processes, so that we can run each with their own set of RTS flags.
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline <- getCmdline
    case cmdRole cmdline of
      Client test -> client cmdline test
      Server      -> server cmdline

