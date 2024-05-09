module Main (main) where

import Test.Stress.Client
import Test.Stress.Cmdline
import Test.Stress.Server

main :: IO ()
main = do
    cmdline <- getCmdline
    case cmdRole cmdline of
      Client -> client
      Server -> server

