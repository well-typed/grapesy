module Main (main) where

import Control.Monad (forever)
import Network.Garp (run, lazyIOApplication, Connection (..))

main :: IO ()
main = run "9999" $ lazyIOApplication id
