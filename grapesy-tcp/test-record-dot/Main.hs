module Main (main) where

import Test.Tasty

import Test.OverloadedRecordDot    qualified as OverloadedRecordDot
import Test.OverloadedRecordUpdate qualified as OverloadedRecordUpdate

main :: IO ()
main = defaultMain $ testGroup "test-proto" [
      OverloadedRecordDot.tests
    , OverloadedRecordUpdate.tests
    ]
