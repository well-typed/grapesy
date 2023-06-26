module Main (main) where

import Test.Tasty

import Test.Sanity.StreamingType.NonStreaming qualified as StreamingType.NonStreaming

main :: IO ()
main = defaultMain $ testGroup "grapesy" [
      testGroup "Sanity" [
          testGroup "StreamingType" [
              StreamingType.NonStreaming.tests
            ]
        ]
    ]
