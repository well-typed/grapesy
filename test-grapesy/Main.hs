module Main (main) where

import Test.Tasty

import Test.Prop.Dialogue                     qualified as Dialogue
import Test.Prop.Serialization                qualified as Serialization
import Test.Sanity.StreamingType.NonStreaming qualified as StreamingType.NonStreaming
import Test.Sanity.HalfClosedLocal            qualified as HalfClosedLocal

main :: IO ()
main = defaultMain $ testGroup "grapesy" [
      testGroup "Sanity" [
          HalfClosedLocal.tests
        , testGroup "StreamingType" [
              StreamingType.NonStreaming.tests
            ]
        ]
    , testGroup "Prop" [
          Serialization.tests
        , Dialogue.tests
        ]
    ]
