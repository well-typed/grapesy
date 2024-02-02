module Main (main) where

import Test.Tasty

import Test.Prop.Dialogue                     qualified as Dialogue
import Test.Prop.Serialization                qualified as Serialization
import Test.Sanity.HalfClosedLocal            qualified as HalfClosedLocal
import Test.Sanity.Interop                    qualified as Interop
import Test.Sanity.StreamingType.CustomFormat qualified as StreamingType.CustomFormat
import Test.Sanity.StreamingType.NonStreaming qualified as StreamingType.NonStreaming

main :: IO ()
main = defaultMain $ testGroup "grapesy" [
      testGroup "Sanity" [
          HalfClosedLocal.tests
        , testGroup "StreamingType" [
              StreamingType.NonStreaming.tests
            , StreamingType.CustomFormat.tests
            ]
        , Interop.tests
        ]
    , testGroup "Prop" [
          Serialization.tests
        , Dialogue.tests
        ]
    ]
