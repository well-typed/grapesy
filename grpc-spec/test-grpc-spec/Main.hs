{-# LANGUAGE CPP #-}

module Main (main) where

--import Control.Concurrent
--import Control.Exception
--import Data.Maybe (fromMaybe)
--import GHC.Conc (setUncaughtExceptionHandler)
--import System.IO
import Test.Tasty

import Test.Prop.IncrementalParsing           qualified as IncrementalParsing
import Test.Prop.Serialization                qualified as Serialization
--import Test.Sanity.BrokenDeployments          qualified as BrokenDeployments
--import Test.Sanity.Compression                qualified as Compression
--import Test.Sanity.Disconnect                 qualified as Disconnect
--import Test.Sanity.EndOfStream                qualified as EndOfStream
--import Test.Sanity.Exception                  qualified as Exception
--import Test.Sanity.Interop                    qualified as Interop
--import Test.Sanity.StreamingType.CustomFormat qualified as StreamingType.CustomFormat
--import Test.Sanity.StreamingType.NonStreaming qualified as StreamingType.NonStreaming

main :: IO ()
main = do
    defaultMain $ testGroup "test-grpc" [
        testGroup "Sanity" [
{-
            Disconnect.tests
          , EndOfStream.tests
          , testGroup "StreamingType" [
                StreamingType.NonStreaming.tests
              , StreamingType.CustomFormat.tests
              ]
          , Compression.tests
          , Exception.tests
          , Interop.tests
          , BrokenDeployments.tests
-}
          ]
      , testGroup "Prop" [
            IncrementalParsing.tests
          , Serialization.tests
          ]
      ]
