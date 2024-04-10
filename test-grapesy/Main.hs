module Main (main) where

import Control.Concurrent
import Control.Exception
import GHC.Conc (setUncaughtExceptionHandler)
import System.IO
import Test.Tasty

import Test.Prop.Dialogue                     qualified as Dialogue
import Test.Prop.IncrementalParsing           qualified as IncrementalParsing
import Test.Prop.Serialization                qualified as Serialization
import Test.Sanity.Interop                    qualified as Interop
import Test.Sanity.StreamingType.CustomFormat qualified as StreamingType.CustomFormat
import Test.Sanity.StreamingType.NonStreaming qualified as StreamingType.NonStreaming

main :: IO ()
main = do
    setUncaughtExceptionHandler uncaughtExceptionHandler

    defaultMain $ testGroup "grapesy" [
        testGroup "Sanity" [
            testGroup "StreamingType" [
                StreamingType.NonStreaming.tests
              , StreamingType.CustomFormat.tests
              ]
          , Interop.tests
          ]
      , testGroup "Prop" [
            IncrementalParsing.tests
          , Serialization.tests
          , Dialogue.tests
          ]
      ]

uncaughtExceptionHandler :: SomeException -> IO ()
uncaughtExceptionHandler e = do
    tid <- myThreadId
    hPutStrLn stderr $ concat [
         "Uncaught exception in "
      , show tid
      , ": "
      , displayException e
      ]
