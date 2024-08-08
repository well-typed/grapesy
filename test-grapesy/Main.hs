{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Data.Maybe (fromMaybe)
import GHC.Conc (setUncaughtExceptionHandler)
import System.IO
import Test.Tasty

#if MIN_VERSION_base(4,18,0)
import GHC.Conc.Sync (threadLabel)
#endif

import Test.Prop.Dialogue                     qualified as Dialogue
import Test.Prop.IncrementalParsing           qualified as IncrementalParsing
import Test.Prop.Serialization                qualified as Serialization
import Test.Sanity.BrokenDeployments          qualified as BrokenDeployments
import Test.Sanity.Disconnect                 qualified as Disconnect
import Test.Sanity.EndOfStream                qualified as EndOfStream
import Test.Sanity.Exception                  qualified as Exception
import Test.Sanity.Interop                    qualified as Interop
import Test.Sanity.StreamingType.CustomFormat qualified as StreamingType.CustomFormat
import Test.Sanity.StreamingType.NonStreaming qualified as StreamingType.NonStreaming

main :: IO ()
main = do
    setUncaughtExceptionHandler uncaughtExceptionHandler

    defaultMain $ testGroup "grapesy" [
        testGroup "Sanity" [
            Disconnect.tests
          , EndOfStream.tests
          , testGroup "StreamingType" [
                StreamingType.NonStreaming.tests
              , StreamingType.CustomFormat.tests
              ]
          , Exception.tests
          , Interop.tests
          , BrokenDeployments.tests
          ]
      , testGroup "Prop" [
            IncrementalParsing.tests
          , Serialization.tests
          , Dialogue.tests
          ]
      ]

uncaughtExceptionHandler :: SomeException -> IO ()
uncaughtExceptionHandler e = do
    tid    <- myThreadId
    mLabel :: Maybe String <-
#if MIN_VERSION_base(4,18,0)
      threadLabel tid
#else
      return $ Just "unknown label"
#endif
    hPutStrLn stderr $ concat [
         "Uncaught exception in "
      , show tid
      , " ("
      , fromMaybe "unlabelled" mLabel
      , "): "
      , displayException e
      ]
