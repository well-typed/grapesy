{-# LANGUAGE OverloadedStrings #-}

module Interop.Client (runInteropClient) where

import Control.Exception
import Control.Monad
import Data.IORef
import System.Exit
import System.Timeout

import Interop.Cmdline
import Interop.Util.ANSI
import Interop.Util.Exceptions

import Interop.Client.TestCase.EmptyUnary                qualified as EmptyUnary
import Interop.Client.TestCase.LargeUnary                qualified as LargeUnary
import Interop.Client.TestCase.ClientCompressedUnary     qualified as ClientCompressedUnary
import Interop.Client.TestCase.ServerCompressedUnary     qualified as ServerCompressedUnary
import Interop.Client.TestCase.ClientStreaming           qualified as ClientStreaming
import Interop.Client.TestCase.ClientCompressedStreaming qualified as ClientCompressedStreaming
import Interop.Client.TestCase.ServerStreaming           qualified as ServerStreaming
import Interop.Client.TestCase.ServerCompressedStreaming qualified as ServerCompressedStreaming
import Interop.Client.TestCase.PingPong                  qualified as PingPong
import Interop.Client.TestCase.EmptyStream               qualified as EmptyStream
import Interop.Client.TestCase.CustomMetadata            qualified as CustomMetadata
import Interop.Client.TestCase.StatusCodeAndMessage      qualified as StatusCodeAndMessage
import Interop.Client.TestCase.SpecialStatusMessage      qualified as SpecialStatusMessage
import Interop.Client.TestCase.UnimplementedMethod       qualified as UnimplementedMethod
import Interop.Client.TestCase.UnimplementedService      qualified as UnimplementedService
import Interop.Client.TestCase.CancelAfterBegin          qualified as CancelAfterBegin
import Interop.Client.TestCase.CancelAfterFirstResponse  qualified as CancelAfterFirstResponse
import Interop.Client.TestCase.TimeoutOnSleepingServer   qualified as TimeoutOnSleepingServer

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

runInteropClient :: Cmdline -> IO ()
runInteropClient cmdline = do
    stats <- newIORef initTestStats

    case cmdTestCase cmdline of
      Nothing ->
        forM_ [minBound .. maxBound] $ testCase cmdline stats
      Just test ->
        testCase cmdline stats test

    ranAllTests stats

{-------------------------------------------------------------------------------
  Running individual tests
-------------------------------------------------------------------------------}

runTest :: TestCase -> Cmdline -> IO ()
runTest EmptyUnary                = EmptyUnary.runTest
runTest LargeUnary                = LargeUnary.runTest
runTest ClientCompressedUnary     = ClientCompressedUnary.runTest
runTest ServerCompressedUnary     = ServerCompressedUnary.runTest
runTest ClientStreaming           = ClientStreaming.runTest
runTest ClientCompressedStreaming = ClientCompressedStreaming.runTest
runTest ServerStreaming           = ServerStreaming.runTest
runTest ServerCompressedStreaming = ServerCompressedStreaming.runTest
runTest PingPong                  = PingPong.runTest
runTest EmptyStream               = EmptyStream.runTest
runTest CustomMetadata            = CustomMetadata.runTest
runTest StatusCodeAndMessage      = StatusCodeAndMessage.runTest
runTest SpecialStatusMessage      = SpecialStatusMessage.runTest
runTest UnimplementedMethod       = UnimplementedMethod.runTest
runTest UnimplementedService      = UnimplementedService.runTest
runTest CancelAfterBegin          = CancelAfterBegin.runTest
runTest CancelAfterFirstResponse  = CancelAfterFirstResponse.runTest
runTest TimeoutOnSleepingServer   = TimeoutOnSleepingServer.runTest

skips :: Cmdline -> TestCase -> Bool
skips cmdline test = or [
      elem test (cmdSkipTest cmdline)
    , cmdSkipCompression cmdline && elem test [
          ClientCompressedUnary
        , ServerCompressedUnary
        , ClientCompressedStreaming
        , ServerCompressedStreaming
        ]
    , cmdSkipClientCompression cmdline && elem test [
          ClientCompressedUnary
        , ClientCompressedStreaming
        ]
    ]

testCase :: Cmdline -> IORef TestStats -> TestCase -> IO ()
testCase cmdline stats test = unless (skips cmdline test) $ do
    result <- try $ timeout (cmdTimeoutTest cmdline * 1_000_000) $
                runTest test cmdline
    case result of
      Right (Just ()) ->
        testOK stats test
      Right Nothing ->
        testFailed stats test "timeout"
      Left err | Just (TestSkipped reason) <- fromException err ->
        testSkipped stats test reason
      Left err ->
        testFailed stats test (displayException err)

{-------------------------------------------------------------------------------
  Test stats
-------------------------------------------------------------------------------}

data TestStats = TestStats {
      numSucceeded :: Int
    , numFailed    :: Int
    , numSkipped   :: Int
    }

initTestStats :: TestStats
initTestStats = TestStats {
      numSucceeded = 0
    , numFailed    = 0
    , numSkipped   = 0
    }

testOK :: IORef TestStats -> TestCase -> IO ()
testOK statsRef test = do
    putDocLn $ mconcat [
        Show test
      , ": "
      , Color Green "OK"
      ]
    modifyIORef statsRef $ \stats ->
      stats { numSucceeded = numSucceeded stats + 1 }

testFailed :: IORef TestStats -> TestCase -> String -> IO ()
testFailed statsRef test err = do
    putDocLn $ mconcat [
        Show test
      , ": "
      , Color Red "Failed: "
      , fromString err
      ]
    modifyIORef statsRef $ \stats ->
      stats { numFailed = numFailed stats + 1 }

testSkipped :: IORef TestStats -> TestCase -> String -> IO ()
testSkipped statsRef test reason = do
    putDocLn $ mconcat [
        Show test
      , ": "
      , Color Yellow "Skipped: "
      , fromString reason
      ]
    modifyIORef statsRef $ \stats ->
      stats { numSkipped = numSkipped stats + 1 }

ranAllTests :: IORef TestStats -> IO ()
ranAllTests statsRef = do
    stats <- readIORef statsRef

    when (numSucceeded stats + numFailed stats > 1 || numSkipped stats > 0) $
      putStrLn $ concat [
          show $ numSucceeded stats
        , " succeeded, "
        , show $ numFailed stats
        , " failed, "
        , show $ numSkipped stats
        , " skipped."
        ]

    when (numFailed stats > 0) $
      exitFailure
