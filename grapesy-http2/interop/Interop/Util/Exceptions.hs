{-# LANGUAGE CPP #-}

module Interop.Util.Exceptions (
    TestSkipped(..)
  , TestUnimplemented(..)
    -- * Test failures
  , TestFailure(..)
  , assertFailure
  , assertUnrecognized
  , assertBool
  , assertEqual
  , assertThrows
  , assertTerminatesWithinSeconds
    -- * Uncaught exception handler
  , uncaughtExceptionHandler
    -- * Re-exports
  , HasCallStack
  , throwIO
  ) where

import Control.Concurrent
import Control.Exception (Exception(..))
import Control.Exception qualified as E
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import GHC.Stack
import Network.GRPC.Common.Exception
import System.IO
import System.Timeout

#if MIN_VERSION_base(4,18,0)
import GHC.Conc.Sync (threadLabel)
#endif

import Interop.Util.NonInterleaved qualified as NI

{-------------------------------------------------------------------------------
  Exceptions thrown by the tests
-------------------------------------------------------------------------------}

data TestSkipped = TestSkipped String
  deriving stock (Show)
  deriving anyclass (Exception)

data TestUnimplemented = TestUnimplemented
  deriving stock (Show)

instance Exception TestUnimplemented where
  displayException TestUnimplemented = "test unimplemented"

{-------------------------------------------------------------------------------
  Test failures

  The output of the test messages is relatively minimal: we show only a
  callstack, so that we know /which/ test failed, and any runtime values.
-------------------------------------------------------------------------------}

data TestFailure =
     TestFailure {
         failureCallStack :: CallStack
       , failureMessage   :: String
       }
  deriving stock (Show)

instance Exception TestFailure where
  displayException (TestFailure cs msg) = intercalate "\n" [
      msg
    , prettyCallStack cs
    ]

assertFailure :: HasCallStack => String -> IO x
assertFailure = throwIO . TestFailure callStack

assertUnrecognized :: (HasCallStack, Show a) => a -> IO x
assertUnrecognized x = assertFailure $ "Unrecognized: " ++ show x

assertBool :: HasCallStack => Bool -> IO ()
assertBool True  = return ()
assertBool False = assertFailure "Predicate failed"

assertEqual :: (HasCallStack, Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual
  | expected == actual
  = return ()

  | otherwise
  = assertFailure $ "Expected: " ++ show expected ++ ", actual: " ++ show actual

assertThrows :: (HasCallStack, Exception e) => (e -> IO ()) -> IO a -> IO ()
assertThrows p io = do
    ma <- E.try io
    case ma of
      Right _  -> assertFailure "Expected exception"
      Left err -> p err

assertTerminatesWithinSeconds :: Int -> IO () -> IO ()
assertTerminatesWithinSeconds s io = do
    result <- timeout (s * 1_000_000) io
    case result of
      Nothing -> assertFailure "Timeout"
      Just () -> return ()


{-------------------------------------------------------------------------------
  Uncaught exception handler
-------------------------------------------------------------------------------}

uncaughtExceptionHandler :: E.SomeException -> IO ()
uncaughtExceptionHandler e = do
    tid    <- myThreadId
    mLabel :: Maybe String <-
#if MIN_VERSION_base(4,18,0)
      threadLabel tid
#else
      return $ Just "unknown label"
#endif
    NI.hPutStrLn stderr $ concat [
         "Uncaught exception in "
      , show tid
      , " ("
      , fromMaybe "unlabelled" mLabel
      , "): "
      , renderAnyException defaultFormatCtx e
      ]
