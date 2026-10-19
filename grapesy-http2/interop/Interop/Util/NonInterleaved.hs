{-# LANGUAGE OverloadedStrings #-}

-- | Non-interleaved output
--
-- Intended for qualified import.
--
-- > import Interop.Util.NonInterleaved qualified as NI
module Interop.Util.NonInterleaved (
    putStrLn
  , hPutStrLn
  , putDocLn
  ) where

import Prelude hiding (putStrLn)
import Prelude qualified

import Control.Concurrent
import System.IO (Handle)
import System.IO qualified
import System.IO.Unsafe (unsafePerformIO)

import Interop.Util.ANSI qualified as ANSI

{-------------------------------------------------------------------------------
  Internal: output lock
-------------------------------------------------------------------------------}

outputLock :: MVar ()
{-# NOINLINE outputLock #-}
outputLock = unsafePerformIO $ newMVar ()

withOutputLock :: IO a -> IO a
withOutputLock k = withMVar outputLock $ \() -> k

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

putStrLn :: String -> IO ()
putStrLn str = withOutputLock $ Prelude.putStrLn str

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h str = withOutputLock $ System.IO.hPutStrLn h str

putDocLn :: ANSI.Doc -> IO ()
putDocLn doc = withOutputLock $ ANSI.putDocLn doc
