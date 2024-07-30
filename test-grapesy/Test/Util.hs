{-# LANGUAGE CPP #-}

module Test.Util (
    -- * Timeouts
    Timeout(..)
  , within

    -- * Files
  , withTemporaryFile
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import GHC.Stack
import System.Directory
import System.IO

{-------------------------------------------------------------------------------
  Timeouts
-------------------------------------------------------------------------------}

data Timeout = forall a. Show a => Timeout a CallStack
  deriving anyclass (Exception)

deriving stock instance Show Timeout

-- | Limit execution time of an action
within :: forall m a info.
     (HasCallStack, Show info, MonadIO m, MonadMask m)
  => Int   -- ^ Maximum duration in seconds
  -> info  -- ^ Additional information to include in the exception if thrown
  -> m a -> m a
within t info io = do
    me <- liftIO $ myThreadId

    let timer :: IO ()
        timer = do
            interruptible $ -- Ensure that the timer can be killed
              threadDelay (t * 1_000_000)
            throwTo me $ Timeout info callStack

        startTimer :: m ThreadId
        startTimer = liftIO $ forkIO timer

        stopTimer :: ThreadId -> ExitCase a -> m ()
        stopTimer tid _ = liftIO $ killThread tid

    fmap fst $
      generalBracket startTimer stopTimer $ \_ -> io

withTemporaryFile :: (FilePath -> IO a) -> IO a
withTemporaryFile k = do
    tmpDir <- getTemporaryDirectory
    Control.Exception.bracket
      (openTempFile tmpDir "grapesy-test-suite.txt")
      (removeFile . fst)
      ( \(fp, h) -> do
          hClose h
          k fp
      )
