{-# LANGUAGE CPP #-}

module Test.Util (
    -- * Bytestring utilities
    dropEnd
    -- * Timeouts
  , Timeout(..)
  , within
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import GHC.Stack

{-------------------------------------------------------------------------------
  Bytestring utilities
-------------------------------------------------------------------------------}

dropEnd :: Int -> Strict.ByteString -> Strict.ByteString
#if MIN_VERSION_bytestring(0,11,1)
dropEnd = BS.Strict.dropEnd
#else
dropEnd n xs = BS.Strict.take (BS.Strict.length xs - n) xs
#endif

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


