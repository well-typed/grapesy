-- | Convenient way to add timeouts to tests
module Test.Util.Within (
    Timeout(..)
  , within
  ) where

import Control.Exception
import GHC.Stack
import Control.Concurrent

data Timeout = forall a. Show a => Timeout a CallStack
  deriving anyclass (Exception)

deriving stock instance Show Timeout

-- | Limit execution time of an action
within ::
     (HasCallStack, Show info)
  => Int   -- ^ Maximum duration in seconds
  -> info  -- ^ Additional information to include in the exception if thrown
  -> IO a -> IO a
within t info io = do
    me <- myThreadId

    let timer :: IO ()
        timer = do
            interruptible $ -- Ensure that the timer can be killed
              threadDelay (t * 1_000_000)
            throwTo me $ Timeout info callStack

    bracket (forkIO timer) killThread $ \_ -> io

