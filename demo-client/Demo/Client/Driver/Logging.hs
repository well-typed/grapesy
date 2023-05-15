module Demo.Client.Driver.Logging (
    threadSafeTracer
  , log
  ) where

import Prelude hiding (log)

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Tracer
import GHC.IO (unsafePerformIO)

tracingLock :: MVar ()
{-# NOINLINE tracingLock #-}
tracingLock = unsafePerformIO $ newMVar ()

threadSafeTracer :: Tracer IO String
threadSafeTracer = arrow $ emit $ \msg ->
    withMVar tracingLock $ \() -> putStrLn msg

log :: (MonadIO m, Show a) => a -> m ()
log = liftIO . traceWith threadSafeTracer . show