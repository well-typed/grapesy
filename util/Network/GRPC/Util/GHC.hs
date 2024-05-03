module Network.GRPC.Util.GHC (
    -- * Thread labelling
    ThreadLabel
  , labelThisThread
  , forkLabelled
  , asyncLabelled
  ) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad.IO.Class
import GHC.Conc

{-------------------------------------------------------------------------------
  Thread labelling
-------------------------------------------------------------------------------}

type ThreadLabel = String

labelThisThread :: MonadIO m => ThreadLabel -> m ()
labelThisThread label = liftIO $ do
    tid <- myThreadId
    labelThread tid label

forkLabelled :: ThreadLabel -> IO () -> IO ThreadId
forkLabelled label io =
    mask_ $ forkIOWithUnmask $ \unmask -> do
      labelThisThread label
      unmask io

asyncLabelled :: ThreadLabel -> IO a -> IO (Async a)
asyncLabelled label io =
    mask_ $ asyncWithUnmask $ \unmask -> do
      labelThisThread label
      unmask io