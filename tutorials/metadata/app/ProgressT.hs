-- | Very simple progress bar
module ProgressT (
    ProgressT -- opaque
  , runProgressT
  , updateProgressBar
  ) where

import Control.Monad.Catch
import Control.Monad.State
import System.IO

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype ProgressT m a = WrapProgressT {
      unwrapProgressT :: StateT ProgressState m a
    }
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

data ProgressState = ProgressState {
      -- | Progress made so far
      progressSoFar :: Integer

      -- | The values of 'progressSoFar' for which we still want to print a dot
    , progressDots :: [Integer]
    }

instance MonadState s m => MonadState s (ProgressT m) where
  state = lift . state

{-------------------------------------------------------------------------------
  Running
-------------------------------------------------------------------------------}

runProgressT ::
     (MonadMask m, MonadIO m, Integral n)
  => n  -- ^ Total size
  -> ProgressT m a -> m a
runProgressT totalSize =
      (`finally` liftIO (putStrLn " done"))
    . flip evalStateT initState
    . unwrapProgressT
  where
    initState :: ProgressState
    initState = ProgressState {
          progressSoFar = 0
        , progressDots  = take numDots $ iterate (+ stepSize) stepSize
        }

    numDots :: Int
    numDots = 50

    stepSize :: Integer
    stepSize = fromIntegral totalSize `div` fromIntegral numDots

{-------------------------------------------------------------------------------
  Monad-specific functionality
-------------------------------------------------------------------------------}

updateProgressBar :: forall m n.
     (MonadIO m, Integral n)
  => n  -- ^ New progress made (i.e., the /change/ in progress)
  -> ProgressT m ()
updateProgressBar delta = WrapProgressT $ do
    ProgressState{progressSoFar, progressDots} <- get
    let progressSoFar' = progressSoFar + fromIntegral delta
    progressDots' <- liftIO $
        printDots progressSoFar' progressDots `finally` hFlush stdout
    put $ ProgressState{
        progressSoFar = progressSoFar'
      , progressDots  = progressDots'
      }
  where
    printDots :: Integer -> [Integer] -> IO [Integer]
    printDots soFar = go
      where
        go :: [Integer] -> IO [Integer]
        go []     = return []
        go (d:ds) = if d <= soFar
                      then putChar '.' >> go ds
                      else return (d:ds)
