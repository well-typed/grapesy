{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Network.GRPC.Util.Concurrency (
    -- * @Control.Concurrent@
    module ReexportConcurrent
  , forkIO
  , forkIOWithUnmask
    -- * @Control.Concurrent.Async@
  , module ReexportAsync
  , withAsync
  , async
  , asyncWithUnmask
    -- * @Control.Concurrent.STM@
  , module ReexportSTM
  , atomically
  ) where

import Prelude hiding (id)

import Control.Exception
import GHC.Stack

import Control.Concurrent as ReexportConcurrent hiding (
    forkIO
  , forkIOWithUnmask
  )
import Control.Concurrent.Async as ReexportAsync hiding (
    withAsync
  , async
  , asyncWithUnmask
  )
import Control.Concurrent.STM as ReexportSTM hiding (
    atomically
  )

import Control.Concurrent       qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM   qualified as STM

{-------------------------------------------------------------------------------
  Wrap thread spawning
-------------------------------------------------------------------------------}

forkIO :: HasCallStack => IO () -> IO ThreadId
forkIO = Concurrent.forkIO . wrapThreadBody

forkIOWithUnmask ::
     HasCallStack
  => ((forall a. IO a -> IO a) -> IO ())
  -> IO ThreadId
forkIOWithUnmask k = Concurrent.forkIOWithUnmask $ \unmask ->
    wrapThreadBody (k unmask)

async :: HasCallStack => IO a -> IO (Async a)
async = Async.async . wrapThreadBody

withAsync :: HasCallStack => IO a -> (Async a -> IO b) -> IO b
withAsync = Async.withAsync . wrapThreadBody

asyncWithUnmask ::
     HasCallStack
  => ((forall b. IO b -> IO b) -> IO a)
  -> IO (Async a)
asyncWithUnmask k = Async.asyncWithUnmask $ \unmask ->
    wrapThreadBody (k unmask)

wrapThreadBody :: HasCallStack => IO a -> IO a
--wrapThreadBody body = body
wrapThreadBody body = do
    tid <- myThreadId
    writeFile ("tmp/" ++ show tid) (prettyCallStack callStack ++ "\n")
    body

{-------------------------------------------------------------------------------
  STM
-------------------------------------------------------------------------------}

atomically :: forall a. HasCallStack => STM a -> IO a
atomically = STM.atomically

{-
atomically :: forall a. HasCallStack => STM a -> IO a
atomically stm = go 2
  where
    go ::
         Int   -- ^ Number of guarded attempts left
      -> IO a
    go 0 = run
    go n = run `catch` \BlockedIndefinitelyOnSTM{} -> go (n - 1)

    run :: IO a
    run = STM.atomically stm
-}