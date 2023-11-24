{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Network.GRPC.Util.Concurrency (
    -- * @Control.Concurrent@
    module ReexportConcurrent
  , forkIO
  , forkIOWithUnmask
    -- * @Control.Concurrent.Async@
  , module ReexportAsync
  , withAsync
    -- * @Control.Concurrent.STM@
  , module ReexportSTM
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
  )
import Control.Concurrent.STM as ReexportSTM

import Control.Concurrent       qualified as Concurrent
import Control.Concurrent.Async qualified as Async

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

withAsync :: HasCallStack => IO a -> (Async a -> IO b) -> IO b
withAsync = Async.withAsync . wrapThreadBody

wrapThreadBody :: HasCallStack => IO a -> IO a
wrapThreadBody body = body
--wrapThreadBody body = do
--    tid <- myThreadId
--    writeFile ("tmp/" ++ show tid) $ prettyCallStack callStack
--    body

{-------------------------------------------------------------------------------
  STM
-------------------------------------------------------------------------------}
