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
  , atomically
  , STMException(..)
  ) where

import Control.Exception
import GHC.Stack

import Control.Concurrent       as ReexportConcurrent hiding (forkIO, forkIOWithUnmask)
import Control.Concurrent.Async as ReexportAsync      hiding (withAsync)
import Control.Concurrent.STM   as ReexportSTM        hiding (atomically)

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

withAsync :: HasCallStack => IO a -> (Async a -> IO b) -> IO b
withAsync = Async.withAsync . wrapThreadBody

wrapThreadBody :: HasCallStack => IO a -> IO a
wrapThreadBody = id
--wrapThreadBody body = do
--    tid <- myThreadId
--    writeFile ("tmp/" ++ show tid) $ prettyCallStack callStack
--    body

{-------------------------------------------------------------------------------
  Wrap exceptions with a callstack
-------------------------------------------------------------------------------}

data STMException = STMException CallStack SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Rethrow STM exceptions with a callstakc
--
-- This is especially helpful to track down "blocked indefinitely" exceptions.
--
-- Implementation note: To catch such exceptions, we /must/ have the exception
-- handler /outside/ of the STM transaction.
atomically :: HasCallStack => STM a -> IO a
atomically action =
    STM.atomically action `catch` (throwIO . STMException callStack )