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

-- | Run STM transaction
--
-- When running an STM tranaction throws a "blocked indefinitely" exception,
-- there should be no point running it again; after all, it is blocked
-- /indefinitely/. It seems however that this is not in fact guaranteed by the
-- STM implementation, and the grapesy test suite manages to trigger a case
-- where the test fails with this exception even though the thread definitely
-- is /not/ blocked indefinitely, and indeed simply trying again is enough.
--
-- (The documentation of @Control.Concurrent@ at
-- <https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Concurrent.html#g:14>
-- mentions an exceptional case related to finalizers
-- (see also <https://gitlab.haskell.org/ghc/ghc/-/issues/11001>)
-- but that is not our case here.
--
-- This is obviously a huge hack and has got to be of the worst functions I have
-- ever written (and I have written some pretty horrible functions in my time).
--
-- TODO: Fix this properly.
-- TODO: Is 1 guarded attempt enough..? Do we need more..? Unlimited..?
atomically :: forall a. STM a -> IO a
atomically = STM.atomically
