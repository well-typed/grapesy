{-# LANGUAGE PartialTypeSignatures #-}
module Network.GRPC.Util.STM (
    module Exported
  , atomically
  , STMException(..)
  ) where

import Control.Concurrent.STM as Exported hiding (atomically)
import Control.Concurrent.STM qualified as NotExported
import Control.Exception
import GHC.Stack

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
    NotExported.atomically action `catch` (throwIO . STMException callStack )
