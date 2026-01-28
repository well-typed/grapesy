{-# LANGUAGE CPP #-}

module Network.GRPC.Util.Backtrace (
    Backtraces -- opaque
  , collectBacktraces
  ) where

#if MIN_VERSION_base(4,20,0)

import Control.Exception.Backtrace qualified as Base
import GHC.Stack

newtype Backtraces = Backtraces {
      getBacktraces :: Base.Backtraces
    }

instance Show Backtraces where
  show = Base.displayBacktraces . getBacktraces

collectBacktraces :: HasCallStack => IO Backtraces
collectBacktraces = Backtraces <$> Base.collectBacktraces

#else

import GHC.Stack

newtype Backtraces = Backtraces {
      getBacktraces :: CallStack
    }

instance Show Backtraces where
  show = prettyCallStack . getBacktraces

collectBacktraces :: HasCallStack => IO Backtraces
collectBacktraces = return $ Backtraces callStack

#endif