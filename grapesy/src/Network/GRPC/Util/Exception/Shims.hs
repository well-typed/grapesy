{-# LANGUAGE CPP            #-}
{-# LANGUAGE ImplicitParams #-}

module Network.GRPC.Util.Exception.Shims (
    -- * Throwing
    throwIO
  , throwM
    -- * Showable wrapper for 'Backtraces' (or 'CallStack' for GHC < 9.10)
  , Backtraces
  , collectBacktraces
  , displayBacktraces
    -- * Make `annotateIO` a no-op for GHC < 9.10
  , annotateIO
  , addExceptionContext
    -- * Bug-free version of 'ExceptionWithContext'
  , WithAnnotations
  , pattern WithAnnotations
    -- * STM
#ifndef PATCHED_GHC_FOR_EXCEPTION_DEBUGGING
  , AtomicallyBacktrace(..)
#endif
  , atomically
  ) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Exception (Exception(..))
import Control.Exception qualified as Base
import Control.Monad.Catch qualified as Exceptions
import GHC.Generics
import GHC.Stack

#if MIN_VERSION_base(4,20,0)
import Control.Exception.Annotation
import Control.Exception.Backtrace qualified as Backtrace
import Control.Exception.Context
#endif

{-------------------------------------------------------------------------------
  Throwing

  This just adds the @HasCallStack@ constraint, even when it is technically
  speaking redundant (in older GHC). Avoids redundant constraints warnings
  upstream.
-------------------------------------------------------------------------------}

throwIO :: (Exception e, HasCallStack) => e -> IO a
throwIO = Base.throwIO
#if !MIN_VERSION_base(4,20,0)
  where
    _suppressRedundantConstraintWarning = callStack
#endif

throwM :: (Exception e, Exceptions.MonadThrow m, HasCallStack) => e -> m a
throwM = Exceptions.throwM
#if !MIN_VERSION_exceptions(0,10,6)
  where
    _suppressRedundantConstraintWarning = callStack
#endif

{-------------------------------------------------------------------------------
  Showable wrapper for 'Backtraces' (or 'CallStack' for GHC < 9.10)
-------------------------------------------------------------------------------}

#if !MIN_VERSION_base(4,20,0)
newtype Backtraces = WrapBacktraces {
      unwrapBacktraces :: CallStack
    }
  deriving stock (Show)

collectBacktraces :: HasCallStack => IO Backtraces
collectBacktraces = return $ WrapBacktraces GHC.Stack.callStack

displayBacktraces :: Backtraces -> String
displayBacktraces = prettyCallStack . unwrapBacktraces

#else

newtype Backtraces = WrapBacktraces {
      unwrapBacktraces :: Backtrace.Backtraces
    }

-- Frustratingly, 'Backtraces' does not have a law-abiding 'Show' instance
instance Show Backtraces where
  show = displayBacktraces

collectBacktraces :: HasCallStack => IO Backtraces
collectBacktraces = WrapBacktraces <$> Backtrace.collectBacktraces

displayBacktraces :: Backtraces -> String
displayBacktraces = Backtrace.displayBacktraces . unwrapBacktraces

#endif

{-------------------------------------------------------------------------------
  Make `annotateIO` a no-op for GHC < 9.10
-------------------------------------------------------------------------------}

#if !MIN_VERSION_base(4,20,0)

annotateIO :: ann -> IO a -> IO a
annotateIO _ = id

addExceptionContext :: ann -> Base.SomeException -> Base.SomeException
addExceptionContext _ = id

#else

annotateIO ::
     ExceptionAnnotation ann
  => ann -> IO a -> IO a
annotateIO = Base.annotateIO

addExceptionContext ::
     ExceptionAnnotation ann
  => ann -> Base.SomeException -> Base.SomeException
addExceptionContext = Base.addExceptionContext

#endif

{-------------------------------------------------------------------------------
  Bug-free version of 'ExceptionWithContext'

  In GHC 9.10 'ExceptionWithContext' is broken (throwing something of type
  @ExceptionWithContext SomeException@ will result in nested @SomeException@,
  breaking exception handlers). Prior to GHC 9.10 it is not available at all.
  The implementation here stays as close as possible to the one in GHC 9.14.
-------------------------------------------------------------------------------}

#if !MIN_VERSION_base(4,20,0)

data ExceptionContext = EmptyExceptionContext
  deriving stock (Show)

data WithAnnotations a = WithAnnotations ExceptionContext a
  deriving stock (Show)

instance Exception e => Exception (WithAnnotations e) where
  toException (WithAnnotations EmptyExceptionContext e) =
      toException e

  fromException se = do
      e <- fromException se
      return (WithAnnotations EmptyExceptionContext e)

  displayException = displayException . toException

#elif !MIN_VERSION_base(4,21,0)

-- | Bug-free replacement for 'ExceptionWithContext' in GHC 9.10
data WithAnnotations e = WithAnnotations ExceptionContext e
  deriving stock Generic

instance Exception a => Show (WithAnnotations a) where
  show (WithAnnotations _ctxt e) = show e

instance Exception a => Exception (WithAnnotations a) where
    toException (WithAnnotations ctxt e) =
        case toException e of
          Base.SomeException c ->
            let ?exceptionContext = ctxt
            in Base.SomeException c
    fromException se = do
        e <- fromException se
        return (WithAnnotations (Base.someExceptionContext se) e)
    backtraceDesired (WithAnnotations _ e) = backtraceDesired e
    displayException = displayException . toException

#else

type WithAnnotations = Base.ExceptionWithContext

pattern WithAnnotations :: ExceptionContext -> a -> WithAnnotations a
pattern WithAnnotations ctxt e = Base.ExceptionWithContext ctxt e

#endif

{-------------------------------------------------------------------------------
  STM
-------------------------------------------------------------------------------}

#ifdef PATCHED_GHC_FOR_EXCEPTION_DEBUGGING
-- Nothing to do, part of the patch
#else
-- | Backtrace to a call to 'atomically'
--
-- When an STM transaction throws an exception, this will tell us where that
-- tranaction was invoked (though not where /within/ the transaction it
-- threw an exception).
newtype AtomicallyBacktrace = AtomicallyBacktrace Backtraces
  deriving stock (Generic, Show)
#if MIN_VERSION_base(4,20,0)
  deriving anyclass (ExceptionAnnotation)
#endif

atomically :: HasCallStack => STM a -> IO a
atomically stm = do
    backtraces <- collectBacktraces
    annotateIO (AtomicallyBacktrace backtraces) $
      STM.atomically stm
#endif
