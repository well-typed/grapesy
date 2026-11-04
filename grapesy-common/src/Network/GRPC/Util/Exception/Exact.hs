{-# LANGUAGE CPP #-}

module Network.GRPC.Util.Exception.Exact (
    ExactException(..)
    -- * Catching
  , catchExact
  , tryExact
  , waitCatchExact
    -- * Utilities
  , throwExact
  , withoutAnnotations
  , catchAndWrap
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM (STM)
import Control.Exception (Exception(..))
import Control.Exception qualified as Base
import Data.Bifunctor
import GHC.Stack

import Network.GRPC.Util.Exception.Shims

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Exception with emphasis on accurate annotations
--
-- When this type appears in the @grapesy@ API, it emphasises that we have tried
-- to ensure that any exception annotations are taken seriously.
--
-- Unlike the 'Exception' instance for 'SomeException', the instance for
-- 'ExactException' can be used with 'throwIO', 'throwTo', 'cancelWith', etc.,
-- without losing any annotations.
--
-- See also 'catchExact', 'tryExact', 'waitCatchExact'.
newtype ExactException = WrapExactException {
      unwrapExactException :: Base.SomeException
    }
  deriving stock (Show)

instance Exception ExactException where
  fromException    = Just . WrapExactException
  toException      = unwrapExactException
  displayException = displayException . unwrapExactException
#if MIN_VERSION_base(4,20,0)
  backtraceDesired = const False
#endif

{-------------------------------------------------------------------------------
  Catching 'ExactException'

  This is primarily useful to avoid accidentally throwing 'SomeException'.
-------------------------------------------------------------------------------}

-- | Catch 'ExactException'
--
-- Won't install any other kind of exception handler (i.e., no 'WhileHandling'
-- annotation will be added). This is comparable to 'catchNoPropagate' (see
-- discussion in 'ExactException'); there is no analogue of 'rethrowIO',
-- since 'throwIO' /itself/ can be used safely with 'ExactException'.
catchExact :: IO a -> (ExactException -> IO a) -> IO a
#if !MIN_VERSION_base(4,21,0)
catchExact = Base.catch
#else
catchExact action handler =
    Base.catchNoPropagate action (handler . aux)
  where
    -- NOTE: only used in GHC 9.12 and up
    aux :: Base.ExceptionWithContext Base.SomeException -> ExactException
    aux (Base.ExceptionWithContext _ctxt se) = WrapExactException se
#endif

tryExact :: IO a -> IO (Either ExactException a)
tryExact = Base.try

waitCatchExact :: Async a -> STM (Either ExactException a)
waitCatchExact = fmap (first WrapExactException) . waitCatchSTM

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

-- | Type-specialized wrapper around throwIO, to avoid mistakes
--
-- This does not need a `HasCallSTack` constraint, because no backtrace is
-- added to `ExactException`.
throwExact :: ExactException -> IO a
throwExact = Base.throwIO

withoutAnnotations :: ExactException -> (forall e. Exception e => e -> r) -> r
withoutAnnotations (WrapExactException (Base.SomeException e)) k = k e

-- | Wrap an exception
--
-- Notes:
--
-- * Since the original exception is wrapped as-is, including any annotations,
--   we use 'catchExact' to avoid adding a 'WhileHandling' annotation.
-- * We use 'throwIO' to throw the new wrapped exception, so that /if/ the
--   exception wrapper has 'backtraceDesired', we get a backtrace to the wrap.
catchAndWrap ::
     (HasCallStack, Exception e)
  => (ExactException -> e) -> IO a -> IO a
catchAndWrap f io = io `catchExact` (throwIO . f)
