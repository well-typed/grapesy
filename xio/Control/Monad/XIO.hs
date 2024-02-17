-- | IO with explicit exceptions
--
-- Intended for qualified import.
--
-- > import Control.Monad.XIO (XIO', XIO, NeverThrows)
-- > import Control.Monad.XIO qualified as XIO
module Control.Monad.XIO (
    -- * Definition
    XIO' -- opaque
  , XIO
    -- * Converting between 'IO' and 'XIO'
  , runThrow
  , runCatch
  , liftIO
    -- * Exception handling
  , throwM
  , catchError
  , handleError
  , tryError
    -- * Ruling out exceptions
  , NeverThrows
  , neverThrows
  , run
  , swallowIO
  , unsafeTrustMe
  ) where

import Control.Exception (Exception, SomeException)
import Control.Exception qualified as Exception
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Catch qualified as MonadCatch
import Control.Monad.Except (MonadError)
import Control.Monad.Except qualified as MonadError
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as MonadIO

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | IO with explicit exceptions
--
-- Exceptions in 'XIO'' are explicit in the sense that
--
-- 1. They appear in the type:
--    we are explicit about /which/ exceptions can be thrown.
-- 2. They are /always/ synchronous:
--    we explicit about /when/ exceptions can be thrown.
--
-- Indeed, (1) only makes sense if we also have (2): if we allow for
-- asynchronous exceptions, any action can throw anything. Running an 'XIO'
-- action masks asynchronous exceptions, and running an 'IO' action inside of
-- 'XIO' (using 'liftIO') unmasks them again. For this reason, 'liftIO' only
-- applies to @XIO SomeException@.
--
-- @XIO' e a@ is a newtype wrapper around @IO a@; @e@ is a phantom type. As
-- such, there should be no performance penalty for using @XIO'@, apart from
-- perhaps the overhead of repeatedly unmasking exceptions.
newtype XIO' e a = Wrap { unwrap :: IO a }
  deriving newtype (Functor, Applicative, Monad)

-- The role of @e@ is nominal as exception handling is based on 'Typeable'
type role XIO' nominal representational

-- | 'XIO' specialized to arbitrary exceptions
--
-- 'XIO' carries no more information than 'IO' at the type level (both may throw
-- arbitrary exceptions), but is nonetheless still more explicit than 'IO':
-- all exceptions are still synchronous (that is, we are explicit about where
-- exceptions can be thrown).
type XIO = XIO' SomeException

{-------------------------------------------------------------------------------
  Converting between 'IO' and 'XIO'
-------------------------------------------------------------------------------}

-- | Run 'XIO' action, allowing it to throw exceptions
--
-- Asynchronous exceptions will be masked (and only unmasked when running 'IO'
-- actions, see 'liftIO').
runThrow :: XIO' e a -> IO a
runThrow = Exception.mask_ . unwrap

-- | Run 'XIO' action', catching any exceptions it may throw
--
-- See also 'runThrow'.
runCatch :: Exception e => XIO' e a -> IO (Either e a)
runCatch = Exception.try . runThrow

-- | The 'IO' action is marked as 'Exception.interruptible'
instance MonadIO (XIO' SomeException) where
  liftIO = Wrap . Exception.interruptible

-- | Type-specialization of 'MonadIO.liftIO'
--
-- This can help with inferring the @XIO e@ parameter.
liftIO :: IO a -> XIO a
liftIO = MonadIO.liftIO

{-------------------------------------------------------------------------------
  Exception handling
-------------------------------------------------------------------------------}

instance Exception e => MonadError e (XIO' e) where
  throwError = Wrap . Exception.throwIO
  catchError = Control.Monad.XIO.catchError

deriving newtype instance MonadThrow (XIO' SomeException)
deriving newtype instance MonadCatch (XIO' SomeException)

-- | Provided for interoperability with other packages only
--
-- @XIO@ does not require 'MonadMask' for safe resource allocation/deallocation,
-- since it rules out asynchronous exceptions.
deriving newtype instance MonadMask (XIO' SomeException)

-- | Type-specialization of 'MonadCatch.throwM'
--
-- This can help with inferring the @XIO e@ parameter.
throwM :: Exception e => e -> XIO a
throwM = MonadCatch.throwM

-- | Generalization of 'MonadError.catchError'
--
-- This allows the handler to have a different @XIO e'@ parameter, thereby
-- providing evidence that the exception has been handled.
catchError :: Exception e => XIO' e a -> (e -> XIO' e' a) -> XIO' e' a
catchError f h = Wrap $ unwrap f `Exception.catch` (unwrap . h)

-- | Generalization of 'MonadError.handleError'
--
-- See also 'catchError'.
handleError :: Exception e => (e -> XIO' e' a) -> XIO' e a -> XIO' e' a
handleError = flip catchError

-- | Generalization of 'MonadError.tryError'
--
-- See also 'catchError'.
tryError :: Exception e => XIO' e a -> XIO' e' (Either e a)
tryError = handleError (return . Left) . fmap Right

{-------------------------------------------------------------------------------
  Ruling out exceptions
-------------------------------------------------------------------------------}

-- | Mark an 'XIO' action that never throws any exceptions
--
-- Since all exceptions in 'XIO' are synchronous, if an action is marked as
-- 'NeverThrows', it will /really/ never throw (unless 'unsafeTrustMe'
-- is abused).
data NeverThrows

-- | Run an action that never throws any exceptions
--
-- This is just an alias for 'runThrow', with a more precise type.
run :: XIO' NeverThrows a -> IO a
run = runThrow

-- | Run an action that never throws in a context that might
--
-- When we have an action that never throws, we can give it one of two types:
--
-- > forall e. XIO' e a  -- option (A)
-- > XIO' NeverThrows a  -- option (B)
--
-- These are equivalent; the former is more convenient, but less explicit. In
-- this module we use 'NeverThrows' only in negative position (for /arguments/
-- that must never throw), opting for option (A) elsewhere; for client code
-- option (B) may however be more explicit. When option (B) is used,
-- 'neverThrows' can be used to recover the more general type of option (A).
neverThrows :: XIO' NeverThrows a -> XIO' e a
neverThrows = Wrap . unwrap

-- | Lift 'IO' action and swallow any synchronous exceptions it might throw
--
-- Wraps the argument in 'Exception.uninterruptibleMask_'. As such, the provisos
-- listed for 'Exception.uninterruptibleMask' apply here also. Use with care.
--
-- Note that
--
-- > swallowIO io
--
-- is /NOT/ the same as
--
-- > liftIO io `catchError` \_ -> return ()
--
-- In the latter, asynchronous exceptions are enabled and therefore potentially
-- swallowed along with any synchronous exceptions; this is typically not what
-- you want. In the former, asynchronous exceptions are masked, and the action
-- itself is wrapped in 'Exception.uninterruptibleMask_' (in case the action is
-- interruptible). This means that only exceptions thrown by the action /itself/
-- are swallowed.
--
-- This should be preferred over 'unsafeTrustMe' when possible.
swallowIO :: IO () -> XIO' e ()
swallowIO io =
      Wrap (Exception.uninterruptibleMask_ io)
    `catchError`
      \(_ :: SomeException) -> return ()

-- | Lift 'IO' actions for an arbitrary @XIO' e@ context
--
-- This can be used for @IO@ actions that the caller knows to only throw
-- exceptions of type @e@ (including none, for 'NeverThrows').
--
-- In order to ensure that this cannot throw any asynchronous exceptions, the
-- argument is wrapped in 'Exception.uninterruptibleMask_'. As such, the
-- provisos listed for 'Exception.uninterruptibleMask' apply here also. Use with
-- care.
unsafeTrustMe :: IO a -> XIO' e a
unsafeTrustMe = Wrap . Exception.uninterruptibleMask_

