module Network.GRPC.Internal.NestedException (
    -- * Wrapping exceptions
    HasNestedException(..)
  , maybeNestedException
    -- * Wrapped exceptions in the exception hierarchy
  , SomeExceptionWrapper(..)
  , isExceptionWrapper
    -- ** Deriving-via support
  , ExceptionWrapper(..)
    -- * Example
  , WrapWithCallStack(..)
  ) where

import Control.Exception
import Data.Typeable
import GHC.Stack

{-------------------------------------------------------------------------------
  Wrapping exceptions
-------------------------------------------------------------------------------}

-- | Exceptions that have nested exceptions
--
-- This is primarily intended for exceptions that wrap other exceptions and add
-- some additional information. For example:
--
-- > data WrapWithCallStack e = WrapWithCallStack e CallStack
-- >   deriving stock (Show)
-- >   deriving Exception via ExceptionWrapper (WrapWithCallStack e)
-- >
-- > instance Exception e => Exception (WrapWithCallStack e) where
-- >   toException   = exceptionWrapperToException
-- >   fromException = exceptionWrapperFromException
--
-- In this case, we refer to @e@ as a /nested/ exception and to
-- 'WrapWithCallStack' as an exception /wrapper/.
class Exception e => HasNestedException e where
  getNestedException :: e -> SomeException

-- | Get nested exception, if it exists
maybeNestedException :: Exception e => e -> Maybe SomeException
maybeNestedException = fmap aux . isExceptionWrapper
  where
    aux :: SomeExceptionWrapper -> SomeException
    aux (SomeExceptionWrapper e) = getNestedException e

{-------------------------------------------------------------------------------
  Wrapped exceptions in the exception hierarchy
-------------------------------------------------------------------------------}

data SomeExceptionWrapper where
  SomeExceptionWrapper :: HasNestedException e => e -> SomeExceptionWrapper

deriving stock    instance Show      SomeExceptionWrapper
deriving anyclass instance Exception SomeExceptionWrapper

exceptionWrapperToException :: HasNestedException e => e -> SomeException
exceptionWrapperToException = toException . SomeExceptionWrapper

exceptionWrapperFromException :: Exception e => SomeException -> Maybe e
exceptionWrapperFromException e = do
    SomeExceptionWrapper e' <- fromException e
    cast e'

isExceptionWrapper :: Exception e => e -> Maybe SomeExceptionWrapper
isExceptionWrapper = fromException . toException

{-------------------------------------------------------------------------------
  Deriving-via support
-------------------------------------------------------------------------------}

-- | Deriving-via support for 'Exception' instances
--
-- Usage example:
--
-- > data WrapWithCallStack e = WrapWithCallStack e CallStack
-- >   deriving stock (Show)
-- >   deriving Exception via ExceptionWrapper (WrapWithCallStack e)
newtype ExceptionWrapper a = ExceptionWrapper a
  deriving newtype (Show)

instance HasNestedException a => Exception (ExceptionWrapper a) where
  toException (ExceptionWrapper a) = exceptionWrapperToException a
  fromException = fmap ExceptionWrapper . exceptionWrapperFromException

{-------------------------------------------------------------------------------
  Example
-------------------------------------------------------------------------------}

data WrapWithCallStack e = WrapWithCallStack e CallStack
  deriving stock (Show)
  deriving Exception via ExceptionWrapper (WrapWithCallStack e)

instance Exception e => HasNestedException (WrapWithCallStack e) where
  getNestedException (WrapWithCallStack e _) = toException e
