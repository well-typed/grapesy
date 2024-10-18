-- | Utility exception types for the tests
module Test.Util.Exception
  ( -- * User exceptions
    SomeServerException(..)
  , SomeClientException(..)

    -- * Deliberate exceptions
  , DeliberateException(..)
  , ExceptionId
  ) where

import Control.Exception

{-------------------------------------------------------------------------------
  User exceptions

  When a test calls for the client or the server to throw an exception, we throw
  one of these. Their sole purpose is to be "any" kind of exception (not a
  specific one).
-------------------------------------------------------------------------------}

data SomeServerException = SomeServerException ExceptionId
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

data SomeClientException = SomeClientException ExceptionId
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

-- | Exception thrown by client or handler to test exception handling
data DeliberateException = forall e. Exception e => DeliberateException e
  deriving anyclass (Exception)
deriving stock instance Show DeliberateException

-- | We distinguish exceptions from each other simply by a number
type ExceptionId = Int
