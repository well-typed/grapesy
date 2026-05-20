-- | Utility exception types for the tests
module Test.Util.Exception
  ( -- * User exceptions
--    SomeServerException(..)
--  , SomeClientException(..)

    -- * Deliberate exceptions
    DeliberateException(..)
  , ExceptionId
  ) where

import Control.Exception

{-------------------------------------------------------------------------------
  User exceptions

  When a test calls for the client or the server to throw an exception, we throw
  one of these. Their sole purpose is to be "any" kind of exception (not a
  specific one).
-------------------------------------------------------------------------------}

-- | Deliberate exceptions do not constitute test failures
data DeliberateException =
    -- | Deliberate exception thrown in the server
    DeliberateServerException ExceptionId

    -- | Deliberate exception thrown in the client
  | DeliberateClientException ExceptionId
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

-- | We distinguish exceptions from each other simply by a number
type ExceptionId = Int
