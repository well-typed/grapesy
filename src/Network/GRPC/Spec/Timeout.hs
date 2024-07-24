module Network.GRPC.Spec.Timeout (
    -- * Timeouts
    Timeout(..)
  , TimeoutValue(TimeoutValue, getTimeoutValue)
  , TimeoutUnit(..)
  , isValidTimeoutValue
    -- * Translation
  , timeoutToMicro
  ) where

import GHC.Generics (Generic)
import GHC.Show

{-------------------------------------------------------------------------------
  Timeouts
-------------------------------------------------------------------------------}

data Timeout = Timeout TimeoutUnit TimeoutValue
  deriving stock (Show, Eq, Generic)

-- | Positive integer with ASCII representation of at most 8 digits
newtype TimeoutValue = UnsafeTimeoutValue {
      getTimeoutValue :: Word
    }
  deriving newtype (Eq)
  deriving stock (Generic)

-- | 'Show' instance relies on the 'TimeoutValue' pattern synonym
instance Show TimeoutValue where
  showsPrec p (UnsafeTimeoutValue val) = showParen (p >= appPrec1) $
        showString "TimeoutValue "
      . showsPrec appPrec1 val

pattern TimeoutValue :: Word -> TimeoutValue
pattern TimeoutValue t <- UnsafeTimeoutValue t
  where
    TimeoutValue t
      | isValidTimeoutValue t = UnsafeTimeoutValue t
      | otherwise = error $ "invalid TimeoutValue: " ++ show t

{-# COMPLETE TimeoutValue #-}

isValidTimeoutValue :: Word -> Bool
isValidTimeoutValue t = length (show t) <= 8

data TimeoutUnit =
    Hour
  | Minute
  | Second
  | Millisecond
  | Microsecond
  | Nanosecond
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

-- | Translate 'Timeout' to microseconds
--
-- For 'Nanosecond' timeout we round up.
--
-- Note: the choice of 'Integer' for the result is important: timeouts can be
-- quite long, and might easily exceed the range of a 32-bit int: @2^31@
-- microseconds is roughly 35 minutes (on 64-bit architectures this is much less
-- important; @2^63@ microseconds is 292,277.2 /years/). We could use @Int64@ or
-- @Word64@, but 'Integer' works nicely with the @unbounded-delays@ package.
timeoutToMicro :: Timeout -> Integer
timeoutToMicro = \case
    Timeout Hour        (TimeoutValue n) -> mult n $ 1 * 1_000 * 1_000 * 60 * 24
    Timeout Minute      (TimeoutValue n) -> mult n $ 1 * 1_000 * 1_000 * 60
    Timeout Second      (TimeoutValue n) -> mult n $ 1 * 1_000 * 1_000
    Timeout Millisecond (TimeoutValue n) -> mult n $ 1 * 1_000
    Timeout Microsecond (TimeoutValue n) -> mult n $ 1
    Timeout Nanosecond  (TimeoutValue n) -> nano n
  where
    mult :: Word -> Integer -> Integer
    mult n m = fromIntegral n * m

    nano :: Word -> Integer
    nano n = fromIntegral $
        mu + if n' == 0 then 0 else 1
      where
        (mu, n') = divMod n 1_000
