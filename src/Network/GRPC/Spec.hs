-- | Parts of the gRPC specification that are not HTTP2 specific.
--
-- Intended for unqualified import.
module Network.GRPC.Spec (
    -- * Call parameters
    CallParams(..)
    -- ** Timeouts
  , Timeout(..)
  , TimeoutValue(TimeoutValue, getTimeoutValue)
  , TimeoutUnit(..)
    -- * Inputs (message sent to the peer)
  , IsFinal(..)
    -- * Outputs (messages received from the peer)
  , Trailers(..)
  ) where

import Data.Default

import Network.GRPC.Spec.CustomMetadata

{-------------------------------------------------------------------------------
  Requests
-------------------------------------------------------------------------------}

-- | RPC parameters
data CallParams = CallParams {
      -- | Timeout
      --
      -- If Timeout is omitted a server should assume an infinite timeout.
      -- Client implementations are free to send a default minimum timeout based
      -- on their deployment requirements.
      callTimeout :: Maybe Timeout

      -- | Custom metadata
    , callCustomMetadata :: [CustomMetadata]
    }
  deriving stock (Show)

-- | Default 'CallParams'
instance Default CallParams where
  def = CallParams {
        callTimeout        = Nothing
      , callCustomMetadata = []
      }

{-------------------------------------------------------------------------------
  Timeouts
-------------------------------------------------------------------------------}

data Timeout = Timeout TimeoutUnit TimeoutValue
  deriving stock (Show)

-- | Positive integer with ASCII representation of at most 8 digits
newtype TimeoutValue = UnsafeTimeoutValue {
      getTimeoutValue :: Word
    }
  deriving newtype (Show) -- relies on the Num instance

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
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Inputs (message sent to the peer)
-------------------------------------------------------------------------------}

-- | Mark a input sent as final
data IsFinal = Final | NotFinal
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Outputs (messages received from the peer)
-------------------------------------------------------------------------------}

-- | Information sent by the peer after the final output
data Trailers = Trailers {
      trailersGrpcStatus  :: Word
    , trailersGrpcMessage :: Maybe String
    }
  deriving stock (Show, Eq)

