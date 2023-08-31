module Network.GRPC.Spec.Call (
    -- * Parameters
    CallParams(..)
  ) where

import Data.Default

import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.Timeout

{-------------------------------------------------------------------------------
  Parameteres
-------------------------------------------------------------------------------}

-- | RPC parameters that can be chosen on a per-call basis
data CallParams = CallParams {
      -- | Timeout
      --
      -- If Timeout is omitted a server should assume an infinite timeout.
      -- Client implementations are free to send a default minimum timeout based
      -- on their deployment requirements.
      callTimeout :: Maybe Timeout

      -- | Custom metadata
      --
      -- This is the metadata included in the request. (The server can include
      -- its own metadata in the response: see 'responseMetadata' and
      -- 'trailerMetadatda'.)
    , callRequestMetadata :: [CustomMetadata]
    }
  deriving stock (Show, Eq)

-- | Default 'CallParams'
instance Default CallParams where
  def = CallParams {
        callTimeout         = Nothing
      , callRequestMetadata = []
      }

