module Network.GRPC.Common.JSON (
    JsonRpc

    -- * Aeson support
  , JsonObject(..)
  , Required(..)
  , Optional(..)

    -- * Re-exports
    -- ** Aeson
  , ToJSON(..)
  , FromJSON(..)
  ) where

import Data.Aeson

import Network.GRPC.Spec

