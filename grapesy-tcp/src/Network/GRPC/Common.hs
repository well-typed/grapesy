module Network.GRPC.Common (

    --
    -- * Stream elements
    --
    -- We export only the main type here; for operations on 'StreamElem', see
    -- "Network.GRPC.Common.StreamElem" (intended for qualified import).
    StreamElem(..),
    NextElem(..),

    -- * Custom metadata
    NoMetadata(..),

    -- * Convenience re-exports
    Proxy(..),
    Default(..),
) where

import Data.Default
import Data.Proxy

import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Spec

