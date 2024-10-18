{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API.Ping (
    Ping

    -- * Re-exports
  , module Proto.Ping
) where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Ping

{-------------------------------------------------------------------------------
  Ping
-------------------------------------------------------------------------------}

type Ping = Protobuf PingService "ping"

type instance RequestMetadata          (Protobuf PingService meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf PingService meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf PingService meth) = NoMetadata
