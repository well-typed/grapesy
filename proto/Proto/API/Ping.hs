{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API.Ping (
    Ping
  ) where

import Network.GRPC.Common
import Network.GRPC.Common.Binary

{-------------------------------------------------------------------------------
  Ping
-------------------------------------------------------------------------------}

type Ping = RawRpc "Ping" "ping"

type instance RequestMetadata          (RawRpc "Ping" meth) = NoMetadata
type instance ResponseInitialMetadata  (RawRpc "Ping" meth) = NoMetadata
type instance ResponseTrailingMetadata (RawRpc "Ping" meth) = NoMetadata
