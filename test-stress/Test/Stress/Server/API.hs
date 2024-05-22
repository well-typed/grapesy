{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Stress.Server.API (
    ManyShortLived
  ) where

import Network.GRPC.Common
import Network.GRPC.Common.Binary

type ManyShortLived = RawRpc "stresstest" "manyshortlived"

type instance RequestMetadata          (RawRpc "stresstest" meth) = NoMetadata
type instance ResponseInitialMetadata  (RawRpc "stresstest" meth) = NoMetadata
type instance ResponseTrailingMetadata (RawRpc "stresstest" meth) = NoMetadata
