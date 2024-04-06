{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Stress.Server.API (
    ManyShortLived
  ) where

import Network.GRPC.Common
import Network.GRPC.Common.Binary

type ManyShortLived = BinaryRpc "stresstest" "manyshortlived"

type instance RequestMetadata          (BinaryRpc "stresstest" meth) = NoMetadata
type instance ResponseInitialMetadata  (BinaryRpc "stresstest" meth) = NoMetadata
type instance ResponseTrailingMetadata (BinaryRpc "stresstest" meth) = NoMetadata
