{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Stress.Server.API (
    ManyServerStreaming
  ) where

import Network.GRPC.Common
import Network.GRPC.Common.Binary

type ManyServerStreaming = BinaryRpc "stresstest" "manyserverstreaming"

type instance RequestMetadata          (BinaryRpc "stresstest" meth) = NoMetadata
type instance ResponseInitialMetadata  (BinaryRpc "stresstest" meth) = NoMetadata
type instance ResponseTrailingMetadata (BinaryRpc "stresstest" meth) = NoMetadata