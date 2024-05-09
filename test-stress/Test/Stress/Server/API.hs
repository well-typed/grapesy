{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Stress.Server.API (
    ManyNonStreaming
  , ManyClientStreaming
  , ManyServerStreaming
  , ManyBiDiStreaming
  , ManyReconnects
  ) where

import Network.GRPC.Common
import Network.GRPC.Common.Binary

type ManyNonStreaming    = BinaryRpc "stresstest" "manynonstreaming"
type ManyClientStreaming = BinaryRpc "stresstest" "manyclientstreaming"
type ManyServerStreaming = BinaryRpc "stresstest" "manyserverstreaming"
type ManyBiDiStreaming   = BinaryRpc "stresstest" "manybidistreaming"
type ManyReconnects      = BinaryRpc "stresstest" "manyreconnects"

type instance RequestMetadata          (BinaryRpc "stresstest" meth) = NoMetadata
type instance ResponseInitialMetadata  (BinaryRpc "stresstest" meth) = NoMetadata
type instance ResponseTrailingMetadata (BinaryRpc "stresstest" meth) = NoMetadata
