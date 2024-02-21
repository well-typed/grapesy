module Test.Stress.Server.API (
    ManyShortLived
  ) where

import Network.GRPC.Common.Binary

type ManyShortLived = BinaryRpc "stresstest" "manyshortlived"
