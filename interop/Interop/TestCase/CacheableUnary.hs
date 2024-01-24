module Interop.TestCase.CacheableUnary (
    handleCacheableUnaryCall
  ) where

import Data.ProtoLens

import Network.GRPC.Common.StreamType
import Network.GRPC.Server

import Proto.Src.Proto.Grpc.Testing.Test
import Proto.Src.Proto.Grpc.Testing.Messages

handleCacheableUnaryCall ::
     NonStreamingHandler IO (Protobuf TestService "cacheableUnaryCall")
handleCacheableUnaryCall =
    mkNonStreaming go
  where
    go :: SimpleRequest -> IO SimpleResponse
    go _ = return defMessage

