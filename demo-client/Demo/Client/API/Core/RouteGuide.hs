module Demo.Client.API.Core.RouteGuide (
    listFeatures
  ) where

import Prelude hiding (log)

import Control.Concurrent.STM
import Data.Default

import Network.GRPC.Client
import Network.GRPC.Client.Protobuf (RPC(..))

import Proto.RouteGuide

import Demo.Common.Logging

{-------------------------------------------------------------------------------
  RouteGuide
-------------------------------------------------------------------------------}

listFeatures :: Connection -> Rectangle -> IO ()
listFeatures conn r = do
    withRPC conn def (RPC @RouteGuide @"listFeatures") $ \call -> do
      sendOnlyInput call r

      -- Show response custom metadata
      --
      -- In the gRPC @Trailers-Only@ case (which will be triggered if there
      -- are zero features in the provided rectangle), this will also be the
      -- trailing custom metadata.
      initMetadata <- atomically $ recvResponseMetadata call
      log initMetadata

      finalMetadata <- recvAllOutputs call log
      log finalMetadata




