module Demo.Client.API.Core.RouteGuide (
    listFeatures
  ) where

import Data.Default
import Data.Proxy

import Network.GRPC.Client

import Proto.RouteGuide

import Debug.Concurrent

import Demo.Common.Logging

{-------------------------------------------------------------------------------
  RouteGuide
-------------------------------------------------------------------------------}

listFeatures :: Connection -> Rectangle -> IO ()
listFeatures conn r = do
    withRPC conn def (Proxy @(Protobuf RouteGuide "listFeatures")) $ \call -> do
      sendFinalInput call r

      -- Show response custom metadata
      --
      -- In the gRPC @Trailers-Only@ case (which will be triggered if there
      -- are zero features in the provided rectangle), this will also be the
      -- trailing custom metadata.
      initMetadata <- atomically $ recvResponseMetadata call
      logMsg initMetadata

      finalMetadata <- recvAllOutputs call $ logMsg
      logMsg finalMetadata




