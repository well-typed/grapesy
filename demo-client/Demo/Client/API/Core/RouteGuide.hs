module Demo.Client.API.Core.RouteGuide (
    listFeatures
  ) where

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.RouteGuide

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
      initMetadata <- recvResponseInitialMetadata call
      logMsg initMetadata

      finalMetadata <- recvAllOutputs call $ logMsg
      logMsg finalMetadata




