module Demo.Client.API.Core.Greeter (
    sayHelloStreamReply
  , sayHelloBidiStream
  ) where

import Control.Exception

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Demo.Client.Util.DelayOr (DelayOr)
import Demo.Client.Util.DelayOr qualified as DelayOr
import Demo.Client.Util.Logging
import Demo.Common.API

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHelloStreamReply :: Connection -> Proto HelloRequest -> IO ()
sayHelloStreamReply conn name =
    withRPC conn def (Proxy @SayHelloStreamReply) $ \call -> do
      -- The server only sends a response once we send an input
      sendFinalInput call name

      -- We should see the response metadata immediately, and the first output
      -- after a delay.
      initMetadata <- recvResponseInitialMetadata call
      logMsg initMetadata

      -- For completeness, we also show the final metadata, although the
      -- example does not include any.
      finalMetadata <- recvAllOutputs call logMsg
      logMsg finalMetadata

sayHelloBidiStream :: Connection -> [DelayOr (Proto HelloRequest)] -> IO ()
sayHelloBidiStream conn names = handle cancelled $
    withRPC conn def (Proxy @SayHelloBidiStream) $ \call -> do
      DelayOr.forM_ names $ \name -> do
        sendNextInput call name
        print =<< recvNextOutput call
  where
    cancelled :: GrpcException -> IO ()
    cancelled err
      | grpcError err == GrpcCancelled
      = putStrLn "sayHelloBidiStream client: cancelled."

      | otherwise
      = throwIO err

