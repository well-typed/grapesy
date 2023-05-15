module Demo.Client.API.Core.NoFinal.Greeter (
    sayHello
  ) where

import Control.Concurrent.STM
import Network.GRPC.Client
import Network.GRPC.Protobuf (RPC(..))

import Proto.Helloworld

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHello :: Connection -> PerCallParams -> HelloRequest -> IO ()
sayHello conn params n =
    withRPC conn params (RPC @Greeter @"sayHello") $ \call -> do
      atomically $ sendInput call NotFinal (Just n)
      out      <- atomically $ recvOutput call
      trailers <- atomically $ recvOutput call
      print (out, trailers)
