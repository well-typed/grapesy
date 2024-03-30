module Demo.Client.API.Core.NoFinal.Greeter (
    sayHello
  ) where

import Network.GRPC.Client
import Network.GRPC.Common

import Demo.Common.API

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHello :: Connection -> HelloRequest -> IO ()
sayHello conn n =
    withRPC conn def (Proxy @SayHello) $ \call -> do
      sendInput call $ StreamElem n
      out      <- recvOutput call
      trailers <- recvOutput call
      print (out, trailers)

