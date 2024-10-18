module Demo.Client.API.Core.NoFinal.Greeter (
    sayHello
  ) where

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.API.Helloworld

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHello :: Connection -> Proto HelloRequest -> IO ()
sayHello conn n =
    withRPC conn def (Proxy @SayHello) $ \call -> do
      sendInput call $ StreamElem n
      out      <- recvOutput call
      trailers <- recvOutput call
      print (out, trailers)

