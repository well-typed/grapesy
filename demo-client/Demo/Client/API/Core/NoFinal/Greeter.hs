module Demo.Client.API.Core.NoFinal.Greeter (
    sayHello
  ) where

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Helloworld

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHello :: Connection -> HelloRequest -> IO ()
sayHello conn n =
    withRPC conn def (Proxy @(Protobuf Greeter "sayHello")) $ \call -> do
      sendInput call $ StreamElem n
      out      <- recvOutput call
      trailers <- recvOutput call
      print (out, trailers)

