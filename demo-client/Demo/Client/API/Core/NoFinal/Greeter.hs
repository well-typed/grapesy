module Demo.Client.API.Core.NoFinal.Greeter (
    sayHello
  ) where

import Control.Concurrent.STM
import Data.Default
import Data.Proxy

import Network.GRPC.Client

import Proto.Helloworld

{-------------------------------------------------------------------------------
  helloworld.Greeter
-------------------------------------------------------------------------------}

sayHello :: Connection -> HelloRequest -> IO ()
sayHello conn n =
    withRPC conn def (Proxy @(Protobuf Greeter "sayHello")) $ \call -> do
      atomically $ sendInput call $ StreamElem n
      out      <- atomically $ recvOutput call
      trailers <- atomically $ recvOutput call
      print (out, trailers)

