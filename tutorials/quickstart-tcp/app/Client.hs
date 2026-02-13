module Client (main) where

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.API.Helloworld

main :: IO ()
main =
    withConnection def server $ \conn -> do
      let req = defMessage & #name .~ "you"
      resp <- nonStreaming conn (rpc @(Protobuf Greeter "sayHello")) req
      print resp
  where
    server :: Server
    server = ServerInsecure $ Address "127.0.0.1" defaultInsecurePort Nothing
