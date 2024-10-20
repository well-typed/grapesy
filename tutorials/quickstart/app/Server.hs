module Server (main) where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Proto.API.Helloworld

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

sayHello :: Proto HelloRequest -> IO (Proto HelloReply)
sayHello req = do
    let resp = defMessage & #message .~ "Hello, " <> req ^. #name
    return resp

{-------------------------------------------------------------------------------
  Server top-level
-------------------------------------------------------------------------------}

methods :: Methods IO (ProtobufMethodsOf Greeter)
methods =
      Method (mkNonStreaming sayHello)
    $ NoMoreMethods

main :: IO ()
main =
    runServerWithHandlers def config $ fromMethods methods
  where
    config :: ServerConfig
    config = ServerConfig {
          serverInsecure = Just (InsecureConfig Nothing defaultInsecurePort)
        , serverSecure   = Nothing
        }
