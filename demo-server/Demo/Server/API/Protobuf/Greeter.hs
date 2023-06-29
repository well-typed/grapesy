{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo.Server.API.Protobuf.Greeter (handlers) where

import Control.Lens ((.~), (^.))
import Control.Monad
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()
import Data.Proxy
import Data.Text (Text)

import Network.GRPC.Common.CustomMetadata (CustomMetadata(..))
import Network.GRPC.Common.StreamType
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.StreamType

import Proto.Helloworld

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handlers :: Methods IO (ProtobufMethodsOf Greeter)
handlers =
      Method (mkNonStreaming sayHello)
    $ RawMethod sayHelloStreamReply
    $ NoMoreMethods

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

sayHello :: HelloRequest -> IO HelloReply
sayHello req = return $ defMessage & #message .~ msg
  where
    msg :: Text
    msg = "Hello, " <> req ^. #name <> "!"

sayHelloStreamReply :: RpcHandler IO
sayHelloStreamReply =
    (mkRpcHandler (Proxy @(Protobuf Greeter "sayHelloStreamReply")) go) {
        handlerMetadata = \_reqMetadata -> return [
            AsciiHeader "initial-md" "initial-md-value"
          ]
      }
  where
    go :: Call (Protobuf Greeter "sayHelloStreamReply") -> IO ()
    go call = do
        req <- recvFinalInput call

        let msg :: Text -> Text
            msg i = "Hello " <> req ^. #name <> " times " <> i

        forM_ ["0", "1", "2"] $ \i ->
          sendNextOutput call $ defMessage & #message .~ msg i

        sendTrailers call []
