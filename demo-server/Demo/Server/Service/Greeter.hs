{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo.Server.Service.Greeter (handlers) where

import Control.Monad
import Data.Proxy
import Data.Text (Text)

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
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
    $ UnsupportedMethod -- TODO: sayHelloBidiStream
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
    mkRpcHandler (Proxy @(Protobuf Greeter "sayHelloStreamReply")) go
  where
    go :: Call (Protobuf Greeter "sayHelloStreamReply") -> IO ()
    go call = do
        setResponseMetadata call [
            CustomMetadata "initial-md" "initial-md-value"
          ]

        -- The client expects the metadata well before the first output
        _ <- initiateResponse call

        req <- recvFinalInput call

        let msg :: Text -> Text
            msg i = "Hello " <> req ^. #name <> " times " <> i

        forM_ ["0", "1", "2"] $ \i ->
          sendNextOutput call $ defMessage & #message .~ msg i

        sendTrailers call def
