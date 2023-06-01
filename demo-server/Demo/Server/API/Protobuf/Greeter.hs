{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo.Server.API.Protobuf.Greeter (handlers) where

import Control.Lens ((.~), (^.))
import Control.Monad
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()
import Data.Text (Text)

import Network.GRPC.Server
import Network.GRPC.Server.Protobuf

import Proto.Helloworld

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handlers :: MethodsOf IO Greeter
handlers =
      Method sayHello
    $ RawMethod sayHelloStreamReply
    $ NoMoreMethods

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

sayHello :: NonStreamingHandler IO Greeter "sayHello"
sayHello = nonStreaming $ \req -> do
    let msg :: Text
        msg = "Hello, " <> req ^. #name <> "!"

    return $ defMessage & #message .~ msg

sayHelloStreamReply :: RpcHandler IO
sayHelloStreamReply =
    (mkRpcHandler (RPC @Greeter @"sayHelloStreamReply") go) {
        handlerMetadata = \_reqMetadata -> return [
            AsciiHeader "initial-md" "initial-md-value"
          ]
      }
  where
    go :: Call (RPC Greeter "sayHelloStreamReply") -> IO ()
    go call = do
        req <- recvOnlyInput call

        let msg :: Text -> Text
            msg i = "Hello " <> req ^. #name <> " times " <> i

        forM_ ["0", "1", "2"] $ \i ->
          sendNextOutput call $ defMessage & #message .~ msg i

        sendTrailers call []
