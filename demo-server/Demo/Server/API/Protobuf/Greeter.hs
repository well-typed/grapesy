{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo.Server.API.Protobuf.Greeter (handlers) where

import Control.Lens ((.~), (^.))
import Control.Monad
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()
import Data.Text (Text)

import Network.GRPC.Server.Protobuf

import Proto.Helloworld

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handlers :: MethodsOf IO Greeter
handlers =
      Method sayHello
    $ Method sayHelloStreamReply
    $ NoMoreMethods

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

sayHello :: NonStreamingHandler IO Greeter "sayHello"
sayHello = nonStreaming $ \req -> do
    let msg :: Text
        msg = "Hello, " <> req ^. #name <> "!"

    return (defMessage & #message .~ msg, [])

-- TODO: Currently this just tests server-side streaming of responses.
-- We should emulate the Python example code more closely here.
sayHelloStreamReply :: ServerStreamingHandler IO Greeter "sayHelloStreamReply"
sayHelloStreamReply = serverStreaming $ \req send -> do
    let msg :: Text -> Text
        msg i = "Hello " <> req ^. #name <> " times " <> i

    forM_ ["0", "1", "2"] $ \i ->
      send $ defMessage & #message .~ msg i

    return []
