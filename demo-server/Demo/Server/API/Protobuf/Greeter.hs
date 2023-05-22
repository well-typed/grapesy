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

handlers :: MethodsOf Greeter
handlers = Methods $
      Method sayHello
    $ Method sayHelloStreamReply
    $ NoMoreMethods

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

sayHello :: HelloRequest -> IO (HelloReply, [CustomMetadata])
sayHello req = do
    return (defMessage & #message .~ msg, [])
  where
    msg :: Text
    msg = "Hello, " <> req ^. #name <> "!"

sayHelloStreamReply ::
     HelloRequest
  -> (HelloReply -> IO ())
  -> IO [CustomMetadata]
sayHelloStreamReply req send = do
    -- TODO: Currently this just tests server-side streaming of responses.
    -- We should emulate the Python example code more closely here.
    forM_ ["0", "1", "2"] $ \i ->
      send $ defMessage & #message .~ msg i
    return []
  where
    msg :: Text -> Text
    msg i = "Hello " <> req ^. #name <> " times " <> i

