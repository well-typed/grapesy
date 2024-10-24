{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo.Server.Service.Greeter (handlers) where

import Control.Exception
import Control.Monad
import Data.Text (Text)

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.StreamType

import Proto.API.Helloworld

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

handlers :: Methods IO (ProtobufMethodsOf Greeter)
handlers =
      Method (mkNonStreaming sayHello)
    $ RawMethod sayHelloBidiStream
    $ RawMethod sayHelloStreamReply
    $ NoMoreMethods

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

sayHello :: Proto HelloRequest -> IO (Proto HelloReply)
sayHello req = return $ defMessage & #message .~ msg
  where
    msg :: Text
    msg = "Hello, " <> req ^. #name <> "!"

sayHelloStreamReply :: RpcHandler IO SayHelloStreamReply
sayHelloStreamReply = mkRpcHandlerNoDefMetadata $ \call -> do
    setResponseInitialMetadata call $ SayHelloMetadata (Just "initial-md-value")

    -- The client expects the metadata well before the first output
    initiateResponse call

    req <- recvFinalInput call

    let msg :: Text -> Text
        msg i = "Hello " <> req ^. #name <> " times " <> i

    forM_ ["0", "1", "2"] $ \i ->
      sendNextOutput call $ defMessage & #message .~ msg i

    sendTrailers call def

sayHelloBidiStream :: RpcHandler IO (Protobuf Greeter "sayHelloBidiStream")
sayHelloBidiStream = mkRpcHandler $ \call -> do
    let loop :: IO ()
        loop = do
          mReq <- recvInput call
          case mReq of
            StreamElem req   -> handleRequest call req >> loop
            FinalElem  req _ -> handleRequest call req
            NoMoreElems    _ -> return ()

    handle cancellation $ loop
  where
    handleRequest :: Call SayHelloBidiStream -> Proto HelloRequest -> IO ()
    handleRequest call req =
        sendNextOutput call $ defMessage & #message .~ msg
      where
        msg :: Text
        msg = req ^. #name <> " Ack"

    cancellation :: ClientDisconnected -> IO ()
    cancellation _ = putStrLn "sayHelloBidiStream server: cancelled."

