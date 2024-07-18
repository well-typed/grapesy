{-# LANGUAGE OverloadedLabels #-}

module Demo.Server.Service.Ping (handlers) where

import Network.GRPC.Common.Protobuf
import Network.GRPC.Server.StreamType

import Proto.API.Ping

handlers :: Methods IO '[Ping]
handlers =
      Method (mkNonStreaming handlePing)
    $ NoMoreMethods

handlePing :: Proto PingMessage -> IO (Proto PongMessage)
handlePing ping = return $ defMessage & #id .~ (ping ^. #id)

