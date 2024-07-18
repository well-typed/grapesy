module Demo.Client.API.StreamType.IO.Ping (ping) where

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common.Protobuf

import Proto.API.Ping

ping :: Connection -> Proto PingMessage -> IO ()
ping conn msg = do
    pong <- nonStreaming conn (rpc @Ping) msg
    print pong
