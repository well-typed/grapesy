module Demo.Client.API.StreamType.IO.Ping (ping) where

import Data.ByteString.Lazy.Char8 qualified as Lazy

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO

import Proto.API.Ping

ping :: Connection -> Lazy.ByteString -> IO ()
ping conn msg = do
    pong <- nonStreaming conn (rpc @Ping) msg
    Lazy.putStrLn pong
