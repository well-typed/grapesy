module Demo.Client.API.StreamType.IO.Ping (ping) where

import Data.ByteString.Lazy.Char8 qualified as Lazy

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO

import Demo.Common.API

ping :: Connection -> Lazy.ByteString -> IO ()
ping conn msg = Lazy.putStrLn =<< nonStreaming conn (rpc @Ping) msg
