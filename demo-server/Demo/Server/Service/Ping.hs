module Demo.Server.Service.Ping (handlers) where

import Data.ByteString.Lazy qualified as Lazy (ByteString)

import Network.GRPC.Server.StreamType

import Proto.API.Ping

handlers :: Methods IO '[Ping]
handlers =
      Method (mkNonStreaming handlePing)
    $ NoMoreMethods

handlePing :: Lazy.ByteString -> IO Lazy.ByteString
handlePing = return

