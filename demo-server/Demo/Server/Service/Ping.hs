module Demo.Server.Service.Ping (handlers) where

import Data.ByteString.Lazy qualified as Lazy (ByteString)

import Network.GRPC.Common
import Network.GRPC.Common.StreamType
import Network.GRPC.Server.StreamType

import Demo.Common.API

handlers :: Methods IO '[Ping]
handlers =
      MethodOfStreamingType (Proxy @NonStreaming) (mkNonStreaming handlePing)
    $ NoMoreMethods

handlePing :: Lazy.ByteString -> IO Lazy.ByteString
handlePing = return

