module Interop.Client.Connect (connect) where

import Network.GRPC.Client
import Network.GRPC.Common

import Interop.Cmdline

connect :: Cmdline -> (Connection -> IO a) -> IO a
connect cmdline = withConnection params server
  where
    params :: ConnParams
    params = def

    server :: Server
    server
      | cmdUseTLS cmdline
      = ServerSecure NoServerValidation SslKeyLogFromEnv serverAddress

      | otherwise
      = ServerInsecure serverAddress

    serverAddress :: Address
    serverAddress = Address {
          addressHost      = cmdHost cmdline
        , addressPort      = fromIntegral $ cmdPort cmdline
        , addressAuthority = Nothing
        }
