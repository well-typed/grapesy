module Interop.Client.Connect (testServer) where

import Network.GRPC.Client
import Network.GRPC.Common

import Interop.Cmdline

-- | Test server to connect to
--
-- The spec says
--
-- > Clients must not disable certificate checking.
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md>
testServer :: Cmdline -> Server
testServer cmdline
  | cmdUseTLS cmdline
  = ServerSecure
          (ValidateServer certStore)
          SslKeyLogFromEnv
          serverAddress

  | otherwise
  = ServerInsecure serverAddress
  where
    certStore :: CertificateStoreSpec
    certStore
      | cmdUseTestCA cmdline
      = mconcat [
            certStoreFromSystem
          , certStoreFromPath $ cmdRootCA cmdline
          ]

      | otherwise
      = certStoreFromSystem

    serverAddress :: Address
    serverAddress = Address {
          addressHost      = cmdHost cmdline
        , addressPort      = cmdPort cmdline
        , addressAuthority = cmdServerHostOverride cmdline
        }
