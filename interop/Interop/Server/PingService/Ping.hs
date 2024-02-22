module Interop.Server.PingService.Ping (handle) where

import Network.GRPC.Common.Protobuf

import Proto.Ping

-- | Handle @PingService.Ping@
--
-- This is not part of the gRPC interop tests, but an internal debugging tool.
handle :: PingMessage -> IO PongMessage
handle ping = return $ defMessage & #id .~ (ping ^. #id)
