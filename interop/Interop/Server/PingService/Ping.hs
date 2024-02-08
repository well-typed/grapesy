{-# LANGUAGE OverloadedLabels #-}

module Interop.Server.PingService.Ping (handlePing) where

import Network.GRPC.Common.Protobuf

import Proto.Ping

-- | Handle @PingService.Ping@
--
-- This is not part of the gRPC interop tests, but an internal debugging tool.
handlePing :: PingMessage -> IO PongMessage
handlePing ping = return $ defMessage & #id .~ (ping ^. #id)
