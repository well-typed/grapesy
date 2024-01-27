{-# LANGUAGE OverloadedLabels #-}

module Interop.Server.PingService.Ping (handlePing) where

import Control.Lens ((.~), (^.))
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()

import Proto.Ping

-- | Handle @PingService.Ping@
--
-- This is not part of the gRPC interop tests, but an internal debugging tool.
handlePing :: PingMessage -> IO PongMessage
handlePing ping = return $ defMessage & #id .~ (ping ^. #id)
