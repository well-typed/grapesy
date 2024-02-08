{-# LANGUAGE OverloadedLabels #-}

module Interop.Server.TestService.StreamingOutputCall (
    handleStreamingOutputCall
  , handleStreamingOutputCallRequest
  ) where

import Control.Concurrent
import Control.Monad

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Spec

import Proto.Src.Proto.Grpc.Testing.Messages
import Proto.Src.Proto.Grpc.Testing.Test

import Interop.Server.Util

-- | Handle @TestService.StreamingOutputCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#streamingoutputcall>
handleStreamingOutputCall ::
     Call (Protobuf TestService "streamingOutputCall")
  -> IO ()
handleStreamingOutputCall call = do
    request <- recvFinalInput call
    handleStreamingOutputCallRequest call request
    sendTrailers call []

-- | Handle specific request
--
-- Abstracted out because also used in the @fullDuplexCall@ test.
handleStreamingOutputCallRequest :: forall meth.
     (Output (Protobuf TestService meth) ~ StreamingOutputCallResponse)
  => Call (Protobuf TestService meth)
  -> StreamingOutputCallRequest
  -> IO ()
handleStreamingOutputCallRequest call request =
    forM_ (request ^. #responseParameters) $ \responseParameters -> do
      let size, intervalUs :: Int
          size       = fromIntegral $ responseParameters ^. #size
          intervalUs = fromIntegral $ responseParameters ^. #intervalUs

          shouldCompress :: Bool
          shouldCompress = responseParameters ^. #compressed ^. #value

      payload <- mkPayload COMPRESSABLE size

      let envelope :: OutboundEnvelope
          envelope = def { outboundEnableCompression = shouldCompress }

          response :: StreamingOutputCallResponse
          response = defMessage & #payload .~ payload

      sendOutputWithEnvelope call $ StreamElem (envelope, response)
      threadDelay intervalUs

