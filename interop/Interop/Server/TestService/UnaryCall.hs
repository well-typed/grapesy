{-# LANGUAGE OverloadedStrings #-}

module Interop.Server.TestService.UnaryCall (handle) where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server
import Network.GRPC.Spec

import Interop.Server.Common
import Interop.Util.Messages

import Proto.API.Interop

-- | Handle @TestService.UnaryCall@
--
-- This is not implemented using the Protobuf communication patterns because
-- we need to deal with metadata.
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#unarycall>
handle :: Call UnaryCall -> IO ()
handle call = do
    -- Send initial metadata, hold on the trailers
    trailers <- constructResponseMetadata call

    -- Wait for the request
    (inboundEnvelope, request) <- do
      streamElem <- recvInputWithEnvelope call
      case StreamElem.value streamElem of
        Nothing -> fail "Expected element"
        Just x  -> return x

    -- Check compression of the input
    -- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#client_compressed_unary>
    let expectCompressed :: Bool
        expectCompressed = request ^. #expectCompressed . #value
    checkInboundCompression expectCompressed inboundEnvelope

    -- Send response
    payload <- payloadOfType (request ^. #responseType) (request ^. #responseSize)

    let outboundEnvelope :: OutboundEnvelope
        outboundEnvelope = def {
              outboundEnableCompression =
                request ^. #responseCompressed . #value
            }

        response :: Proto SimpleResponse
        response = defMessage & #payload .~ payload

    sendOutputWithEnvelope call $ StreamElem (outboundEnvelope, response)

    -- Send status and trailers
    echoStatus (request ^. #responseStatus)
    sendTrailers call trailers
