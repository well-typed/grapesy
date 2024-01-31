{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Interop.Server.TestService.UnaryCall (handleUnaryCall) where

import Control.Lens ((.~), (^.))
import Data.Default
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()

import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server
import Network.GRPC.Spec

import Proto.Src.Proto.Grpc.Testing.Messages
import Proto.Src.Proto.Grpc.Testing.Test

import Interop.Server.Util

-- | Handle @TestService.UnaryCall@
--
-- This is not implemented using the Protobuf communication patterns because
-- we need to deal with metadata.
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#unarycall>
handleUnaryCall :: Call (Protobuf TestService "unaryCall") -> IO ()
handleUnaryCall call = do
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
    payload <- mkPayload (request ^. #responseType) (request ^. #responseSize)

    let outboundEnvelope :: OutboundEnvelope
        outboundEnvelope = def {
              outboundEnableCompression =
                request ^. #responseCompressed . #value
            }

        response :: SimpleResponse
        response = defMessage & #payload .~ payload

    sendOutputWithEnvelope call $ StreamElem (outboundEnvelope, response)

    -- Send status and trailers
    echoStatus (request ^. #responseStatus) trailers
    sendTrailers call trailers
