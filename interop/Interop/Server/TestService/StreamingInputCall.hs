{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Interop.Server.TestService.StreamingInputCall (handleStreamingInputCall) where

import Data.ByteString qualified as BS.Strict

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Spec

import Proto.Messages
import Proto.Test

import Interop.Server.Util

-- | Handle @TestService.StreamingInputCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#streaminginputcall>
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#client_compressed_streaming>
handleStreamingInputCall ::
     Call (Protobuf TestService "streamingInputCall")
  -> IO ()
handleStreamingInputCall call = do
    sz <- loop 0

    let response :: StreamingInputCallResponse
        response = defMessage & #aggregatedPayloadSize .~ fromIntegral sz

    sendFinalOutput call (response, [])
  where
    -- Returns the sum of all request payload bodies received.
    loop :: Int -> IO Int
    loop !acc = do
        streamElem <- recvInputWithEnvelope call
        case streamElem of
          StreamElem  r   -> handleRequest r >>= \sz -> loop (acc + sz)
          FinalElem   r _ -> handleRequest r >>= \sz -> return $ acc + sz
          NoMoreElems   _ -> return acc

    handleRequest :: (InboundEnvelope, StreamingInputCallRequest) -> IO Int
    handleRequest (envelope, request) = do
        checkInboundCompression expectCompressed envelope
        return $ BS.Strict.length (request ^. #payload ^. #body)
      where
        expectCompressed :: Bool
        expectCompressed = request ^. #expectCompressed ^. #value
