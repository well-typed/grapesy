{-# LANGUAGE OverloadedStrings #-}

module Interop.Server.TestService.StreamingInputCall (handle) where

import Data.ByteString qualified as BS.Strict

import Network.GRPC.Common
import Network.GRPC.Server
import Network.GRPC.Spec

import Interop.API
import Interop.Server.Common

-- | Handle @TestService.StreamingInputCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#streaminginputcall>
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#client_compressed_streaming>
handle :: Call StreamingInputCall -> IO ()
handle call = do
    sz <- loop 0

    let response :: StreamingInputCallResponse
        response = defMessage & #aggregatedPayloadSize .~ fromIntegral sz

    sendFinalOutput call (response, def)
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
