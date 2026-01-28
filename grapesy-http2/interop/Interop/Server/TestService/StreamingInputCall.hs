{-# LANGUAGE OverloadedStrings #-}

module Interop.Server.TestService.StreamingInputCall (handle) where

import Data.ByteString qualified as BS.Strict

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server

import Interop.Server.Common

import Proto.API.Interop

-- | Handle @TestService.StreamingInputCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#streaminginputcall>
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#client_compressed_streaming>
handle :: Call StreamingInputCall -> IO ()
handle call = do
    sz <- loop 0

    let response :: Proto StreamingInputCallResponse
        response = defMessage & #aggregatedPayloadSize .~ fromIntegral sz

    sendFinalOutput call (response, def)
  where
    -- Returns the sum of all request payload bodies received.
    loop :: Int -> IO Int
    loop !acc = do
        streamElem <- recvInputWithMeta call
        case streamElem of
          StreamElem  r   -> handleRequest r >>= \sz -> loop (acc + sz)
          FinalElem   r _ -> handleRequest r >>= \sz -> return $ acc + sz
          NoMoreElems   _ -> return acc

    handleRequest :: (InboundMeta, Proto StreamingInputCallRequest) -> IO Int
    handleRequest (meta, request) = do
        checkInboundCompression expectCompressed meta
        return $ BS.Strict.length (request ^. #payload ^. #body)
      where
        expectCompressed :: Bool
        expectCompressed = request ^. #expectCompressed ^. #value
