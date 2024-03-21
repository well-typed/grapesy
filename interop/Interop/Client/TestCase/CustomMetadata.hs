{-# LANGUAGE OverloadedStrings #-}

module Interop.Client.TestCase.CustomMetadata (runTest) where

import Data.ByteString qualified as BS.Strict

import Network.GRPC.Client
import Network.GRPC.Common

import Interop.API
import Interop.Client.Common
import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#custom_metadata>
--
-- For both UnaryCall and FullDuplexCall, the reference server (at least some)
-- does not return any initial metadata until we send the first request. The
-- test spec does not specify whether this is expected behaviour or not, so we
-- play it save and only ask for the initial metadata after sending the request.
runTest :: Cmdline -> IO ()
runTest cmdline = do
    withConnection def (testServer cmdline) $ \conn -> do
      -- 1. UnaryCall
      withRPC conn callParams (Proxy @UnaryCall) $ \call -> do
        -- For UnaryCall the server does not respond until we send the input
        sendFinalInput call simpleRequest
        responseMetadata <- recvResponseMetadata call
        (_, trailers)    <- recvFinalOutput call
        assertBool $ metadataInit  `elem` responseMetadata
        assertBool $ metadataFinal `elem` trailers

      -- 2. FullDuplexCall
      withRPC conn callParams (Proxy @FullDuplexCall) $ \call -> do
        sendFinalInput call streamingRequest
        responseMetadata <- recvResponseMetadata call
        (_, trailers) <- recvFinalOutput call
        assertBool $ metadataInit  `elem` responseMetadata
        assertBool $ metadataFinal `elem` trailers
  where
    callParams :: CallParams
    callParams = def {
          callRequestMetadata = [metadataInit, metadataFinal]
        }

    metadataInit, metadataFinal :: CustomMetadata
    metadataInit  = CustomMetadata
                      (AsciiHeader "x-grpc-test-echo-initial")
                      "test_initial_metadata_value"
    metadataFinal = CustomMetadata
                      (BinaryHeader "x-grpc-test-echo-trailing-bin")
                      (BS.Strict.pack [0xab, 0xab, 0xab])

    simpleRequest :: SimpleRequest
    simpleRequest =
        mkSimpleRequest False

    streamingRequest :: StreamingOutputCallRequest
    streamingRequest =
        mkStreamingOutputCallRequest [(False, 314159)] (Just 271828)

