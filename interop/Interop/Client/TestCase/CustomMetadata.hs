{-# LANGUAGE OverloadedStrings #-}

module Interop.Client.TestCase.CustomMetadata (runTest) where

import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Interop.Client.Common
import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

import Proto.API.Interop

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
        responseMetadata <- recvResponseInitialMetadata call
        (_, trailers)    <- recvFinalOutput call
        verifyRespMetadata responseMetadata trailers

      -- 2. FullDuplexCall
      withRPC conn callParams (Proxy @FullDuplexCall) $ \call -> do
        sendFinalInput call streamingRequest
        responseMetadata <- recvResponseInitialMetadata call
        (_, trailers) <- recvFinalOutput call
        verifyRespMetadata responseMetadata trailers
  where
    callParams :: forall meth. CallParams (Protobuf TestService meth)
    callParams = def {
          callRequestMetadata = InteropReqMeta {
              interopExpectInit  = Just expectInitVal
            , interopExpectTrail = Just expectTrailVal
            }
        }

    simpleRequest :: Proto SimpleRequest
    simpleRequest =
        mkSimpleRequest False

    streamingRequest :: Proto StreamingOutputCallRequest
    streamingRequest =
        mkStreamingOutputCallRequest [(False, 314159)] (Just 271828)

verifyRespMetadata ::
     ResponseMetadata (Protobuf TestService meth)
     -- ^ This /could/ be trailing (if the server responded with Trailers-Only)
  -> ResponseTrailingMetadata (Protobuf TestService meth)
  -> IO ()
verifyRespMetadata initMeta trailMeta = do
    assertEqual initMeta $
      ResponseInitialMetadata $ InteropRespInitMeta (Just expectInitVal)
    assertEqual trailMeta $
      InteropRespTrailMeta (Just expectTrailVal)

expectInitVal, expectTrailVal :: Strict.ByteString
expectInitVal  = "test_initial_metadata_value"
expectTrailVal = BS.Strict.pack [0xab, 0xab, 0xab]
