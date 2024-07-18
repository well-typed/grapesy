{-# LANGUAGE OverloadedStrings #-}

module Interop.Client.TestCase.ClientCompressedUnary (runTest) where

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Spec

import Interop.Client.Common
import Interop.Client.Connect
import Interop.Cmdline

import Proto.API.Interop

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#client_compressed_unary>
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection enableInitCompression (testServer cmdline) $ \conn -> do
      -- 1. Call UnaryCall with the feature probe, an uncompressed message
      checkServerSupportsCompressedRequest conn

      -- 2. Call UnaryCall with the compressed message
      withRPC conn def (Proxy @UnaryCall) $ \call -> do
        sendInputWithEnvelope call $ FinalElem compressed NoMetadata
        (resp, _metadata) <- recvFinalOutput call
        verifySimpleResponse resp

      -- 3. Call UnaryCall with the uncompressed message
      withRPC conn def (Proxy @UnaryCall) $ \call -> do
        sendInputWithEnvelope call $ FinalElem uncompressed NoMetadata
        (resp, _metadata) <- recvFinalOutput call
        verifySimpleResponse resp
  where
    -- Expect compressed, and /is/ compressed
    compressed :: (OutboundEnvelope, Proto SimpleRequest)
    compressed = (
          def { outboundEnableCompression = True }
        , mkSimpleRequest True
        )

    -- Expect uncompressed, and /is/ uncompressed
    uncompressed :: (OutboundEnvelope, Proto SimpleRequest)
    uncompressed = (
          def { outboundEnableCompression = False }
        , mkSimpleRequest False
        )

-- | Check whether the server supports CompressedRequest
--
-- If it does not, throws 'TestSkipped'.
checkServerSupportsCompressedRequest :: Connection -> IO ()
checkServerSupportsCompressedRequest conn =
    withRPC conn def (Proxy @UnaryCall) $ \call -> do
      sendInputWithEnvelope call $ FinalElem featureProbe NoMetadata
      expectInvalidArgument $ recvFinalOutput call
  where
    -- Expect compressed, but is /not/ actually compressed
    featureProbe :: (OutboundEnvelope, Proto SimpleRequest)
    featureProbe = (
          def { outboundEnableCompression = False }
        , mkSimpleRequest True
        )

