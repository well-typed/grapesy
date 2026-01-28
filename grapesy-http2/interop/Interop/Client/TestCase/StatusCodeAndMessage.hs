{-# LANGUAGE OverloadedStrings #-}

module Interop.Client.TestCase.StatusCodeAndMessage (runTest) where

import Data.Text (Text)

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec (fromGrpcStatus)

import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

import Proto.API.Interop

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#status_code_and_message>
runTest :: Cmdline -> IO ()
runTest cmdline = do
    withConnection def (testServer cmdline) $ \conn -> do
      -- 1. UnaryCall
      withRPC conn def (Proxy @UnaryCall) $ \call -> do
        sendFinalInput call simpleRequest
        assertThrows verifyError $ recvFinalOutput call

      -- 2. FullDuplexCall
      withRPC conn def (Proxy @FullDuplexCall) $ \call -> do
        sendFinalInput call streamingRequest
        assertThrows verifyError $ recvFinalOutput call
  where
    simpleRequest :: Proto SimpleRequest
    simpleRequest = defMessage & #responseStatus .~ echoStatus

    streamingRequest :: Proto StreamingOutputCallRequest
    streamingRequest = defMessage & #responseStatus .~ echoStatus

    -- Spec mandates the use of code 2, which is 'GrpcUnknown'
    echoStatus :: Proto EchoStatus
    echoStatus =
        defMessage
          & #code    .~ fromIntegral (fromGrpcStatus $ GrpcError GrpcUnknown)
          & #message .~ statusMessage

    statusMessage :: Text
    statusMessage = "test status message"

    verifyError :: GrpcException -> IO ()
    verifyError err = do
        assertEqual GrpcUnknown          $ grpcError        err
        assertEqual (Just statusMessage) $ grpcErrorMessage err
