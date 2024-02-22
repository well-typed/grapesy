{-# LANGUAGE OverloadedStrings #-}

module Interop.Client.TestCase.SpecialStatusMessage (runTest) where

import Control.Exception
import Data.Text (Text)

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Spec

import Interop.API
import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#special_status_message>
runTest :: Cmdline -> IO ()
runTest cmdline = do
    withConnection def (testServer cmdline) $ \conn -> do
      withRPC conn def (Proxy @UnaryCall) $ \call -> do
        sendFinalInput call simpleRequest
        resp <- try $ recvFinalOutput call
        verifyResponse resp
  where
    simpleRequest :: SimpleRequest
    simpleRequest = defMessage & #responseStatus .~ echoStatus

    -- Spec mandates the use of code 2, which is 'GrpcUnknown'
    echoStatus :: EchoStatus
    echoStatus =
        defMessage
          & #code    .~ fromIntegral (fromGrpcStatus $ GrpcError GrpcUnknown)
          & #message .~ statusMessage

    statusMessage :: Text
    statusMessage =
        "\t\ntest with whitespace\r\nand Unicode BMP ☺ and non-BMP 😈\t\n"

    verifyResponse :: Either GrpcException a -> IO ()
    verifyResponse (Right _) =
        assertFailure "Expected UNKNOWN"
    verifyResponse (Left err) = do
        assertEqual GrpcUnknown          $ grpcError        err
        assertEqual (Just statusMessage) $ grpcErrorMessage err
