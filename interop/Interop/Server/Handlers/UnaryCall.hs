{-# LANGUAGE OverloadedLabels #-}

module Interop.Server.Handlers.UnaryCall (handleUnaryCall) where

import Control.Lens ((.~), (^.))
import Data.ByteString qualified as BS.Strict
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()

import Proto.Src.Proto.Grpc.Testing.Messages

import Interop.Server.Util

-- | Handle @TestService.UnaryCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#unarycall>
handleUnaryCall :: SimpleRequest -> IO SimpleResponse
handleUnaryCall request =
    case responseType of
      COMPRESSABLE -> do
        let payload :: Payload
            payload =
                defMessage
                  & #type' .~ responseType
                  & #body  .~ BS.Strict.pack (replicate responseSize 0)

            response :: SimpleResponse
            response =
                defMessage
                  & #payload .~ payload

        return response

      PayloadType'Unrecognized x ->
        throwUnrecognized "response_type" x
  where
    responseType :: PayloadType
    responseType = request ^. #responseType

    responseSize :: Int
    responseSize = fromIntegral $ request ^. #responseSize
