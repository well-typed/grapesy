{-# LANGUAGE OverloadedLabels #-}

module Interop.Server.TestService.UnaryCall (handleUnaryCall) where

import Control.Lens ((.~), (^.))
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()

import Proto.Src.Proto.Grpc.Testing.Messages

import Interop.Server.Util

-- | Handle @TestService.UnaryCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#unarycall>
handleUnaryCall :: SimpleRequest -> IO SimpleResponse
handleUnaryCall request = do
    payload <- mkPayload (request ^. #responseType) (request ^. #responseSize)
    return $ defMessage & #payload .~ payload
