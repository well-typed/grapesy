{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Interop.Server.TestService.UnaryCall (handleUnaryCall) where

import Control.Lens ((.~), (^.))
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()

import Network.GRPC.Server

import Proto.Src.Proto.Grpc.Testing.Test

import Interop.Server.Util

-- | Handle @TestService.UnaryCall@
--
-- This is not implemented using the Protobuf communication patterns because
-- we need to deal with metadata.
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#unarycall>
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#custom_metadata>
handleUnaryCall :: Call (Protobuf TestService "unaryCall") -> IO ()
handleUnaryCall call = do
    trailingMetadata <- constructResponseMetadata call
    request <- recvFinalInput call
    payload <- mkPayload (request ^. #responseType) (request ^. #responseSize)
    sendFinalOutput call (defMessage & #payload .~ payload, trailingMetadata)
