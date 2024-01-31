{-# LANGUAGE OverloadedLabels #-}

module Interop.Server.TestService.FullDuplexCall (handleFullDuplexCall) where

import Control.Lens ((^.))
import Data.ProtoLens.Labels ()

import Network.GRPC.Common
import Network.GRPC.Server

import Proto.Src.Proto.Grpc.Testing.Messages
import Proto.Src.Proto.Grpc.Testing.Test

import Interop.Server.TestService.StreamingOutputCall
import Interop.Server.Util

-- | Handle @TestService.FullDuplexCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#fullduplexcall>
handleFullDuplexCall :: Call (Protobuf TestService "fullDuplexCall") -> IO ()
handleFullDuplexCall call = do
    trailers <- constructResponseMetadata call

    let handleRequest :: StreamingOutputCallRequest -> IO ()
        handleRequest request = do
            handleStreamingOutputCallRequest call request
            echoStatus (request ^. #responseStatus) trailers

        loop :: IO ()
        loop = do
            streamElem <- recvInput call
            case streamElem of
              StreamElem  r   -> handleRequest r >> loop
              FinalElem   r _ -> handleRequest r
              NoMoreElems   _ -> return ()

    loop
    sendTrailers call trailers
