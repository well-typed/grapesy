module Interop.Server.TestService.FullDuplexCall (handleFullDuplexCall) where

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
    trailingMetadata <- constructResponseMetadata call
    loop
    sendTrailers call trailingMetadata
  where
    loop :: IO ()
    loop = do
        streamElem <- recvInput call
        case streamElem of
          StreamElem  r   -> handleStreamingOutputCall r sendResponse >> loop
          FinalElem   r _ -> handleStreamingOutputCall r sendResponse
          NoMoreElems   _ -> return ()

    sendResponse :: StreamingOutputCallResponse -> IO ()
    sendResponse = sendOutput call . StreamElem
