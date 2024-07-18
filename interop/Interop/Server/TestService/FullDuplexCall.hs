module Interop.Server.TestService.FullDuplexCall (handle) where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server

import Interop.API
import Interop.Server.Common
import Interop.Server.TestService.StreamingOutputCall qualified as StreamingOutputCall

-- | Handle @TestService.FullDuplexCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#fullduplexcall>
handle :: Call FullDuplexCall -> IO ()
handle call = do
    trailers <- constructResponseMetadata call

    let handleRequest :: Proto StreamingOutputCallRequest -> IO ()
        handleRequest request = do
            StreamingOutputCall.handleRequest call request
            echoStatus (request ^. #responseStatus)

        loop :: IO ()
        loop = do
            streamElem <- recvInput call
            case streamElem of
              StreamElem  r   -> handleRequest r >> loop
              FinalElem   r _ -> handleRequest r
              NoMoreElems   _ -> return ()

    loop
    sendTrailers call trailers
