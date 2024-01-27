module Interop.Server.TestService.FullDuplexCall (handleFullDuplexCall) where

import Network.GRPC.Common

import Proto.Src.Proto.Grpc.Testing.Messages

import Interop.Server.TestService.StreamingOutputCall

-- | Handle @TestService.FullDuplexCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#fullduplexcall>
handleFullDuplexCall ::
     IO (StreamElem NoMetadata StreamingOutputCallRequest)
  -> (StreamingOutputCallResponse -> IO ())
  -> IO ()
handleFullDuplexCall getRequest sendResponse =
    loop
  where
    loop :: IO ()
    loop = do
        streamElem <- getRequest
        case streamElem of
          StreamElem  r   -> handleStreamingOutputCall r sendResponse >> loop
          FinalElem   r _ -> handleStreamingOutputCall r sendResponse
          NoMoreElems   _ -> return ()
