module Interop.Server.TestService.StreamingOutputCall (
    handle
  , handleRequest
  ) where

import Control.Concurrent
import Control.Monad

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Spec

import Interop.API
import Interop.Util.Messages

-- | Handle @TestService.StreamingOutputCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#streamingoutputcall>
handle :: Call StreamingOutputCall -> IO ()
handle call = do
    request <- recvFinalInput call
    handleRequest call request
    sendTrailers call def

-- | Handle specific request
--
-- Abstracted out because also used in the @fullDuplexCall@ test.
handleRequest :: forall rpc.
     (Output rpc ~ Proto StreamingOutputCallResponse)
  => Call rpc
  -> Proto StreamingOutputCallRequest
  -> IO ()
handleRequest call request =
    forM_ (request ^. #responseParameters) $ \responseParameters -> do
      let size, intervalUs :: Int
          size       = fromIntegral $ responseParameters ^. #size
          intervalUs = fromIntegral $ responseParameters ^. #intervalUs

          shouldCompress :: Bool
          shouldCompress = responseParameters ^. #compressed ^. #value

      payload <- payloadOfType (Proto COMPRESSABLE) size

      let envelope :: OutboundEnvelope
          envelope = def { outboundEnableCompression = shouldCompress }

          response :: Proto StreamingOutputCallResponse
          response = defMessage & #payload .~ payload

      sendOutputWithEnvelope call $ StreamElem (envelope, response)
      threadDelay intervalUs

