{-# LANGUAGE OverloadedLabels #-}

module Interop.Server.TestService.StreamingInputCall (handleStreamingInputCall) where

import Control.Lens ((^.), (.~))
import Data.ByteString qualified as BS.Strict
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()

import Network.GRPC.Common

import Proto.Src.Proto.Grpc.Testing.Messages

-- | Handle @TestService.StreamingInputCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#streaminginputcall>
handleStreamingInputCall ::
     IO (StreamElem NoMetadata StreamingInputCallRequest)
  -> IO StreamingInputCallResponse
handleStreamingInputCall getRequest = do
    sz <- loop 0
    return $ defMessage & #aggregatedPayloadSize .~ fromIntegral sz
  where
    -- Returns the sum of all request payload bodies received.
    loop :: Int -> IO Int
    loop !acc = do
        streamElem <- getRequest
        case streamElem of
          StreamElem  r   -> handleRequest r >>= \sz -> loop (acc + sz)
          FinalElem   r _ -> handleRequest r >>= \sz -> return $ acc + sz
          NoMoreElems   _ -> return acc

    handleRequest :: StreamingInputCallRequest -> IO Int
    handleRequest request =
        return $ BS.Strict.length (request ^. #payload ^. #body)
      where
        -- TODO: grapesy does not currently make it possible to inspect
        -- whether a message is compressed or not.
        _expectCompressed :: Bool
        _expectCompressed = request ^. #expectCompressed ^. #value
