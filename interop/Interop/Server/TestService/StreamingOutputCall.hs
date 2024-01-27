{-# LANGUAGE OverloadedLabels #-}

module Interop.Server.TestService.StreamingOutputCall (handleStreamingOutputCall) where

import Control.Concurrent
import Control.Lens ((.~), (^.))
import Control.Monad
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()

import Proto.Src.Proto.Grpc.Testing.Messages

import Interop.Server.Util

-- | Handle @TestService.StreamingOutputCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#streamingoutputcall>
handleStreamingOutputCall ::
     StreamingOutputCallRequest
  -> (StreamingOutputCallResponse -> IO ())
  -> IO ()
handleStreamingOutputCall request sendResponse =
    forM_ (request ^. #responseParameters) $ \responseParameters -> do
      let size, intervalUs :: Int
          size       = fromIntegral $ responseParameters ^. #size
          intervalUs = fromIntegral $ responseParameters ^. #intervalUs

          -- TODO: grapesy currently does not support setting compression
          -- on a per-response basis
          _compressed :: Bool
          _compressed = responseParameters ^. #compressed ^. #value

      payload <- mkPayload COMPRESSABLE size
      sendResponse $ defMessage & #payload .~ payload
      threadDelay intervalUs

