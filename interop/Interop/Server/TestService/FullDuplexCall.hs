{-# LANGUAGE OverloadedLabels #-}

module Interop.Server.TestService.FullDuplexCall (handleFullDuplexCall) where

import Control.Concurrent
import Control.Lens ((.~), (^.))
import Control.Monad
import Data.Function ((&))
import Data.ProtoLens
import Data.ProtoLens.Labels ()

import Network.GRPC.Common

import Proto.Src.Proto.Grpc.Testing.Messages

import Interop.Server.Util

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
          StreamElem  r   -> handleRequest r >> loop
          FinalElem   r _ -> handleRequest r
          NoMoreElems   _ -> return ()

    handleRequest :: StreamingOutputCallRequest -> IO ()
    handleRequest request =
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
