{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Interop.Server.Util (
    -- * Errors
    throwUnrecognized
    -- * Dealing with the test-suite's message types
  , mkPayload
  ) where

import Control.Exception
import Control.Lens ((&), (.~))
import Data.ByteString qualified as BS.Strict
import Data.ProtoLens
import Data.ProtoLens.Labels ()
import Data.Text qualified as Text

import Network.GRPC.Common

import Proto.Src.Proto.Grpc.Testing.Messages

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

throwUnrecognized :: Show a => String -> a -> IO x
throwUnrecognized field value =
    throwIO $ GrpcException {
        grpcError         = GrpcInvalidArgument
      , grpcErrorMetadata = []
      , grpcErrorMessage  = Just $ mconcat [
           "Unrecognized "
         , Text.pack field
         , ": "
         , Text.pack (show value)
         ]
      }

{-------------------------------------------------------------------------------
  Dealing with the test-suite's message types
-------------------------------------------------------------------------------}

mkPayload :: Integral size => PayloadType -> size -> IO Payload
mkPayload type' size = do
    body <-
      case type' of
        COMPRESSABLE ->
          return $ BS.Strict.pack (replicate (fromIntegral size) 0)
        PayloadType'Unrecognized x ->
          throwUnrecognized "PayloadType" x
    return $
      defMessage
        & #type' .~ type'
        & #body  .~ body
