{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Interop.Server.Util (
    throwUnrecognized
  ) where

import Control.Exception
import Data.ProtoLens.Labels ()
import Data.Text qualified as Text

import Network.GRPC.Common

{-------------------------------------------------------------------------------
  Internal auxiliary
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

