{-# LANGUAGE OverloadedStrings #-}

-- | Functionality required by many handlers
module Interop.Server.Common (
    constructResponseMetadata
  , echoStatus
  , checkInboundCompression
  ) where

import Control.Exception
import Data.List (find)
import Data.Maybe (maybeToList)

import Network.GRPC.Common
import Network.GRPC.Server
import Network.GRPC.Spec

import Interop.API
import Interop.Util.Exceptions

{-------------------------------------------------------------------------------
  Dealing with the test-suite's message types
-------------------------------------------------------------------------------}

-- | Construct response metadata
--
-- Sends the initial response metadata now, and returns the trailing metadata.
--
-- See <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#custom_metadata>
constructResponseMetadata :: Call rpc -> IO [CustomMetadata]
constructResponseMetadata call = do
    requestMetadata <- getRequestMetadata call

    let initialResponseMetadata = maybeToList $
          find ((== nameMetadataInitial) . customMetadataName) requestMetadata
        trailingResponseMetadata = maybeToList $
          find ((== nameMetadataTrailing) . customMetadataName) requestMetadata

    -- Send initial metadata
    setResponseInitialMetadata call initialResponseMetadata
    _initiated <- initiateResponse call

    -- Return the final metadata to be sent at the end of the call
    return trailingResponseMetadata
  where
    nameMetadataInitial, nameMetadataTrailing :: HeaderName
    nameMetadataInitial  = AsciiHeader  "x-grpc-test-echo-initial"
    nameMetadataTrailing = BinaryHeader "x-grpc-test-echo-trailing-bin"

-- | Echo any non-OK status back to the client
--
-- Does nothing if @code@ is set to @0@ ('GrpcOk').
--
-- See <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#status_code_and_message>
echoStatus :: EchoStatus -> [CustomMetadata] -> IO ()
echoStatus status trailers =
    case toGrpcStatus code of
      Just GrpcOk ->
        return ()
      Just (GrpcError err) ->
        throwIO $ GrpcException {
            grpcError         = err
          , grpcErrorMessage  = Just $ status ^. #message
          , grpcErrorMetadata = trailers
          }
      Nothing ->
        assertUnrecognized code
  where
    code :: Word
    code = fromIntegral $ status ^. #code

checkInboundCompression :: Bool -> InboundEnvelope -> IO ()
checkInboundCompression expectCompressed envelope =
    case (expectCompressed, inboundCompressedSize envelope) of
      (True, Just _) ->
        return ()
      (False, Nothing) ->
        return ()
      (True, Nothing) ->
        throwIO GrpcException {
            grpcError         = GrpcInvalidArgument
          , grpcErrorMessage  = Just "Expected compressed message"
          , grpcErrorMetadata = []
          }
      (False, Just _) ->
        throwIO GrpcException {
            grpcError         = GrpcInvalidArgument
          , grpcErrorMessage  = Just "Expected uncompressed message"
          , grpcErrorMetadata = []
          }
