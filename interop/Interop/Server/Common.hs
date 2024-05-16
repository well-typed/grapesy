{-# LANGUAGE OverloadedStrings #-}

-- | Functionality required by many handlers
module Interop.Server.Common (
    constructResponseMetadata
  , echoStatus
  , checkInboundCompression
  ) where

import Control.Exception

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
constructResponseMetadata ::
     Call (Protobuf TestService rpc)
  -> IO InteropRespTrailMeta
constructResponseMetadata call = do
    requestMetadata <- getRequestMetadata call

    let initMeta  :: InteropRespInitMeta
        trailMeta :: InteropRespTrailMeta

        initMeta  = InteropRespInitMeta  $ interopExpectInit  requestMetadata
        trailMeta = InteropRespTrailMeta $ interopExpectTrail requestMetadata

    -- Send initial metadata
    setResponseInitialMetadata call initMeta
    _initiated <- initiateResponse call

    -- Return the final metadata to be sent at the end of the call
    return trailMeta
  where

-- | Echo any non-OK status back to the client
--
-- Does nothing if @code@ is set to @0@ ('GrpcOk').
--
-- See <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#status_code_and_message>
echoStatus :: Proto EchoStatus -> IO ()
echoStatus status =
    case toGrpcStatus code of
      Just GrpcOk ->
        return ()
      Just (GrpcError err) ->
        throwIO $ GrpcException {
            grpcError         = err
          , grpcErrorMessage  = Just $ status ^. #message
          , grpcErrorMetadata = []
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
