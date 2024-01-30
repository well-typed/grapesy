{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Interop.Server.Util (
    -- * Errors
    throwUnrecognized
    -- * Dealing with the test-suite's message types
  , mkPayload
  , clearPayload
  , constructResponseMetadata
  , echoStatus
  ) where

import Control.Exception
import Control.Lens ((&), (.~), (^.))
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ProtoLens
import Data.ProtoLens.Field (HasField)
import Data.ProtoLens.Labels ()
import Data.Text qualified as Text

import Network.GRPC.Common
import Network.GRPC.Server
import Network.GRPC.Spec

import Proto.Src.Proto.Grpc.Testing.Messages

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

throwUnrecognized :: forall a x. Show a => String -> a -> IO x
throwUnrecognized field value =
    throwIO $ GrpcException {
        grpcError         = GrpcInvalidArgument
      , grpcErrorMetadata = []
      , grpcErrorMessage  = Just . Text.pack $ concat [
           "Unrecognized "
         , show field
         , ": "
         , show value
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

clearPayload ::
     ( HasField a "payload" b
     , HasField b "body" Strict.ByteString
     )
  => a -> a
clearPayload x = x & #payload . #body .~ BS.Strict.empty

-- | Construct response metadata
--
-- Sends the initial response metadata now, and returns the trailing metadata.
--
-- See <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#custom_metadata>
constructResponseMetadata :: Call rpc -> IO [CustomMetadata]
constructResponseMetadata call = do
    requestMetadata <- getRequestMetadata call
    initialResponseMetadata <-
      case lookupCustomMetadata nameMetadataInitial requestMetadata of
        Nothing ->
          return []
        Just (Left binaryValue) ->
          throwUnrecognized (show nameMetadataInitial) binaryValue
        Just (Right asciiValue) ->
          return [AsciiHeader nameMetadataInitial asciiValue]
    trailingResponseMetadata <-
      case lookupCustomMetadata nameMetadataTrailing requestMetadata of
        Nothing ->
          return []
        Just (Left binaryValue) ->
          return [BinaryHeader nameMetadataTrailing binaryValue]
        Just (Right asciiValue) ->
          throwUnrecognized (show nameMetadataTrailing) asciiValue

    -- Send initial metadata
    setResponseMetadata call initialResponseMetadata
    _initiated <- initiateResponse call

    -- Return the final metadata to be sent at the end of the call
    return trailingResponseMetadata
  where
    -- NOTE: grapesy strips/adds the @-bin@ suffix automatically
    nameMetadataInitial, nameMetadataTrailing :: HeaderName
    nameMetadataInitial  = "x-grpc-test-echo-initial"
    nameMetadataTrailing = "x-grpc-test-echo-trailing"

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
        throwIO $ GrpcException err (Just $ status ^. #message) trailers
      Nothing ->
        throwUnrecognized "code" code
  where
    code :: Word
    code = fromIntegral $ status ^. #code
