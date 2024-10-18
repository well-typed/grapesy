-- | Functionality required by many test cases
module Interop.Client.Common (
    -- * Setup
    enableInitCompression
    -- * Construct server inputs
  , mkSimpleRequest
  , mkStreamingInputCallRequest
  , mkStreamingOutputCallRequest
    -- * Verify server outputs
  , verifySimpleResponse
  , verifyStreamingOutputCallResponse
  , verifyStreamingOutputs
  , expectInvalidArgument
  ) where


import Data.ByteString qualified as BS.Strict

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Protobuf

import Interop.Util.Exceptions
import Interop.Util.Messages

import Proto.API.Interop

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

-- | Enable compression of the first message
--
-- Some test specifications (@client_compressed_unary@,
-- @client_compressed_streaming@) tell us to
--
-- 1. First send a request that causes the server to send an error response
--    (which will use the gRPC Trailers-Only case)
-- 2. Then send a compressed message
-- 3. Finally, send an uncompressed message
--
-- This is problematic: we don't know which compression algorithms the server
-- supports until we have had a successful response from the server. We could
-- fix this by swapping (2) and (3), but this would be deviating from the test
-- specification. Instead, we override the initial compression to use, and
-- report the test as skipped if the server comes back with 'GrpcUnimplemented'.
enableInitCompression :: ConnParams
enableInitCompression = def {
      connInitCompression = Just Compr.gzip
    }

{-------------------------------------------------------------------------------
  Construct server inputs
-------------------------------------------------------------------------------}

mkSimpleRequest ::
     Bool  -- ^ Should the server expect the message to be compressed?
  -> Proto SimpleRequest
mkSimpleRequest expectCompressed =
    defMessage
      & #expectCompressed .~ boolValue expectCompressed
      & #responseSize     .~ 314159
      & #payload          .~ payloadOfZeroes 271828

mkStreamingInputCallRequest ::
     Bool  -- ^ Should the server expect the message to be compressed?
  -> Int   -- ^ Payload size
  -> Proto StreamingInputCallRequest
mkStreamingInputCallRequest expectCompressed size =
    defMessage
      & #expectCompressed .~ boolValue expectCompressed
      & #payload          .~ payloadOfZeroes size

mkStreamingOutputCallRequest ::
     [(Bool, Int)] -- ^ Size and compression of each expected response
  -> Maybe Int     -- ^ Size of the payload, if any
  -> Proto StreamingOutputCallRequest
mkStreamingOutputCallRequest expectedSizes payload =
    defMessage
      & #responseParameters .~ [
             defMessage
               & #size       .~ fromIntegral sz
               & #compressed .~ boolValue compressed
           | (compressed, sz) <- expectedSizes
           ]
      & ( case payload of
            Nothing -> id
            Just sz -> #payload .~ payloadOfZeroes sz
        )

{-------------------------------------------------------------------------------
  Verify server outputs
-------------------------------------------------------------------------------}

verifySimpleResponse :: HasCallStack => Proto SimpleResponse -> IO ()
verifySimpleResponse resp = do
    assertEqual 314159 $ BS.Strict.length     (resp ^. #payload . #body)
    assertBool         $ BS.Strict.all (== 0) (resp ^. #payload . #body)

verifyStreamingOutputCallResponse ::
     Int -- ^ Expected size
  -> Proto StreamingOutputCallResponse -> IO ()
verifyStreamingOutputCallResponse expectedSize resp = do
    assertEqual expectedSize $ BS.Strict.length     (resp ^. #payload . #body)
    assertBool               $ BS.Strict.all (== 0) (resp ^. #payload . #body)

verifyStreamingOutputs :: forall rpc.
     HasCallStack
  => Call rpc
  -> (ProperTrailers' -> IO ())            -- ^ Verify trailers
  -> [(InboundMeta, Output rpc) -> IO ()]  -- ^ Verifier per expected output
  -> IO ()
verifyStreamingOutputs call verifyTrailers = go
  where
    go :: [(InboundMeta, Output rpc) -> IO ()] -> IO ()
    go verifiers = do
        mResp <- recvOutputWithMeta call
        case (mResp, verifiers) of
          (NoMoreElems trailers, [])     -> verifyTrailers trailers
          (StreamElem{}, [])             -> assertFailure "Too many outputs"
          (FinalElem{}, [])              -> assertFailure "Too many outputs"
          (NoMoreElems{}, _:_)           -> assertFailure "Not enough outputs"
          (FinalElem{}, _:_:_)           -> assertFailure "Not enough outputs"
          (StreamElem resp, v:vs)        -> v resp >> go vs
          (FinalElem resp trailers, [v]) -> v resp >> verifyTrailers trailers

-- | Expect the server to respond with 'InvalidArgument'
--
-- Throws 'TestSkipped' if the server responds with normal response instead
-- (indicating that the server was unable to perform the required check on
-- the inputs we sent it).
--
-- Also throws 'TestSkipped' is the server returns with 'GrpcUnimplemented'.
expectInvalidArgument :: IO a -> IO ()
expectInvalidArgument = assertThrows $ \exception ->
    case grpcError exception of
      GrpcInvalidArgument ->
        return ()
      GrpcUnimplemented ->
        throwIO $ TestSkipped "Server sent UNIMPLEMENTED"
      err ->
        assertFailure $ "Unexpected failure " ++ show err