{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests for HTTP2 frames sent by the server after client sends RST_STREAM
--
-- Normally after a client sends RST_STREAM to a server, the server will not
-- send any more frames on that stream. However, some frames might already have
-- been put on the wire, or enqueued internally by the server, before it gets
-- the RST_STREAM, and so some frames might still arrive.
--
-- Such frames can be dropped, but only after their effect on the
-- connection-level state is taken into account:
--
-- * HPACK state must be updated for HEADERS and CONTINUATION frames
-- * Connection-level window size must be updated for DATA frames
--
-- In this module we use a scripted server ("Test.Util.FrameLevelServer") to
-- simulate some scenarios:
--
-- 1. Client opens a stream, sends its final input, then sends RST_STREAM,
--    after which the server responds with gRPC Trailers-Only headers. For
--    example, this can happen with a gRPC deadline on a unary call: the
--    deadline expires on both the client and the server at roughly the same
--    time, and the RST_STREAM/Trailers-Only cross each other.
-- 2. A variation on (1) where the server's trailers span two frames; the test
--    case is designed so that the two frames cannot be decoded separately but
--    must be decoded as one block.
-- 3. Variation on (1) where the server responds with an initial set of regular
--    headers (not trailers), which the client receives before sending its
--    final message and the RST_STREAM. This mimics cancellation after the
--    response has started, e.g. a deadline on a streaming call.
--
-- In all of these tests, the server is set up to explicitly wait for the
-- RST_STREAM before sending the frames under test; this is intended to mimic
-- the race condition described above, but in a deterministic way.
--
-- TODO: Also test for window size problems.
module Test.Sanity.FramesAfterReset (tests) where

import Control.Exception
import Data.Bits
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS.Lazy.Char8
import Data.String
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Binary

import Test.Util.FrameLevelServer (Script, Frame(..), FrameHeader(..))
import Test.Util.FrameLevelServer qualified as Frame

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "FramesAfterReset" [
      testCase "trailersOnlySingleFrame" $
        test_framesAfterReset cancelBeforeHeaders ignoreRequest sendTrailersOnly
    , testCase "trailersOnlyTwoFrames" $
        test_framesAfterReset cancelBeforeHeaders ignoreRequest sendTrailersOnlyTwoFrames
    , testCase "trailersAfterHeaders" $
        test_framesAfterReset cancelAfterHeaders respondWithHeaders sendTrailers
    ]

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

-- | Call 1 is cancelled, and the server sends a header block on stream 1 after
-- the reset. That block contains a dynamic table insertion, which call 2's
-- response references: it resolves only if the client decoded the late block.
test_framesAfterReset ::
     (Client.Call TestRpc -> IO ())  -- ^ Call 1 (leaving it sends RST_STREAM)
  -> Script ()                       -- ^ Server side of stream 1, before the reset
  -> Script ()                       -- ^ Late block on stream 1, after the reset
  -> Assertion
test_framesAfterReset call1 beforeReset sendLate =
    Frame.withScript Nothing (script beforeReset sendLate) $ \server getHandlerResults -> do
      trailers <- Client.withConnection def server $ \conn -> do
        expectCancelled $ Client.withRPC conn def (Proxy @TestRpc) call1
        Client.withRPC conn def (Proxy @TestRpc) $ \call -> do
          Client.sendFinalInput call BS.Lazy.empty
          Client.recvTrailers call
      assertBool "x-foo: bar in call 2's trailers" $
        CustomMetadata (fromString "x-foo") (BS.Strict.Char8.pack "bar") `elem` trailers
      results <- getHandlerResults
      assertEqual "handler results" [()] results

-- | Scenarios (1) and (2): send the final message, leave without reading
cancelBeforeHeaders :: Client.Call TestRpc -> IO ()
cancelBeforeHeaders call =
    Client.sendFinalInput call BS.Lazy.empty

-- | Scenario (3)
cancelAfterHeaders :: Client.Call TestRpc -> IO ()
cancelAfterHeaders call = do
    _ <- Client.recvResponseInitialMetadata call
    Client.sendFinalInput call BS.Lazy.empty

{-------------------------------------------------------------------------------
  Scripts
-------------------------------------------------------------------------------}

script :: Script () -> Script () -> Script ()
script beforeReset sendLate = do
    Frame.handshake
    beforeReset
    awaitResetAndRequest
    sendLate
    Frame.send $ Frame.mkFrame 0x1 0x5 3 probe  -- END_STREAM | END_HEADERS

-- | Wait for the reset of stream 1 and the end of the request on stream 3
--
-- These can arrive in either order: grapesy does not guarantee that call 1's
-- final message has reached http2 by the time the call is cancelled, and
-- depending on that, http2 sends the reset from one of two places. Either way
-- it removes the stream /before/ sending the reset, so once we have seen it,
-- the client has forgotten stream 1.
awaitResetAndRequest :: Script ()
awaitResetAndRequest = go False False
  where
    go :: Bool -> Bool -> Script ()
    go reset endOfRequest
      | reset && endOfRequest = return ()
      | otherwise = do
          (reset', endOfRequest') <- Frame.recv classify
          go (reset || reset') (endOfRequest || endOfRequest')

    classify :: Frame -> Either String (Bool, Bool)
    classify frame =
        case frameHeader frame of
          FrameHeader{frameType = 0x3, frameStreamId = 1} ->
            Right (True, False)
          FrameHeader{frameType, frameFlags, frameStreamId = 3}
            | frameType `elem` [0x0, 0x1] ->
            Right (False, testBit frameFlags 0)  -- END_STREAM
          _otherwise ->
            Left $ "Expected RST_STREAM on stream 1 or request on stream 3, got "
                ++ show frame

-- | Scenarios (1) and (2): skip the request on stream 1
ignoreRequest :: Script ()
ignoreRequest =
    Frame.ignore $ \frame ->
         Frame.defaultNoiseFilter frame
      || (   frameStreamId (frameHeader frame) == 1
          && frameType     (frameHeader frame) `elem` [0x0, 0x1]
         )

-- | Scenario (3): respond with initial headers, then skip the request message
respondWithHeaders :: Script ()
respondWithHeaders = do
    recvRequestHeaders 1
    Frame.send $ Frame.mkFrame 0x1 0x4 1 responseHeaders  -- END_HEADERS
    Frame.ignore $ \frame ->
         Frame.defaultNoiseFilter frame
      || (   frameStreamId (frameHeader frame) == 1
          && frameType     (frameHeader frame) == 0x0
         )

sendTrailersOnly :: Script ()
sendTrailersOnly =
    Frame.send $ Frame.mkFrame 0x1 0x5 1 lateTrailersOnly  -- END_STREAM | END_HEADERS

-- | Split inside the @x-foo@ literal, so that neither fragment decodes on its own
sendTrailersOnlyTwoFrames :: Script ()
sendTrailersOnlyTwoFrames = do
    Frame.send $ Frame.mkFrame 0x1 0x1 1 (BS.Lazy.take 5 lateTrailersOnly)  -- END_STREAM
    Frame.send $ Frame.mkFrame 0x9 0x4 1 (BS.Lazy.drop 5 lateTrailersOnly)  -- END_HEADERS

sendTrailers :: Script ()
sendTrailers =
    Frame.send $ Frame.mkFrame 0x1 0x5 1 lateTrailers  -- END_STREAM | END_HEADERS

recvRequestHeaders :: Word32 -> Script ()
recvRequestHeaders sid = Frame.recv $ \frame ->
    case frameHeader frame of
      FrameHeader{frameType = 0x1, frameStreamId}
        | frameStreamId == sid -> Right ()
      _otherwise -> Left $ "Expected HEADERS on stream " ++ show sid ++ ", got " ++ show frame

{-------------------------------------------------------------------------------
  Header blocks

  The late blocks are the only ones that insert into the client's dynamic table.
-------------------------------------------------------------------------------}

-- | Initial headers (scenario 3); no insertion
responseHeaders :: Lazy.ByteString
responseHeaders = status200 <> contentType

-- | Late Trailers-Only response (scenarios 1 and 2)
--
-- @x-foo@ comes first after @:status@, so that the split in
-- 'sendTrailersOnlyTwoFrames' falls inside it.
lateTrailersOnly :: Lazy.ByteString
lateTrailersOnly = status200 <> xFooInsert <> contentType <> grpcStatusOk

-- | Late trailers (scenario 3); no pseudo-headers
lateTrailers :: Lazy.ByteString
lateTrailers = xFooInsert <> grpcStatusOk

-- | Call 2's Trailers-Only response, referencing the late block's insertion
probe :: Lazy.ByteString
probe = status200 <> xFooIndexed <> contentType <> grpcStatusOk

status200 :: Lazy.ByteString
status200 = bytes [0x88]                             -- static index 8

contentType :: Lazy.ByteString
contentType = mconcat [
      bytes [0x00, 0x0c], ascii "content-type"      -- literal, no indexing, new name
    , bytes [0x14],       ascii "application/grpc+raw"
    ]

grpcStatusOk :: Lazy.ByteString
grpcStatusOk = mconcat [
      bytes [0x00, 0x0b], ascii "grpc-status"       -- literal, no indexing, new name
    , bytes [0x01],       ascii "0"
    ]

xFooInsert :: Lazy.ByteString
xFooInsert = mconcat [
      bytes [0x40, 0x05], ascii "x-foo"             -- literal, incremental indexing, new name
    , bytes [0x03],       ascii "bar"               --   -> dynamic index 62
    ]

xFooIndexed :: Lazy.ByteString
xFooIndexed = bytes [0xbe]                           -- indexed, dynamic index 62

{-------------------------------------------------------------------------------
  Test RPC
-------------------------------------------------------------------------------}

type TestRpc = RawRpc "FramesAfterReset" "ping"

type instance RequestMetadata          TestRpc = [CustomMetadata]
type instance ResponseInitialMetadata  TestRpc = [CustomMetadata]
type instance ResponseTrailingMetadata TestRpc = [CustomMetadata]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

bytes :: [Word8] -> Lazy.ByteString
bytes = BS.Lazy.pack

ascii :: String -> Lazy.ByteString
ascii = BS.Lazy.Char8.pack

expectCancelled :: IO () -> IO ()
expectCancelled =
    handleJust isCancelled return
  where
    isCancelled :: GrpcException -> Maybe ()
    isCancelled e = if grpcError e == GrpcCancelled then Just () else Nothing
