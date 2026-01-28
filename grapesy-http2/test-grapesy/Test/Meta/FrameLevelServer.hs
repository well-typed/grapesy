{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Meta.FrameLevelServer (tests) where

import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS.Lazy.Char8
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
tests = testGroup "Test.Meta.FrameLevelServer" [
      testCase "trailersOnly" test_trailersOnly
    , testCase "echo"         test_echo
    ]

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

-- | Simplest exchange: the response is a single Trailers-Only frame
test_trailersOnly :: Assertion
test_trailersOnly =
    Frame.withScript Nothing trailersOnlyScript $ \server getHandlerResults -> do
      _trailers <-
        Client.withConnection def server $ \conn ->
          Client.withRPC conn def (Proxy @TestRpc) $ \call -> do
            Client.sendFinalInput call BS.Lazy.empty
            Client.recvTrailers call
      results <- getHandlerResults
      assertEqual "handler results" [()] results

-- | Full response (headers, message, trailers), echoing the request message
test_echo :: Assertion
test_echo =
    Frame.withScript Nothing echoScript $ \server getHandlerResults -> do
      (output, _trailers) <-
        Client.withConnection def server $ \conn ->
          Client.withRPC conn def (Proxy @TestRpc) $ \call -> do
            Client.sendFinalInput call (ascii "ping")
            Client.recvFinalOutput call
      assertEqual "output" (ascii "ping") output
      results <- getHandlerResults
      assertEqual "handler results" [()] results

{-------------------------------------------------------------------------------
  Test RPC
-------------------------------------------------------------------------------}

type TestRpc = RawRpc "FrameLevelServer" "ping"

type instance RequestMetadata          TestRpc = [CustomMetadata]
type instance ResponseInitialMetadata  TestRpc = [CustomMetadata]
type instance ResponseTrailingMetadata TestRpc = [CustomMetadata]

{-------------------------------------------------------------------------------
  Scripts
-------------------------------------------------------------------------------}

-- | Receive the request, respond with a single Trailers-Only frame
trailersOnlyScript :: Script ()
trailersOnlyScript = do
    Frame.handshake
    recvRequestHeaders
    _msg <- Frame.recvUntilEndStream 1
    Frame.send $ Frame.mkFrame 0x1 0x5 1 trailersOnly  -- END_STREAM | END_HEADERS

-- | Receive the request, respond with headers, the same message, and trailers
--
-- The gRPC length-prefixed message format is the same in both directions, so
-- the request message can be sent back verbatim. This relies on the request
-- being uncompressed; we never advertise @grpc-accept-encoding@, so the client
-- has no reason to compress.
echoScript :: Script ()
echoScript = do
    Frame.handshake
    recvRequestHeaders
    msg <- Frame.recvUntilEndStream 1
    Frame.send $ Frame.mkFrame 0x1 0x4 1 responseHeaders   -- END_HEADERS
    Frame.send $ Frame.mkFrame 0x0 0x0 1 msg
    Frame.send $ Frame.mkFrame 0x1 0x5 1 responseTrailers  -- END_STREAM | END_HEADERS

-- | Request headers on stream 1 (not decoded)
recvRequestHeaders :: Script ()
recvRequestHeaders = Frame.recv $ \frame ->
    case frameHeader frame of
      FrameHeader{frameType = 0x1, frameStreamId = 1} -> Right ()
      _otherwise -> Left $ "Expected HEADERS on stream 1, got " ++ show frame

{-------------------------------------------------------------------------------
  Header blocks

  Only static-table references and literals without indexing, so none of these
  responses changes the client's dynamic table.
-------------------------------------------------------------------------------}

-- | Trailers-Only: status, content-type and trailers in a single block
trailersOnly :: Lazy.ByteString
trailersOnly = responseHeaders <> responseTrailers

responseHeaders :: Lazy.ByteString
responseHeaders = mconcat [
      bytes [0x88]                                  -- :status 200 (static index 8)
    , bytes [0x00, 0x0c], ascii "content-type"      -- literal, no indexing, new name
    , bytes [0x14],       ascii "application/grpc+raw"
    ]

responseTrailers :: Lazy.ByteString
responseTrailers = mconcat [
      bytes [0x00, 0x0b], ascii "grpc-status"       -- literal, no indexing, new name
    , bytes [0x01],       ascii "0"
    ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

bytes :: [Word8] -> Lazy.ByteString
bytes = BS.Lazy.pack

ascii :: String -> Lazy.ByteString
ascii = BS.Lazy.Char8.pack
