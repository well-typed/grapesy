{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sanity.Trailers (tests) where

import Control.Monad
import Data.Binary (Binary)
import Data.ByteString qualified as BSS
import Data.List qualified as List
import Data.String
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary

import Test.Driver.ClientServer

{-------------------------------------------------------------------------------
  Testcases

  These are primarily tests of the underlying http2 substrate. We test for
  three regressions specifically:

  A. HPACK desync on reset streams. A header block arrives for a stream we've
     already removed from the stream table; getStream returns Nothing and
     controlOrStream's otherwise -> return () drops it instead of feeding it to
     the decoder. Connection-global table, so everything after is wrong.

  B. Trailers spanning CONTINUATION can't be received. The trailers clause in
     stream tests endOfStream and never endOfHeader, so it decodes only the
     first fragment.

  C. A single field line larger than the frame payload can't be sent. The
     encoder splits at header boundaries, so an oversized field yields "cannot
     compress the header" as a connection error.
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Trailers" [
      testCase (testName params) $ testWithParams params
    | params <- testParams
    ]

data TestParams = TestParams{
      numCalls    :: Int
    , numTrailers :: Int
    , trailerSize :: Int
    }

testParams :: [TestParams]
testParams = [
      -- Controls: everything fits in one frame. Should pass today.
      TestParams 1 2   1000   -- ~2706
    , TestParams 3 2   1000   -- same, 3 calls: is the multi-call harness itself sound?
    , TestParams 1 20  100    -- ~3060, many small — control for the block below

      -- Bracket the CONTINUATION threshold (~1525 at n=2)
    , TestParams 1 2   1400   -- ~3770, one frame  -> pass
    , TestParams 1 2   1600   -- ~4300, two frames -> B

      -- Bug B, unambiguous
    , TestParams 1 2   2000   -- silently loses trailer1

      -- Bracket the B/C threshold (~3050)
    , TestParams 1 2   2900   -- field ~3886 < 4087 -> B
    , TestParams 1 2   3200   -- field ~4286 > 4087 -> C

      -- Bug C, unambiguous
    , TestParams 1 2   4000   -- "cannot compress the header"

      -- Cross-call desync: entries small enough to persist (~181 each, table holds ~22)
    , TestParams 1 40  100    -- ~6120, 2 frames; loses ~14 trailers silently
    , TestParams 2 40  100    -- the payoff: call 2 resolves indices against a wrong table
    , TestParams 3 40  100    -- does it compound, or error?
    , TestParams 2 80  100    -- ~12240, 3 frames; more divergence

      -- Post-fix only: exercises multi-frame reassembly properly
    , TestParams 1 200 100    -- ~30600, 8 frames — under continuationLimit (10)
    ]

{-------------------------------------------------------------------------------
  Test output
-------------------------------------------------------------------------------}

testName :: TestParams -> TestName
testName params = List.intercalate "." . map show $ [
      params.numCalls
    , params.numTrailers
    , params.trailerSize
    ]

type Error = String

-- | Compare received trailers against expected
--
-- We deliberately avoid @show@ing the trailers: at the top end of 'testParams'
-- that is tens of kilobytes of escaped bytes. Instead we exploit the fact that
-- 'mkTrailers' fills each value with the trailer's own index, so @"100 x 17"@
-- both describes the value and identifies which trailer it came from. A desync
-- then reads directly as @expected "100 x 17", got "100 x 21"@.
checkTrailers :: Int -> [CustomMetadata] -> [CustomMetadata] -> [Error]
checkTrailers callIx expected actual = concat [
      [ inCall $ concat [
            "number of trailers: "
          , "expected " , show (length expected)
          , ", got "    , show (length actual)
          ]
      | length expected /= length actual
      ]

    , [ inCall $ concat [
            "trailer "    , show i
          , ": expected " , describe e
          , ", got "      , describe a
          ]
      | (i, e, a) <- zip3 [0 :: Int ..] expected actual
      , e /= a
      ]
    ]
  where
    inCall :: String -> String
    inCall msg = "call " ++ show callIx ++ ": " ++ msg

    describe :: CustomMetadata -> String
    describe md = concat [
          show (customMetadataName md), " = "
        , describeValue (customMetadataValue md)
        ]

    describeValue :: BSS.ByteString -> String
    describeValue bs =
        case BSS.uncons bs of
          Nothing -> "<empty>"
          Just (b, rest)
            | BSS.all (== b) rest -> show (BSS.length bs) ++ " x " ++ show b
            | otherwise           -> show (BSS.length bs) ++ " bytes (mixed)"

{-------------------------------------------------------------------------------
  Test proper / gRPC client
-------------------------------------------------------------------------------}

testWithParams :: TestParams -> Assertion
testWithParams params = testClientServer ClientServerTest{
      config = def
    , server = [Server.someRpcHandler @TestRpc sendTrailers]
    , client = simpleTestClient $ \conn -> do
        errs <- fmap concat $ forM [0 .. params.numCalls - 1] $ \callIx -> do
          Client.withRPC conn def (Proxy @TestRpc) $ \call -> do
            Client.Binary.sendFinalInput call serverParams
            ((), actual) <- Client.Binary.recvFinalOutput call
            return $ checkTrailers callIx (mkTrailers serverParams) actual
        unless (null errs) $ assertFailure $ List.intercalate "\n" errs
    }
  where
    serverParams :: ServerParams
    serverParams = ServerParams{
          numTrailers = params.numTrailers
        , trailerSize = params.trailerSize
        }

{-------------------------------------------------------------------------------
  Server handler
-------------------------------------------------------------------------------}

type TestRpc = RawRpc "TestTrailers" "Test"

type instance RequestMetadata          TestRpc = [CustomMetadata]
type instance ResponseInitialMetadata  TestRpc = [CustomMetadata]
type instance ResponseTrailingMetadata TestRpc = [CustomMetadata]

data ServerParams = ServerParams{
      numTrailers :: Int
    , trailerSize :: Int
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

sendTrailers :: Server.RpcHandler IO TestRpc
sendTrailers = Server.mkRpcHandlerNoDefMetadata $ \call -> do
    params <- Server.Binary.recvFinalInput call
    let trailers = mkTrailers params

    -- We do /not/ announce the trailers ahead of time.
    --
    -- The @Trailer@ header is optional, and we skip it here: with many trailers,
    -- it would /itself/ get large, which would confound the test.
    Server.setResponseInitialMetadataAndTrailers call [] . Just $
      map customMetadataName trailers

    -- Send the trailers proper
    Server.Binary.sendFinalOutput @() call ((), trailers)

mkTrailers :: ServerParams -> [CustomMetadata]
mkTrailers params = [
      metadata i params.trailerSize
    | i <- [0 .. params.numTrailers - 1]
    ]
  where
    metadata :: Int -> Int -> CustomMetadata
    metadata i sz =
        CustomMetadata
          (fromString $ "trailer" ++ printf "%03d" i ++ "-bin")
          (BSS.pack . replicate sz $ fromIntegral i)

