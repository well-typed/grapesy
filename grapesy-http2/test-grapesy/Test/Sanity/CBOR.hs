{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sanity.CBOR (tests) where

import Codec.Serialise (serialise)
import Data.Either (isLeft)
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client (rpc)
import Network.GRPC.Client.StreamType.IO qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.CBOR
import Network.GRPC.Server.StreamType qualified as Server

import Test.Driver.ClientServer

type Sum = CborRpc "calculator" "sum"

type instance Input  Sum = [Int]
type instance Output Sum = Int

type instance RequestMetadata          Sum = NoMetadata
type instance ResponseInitialMetadata  Sum = NoMetadata
type instance ResponseTrailingMetadata Sum = NoMetadata

tests :: TestTree
tests = testGroup "Test.Sanity.CBOR" [
      testCase "sum"           test_sum
    , testCase "trailingBytes" test_trailingBytes
    ]

test_sum :: IO ()
test_sum = testClientServer $ ClientServerTest {
      config = def
    , client = simpleTestClient $ \conn -> do
        resp <- Client.nonStreaming conn (rpc @Sum) [1 .. 100]
        assertEqual "" 5050 resp
    , server = [
          Server.fromMethod @Sum $ Server.mkNonStreaming $ return . sum
        ]
    }

test_trailingBytes :: IO ()
test_trailingBytes = do
    let input  = serialise [1, 2 :: Int]
        output = serialise (3 :: Int)
    assertEqual "" (Right [1, 2]) $
      rpcDeserializeInput (Proxy @Sum) input
    assertEqual "" (Right 3) $
      rpcDeserializeOutput (Proxy @Sum) output
    assertBool "" $ isLeft $
      rpcDeserializeInput (Proxy @Sum) (input <> output)
    assertBool "" $ isLeft $
      rpcDeserializeOutput (Proxy @Sum) (output <> input)
