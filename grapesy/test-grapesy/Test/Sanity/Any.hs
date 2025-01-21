{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Sanity.Any (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client (rpc)
import Network.GRPC.Client.StreamType.IO qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.Protobuf.Any (Any)
import Network.GRPC.Common.Protobuf.Any qualified as Any
import Network.GRPC.Server.StreamType qualified as Server

import Test.Driver.ClientServer

import Proto.API.TestAny

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.Any" [
      testCase "any" testAny
    ]

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

testAny :: Assertion
testAny = testClientServer $ ClientServerTest {
      config = def
    , server = [Server.fromMethod @Reverse $ Server.ServerHandler handler]
    , client = simpleTestClient $ \conn -> do
        let req, expected :: Proto TestAnyMsg
            req      = withDetails [Any.pack detail1, Any.pack detail2]
            expected = withDetails [Any.pack detail2, Any.pack detail1]

        resp <- Client.nonStreaming conn (rpc @Reverse) req
        assertEqual "" expected $ resp
    }
  where
    handler :: Proto TestAnyMsg -> IO (Proto TestAnyMsg)
    handler msg = return $ msg & #details %~ reverse

    detail1 :: Proto A
    detail1 = defMessage

    detail2 :: Proto B
    detail2 = defMessage

    withDetails :: [Proto Any] -> Proto TestAnyMsg
    withDetails details = defMessage
            & #message .~ "foo"
            & #details .~ details
