{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Sanity.Any (tests) where

import Control.Exception
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

import Proto.API.Ping
import Proto.API.TestAny

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.Any" [
      testCase "Any"    testAny
    , testCase "Status" testStatus
    ]

{-------------------------------------------------------------------------------
  Using the 'Any' wrapper
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
    detail1 = defMessage & #a .~ 1

    detail2 :: Proto B
    detail2 = defMessage & #b .~ 1

    withDetails :: [Proto Any] -> Proto TestAnyMsg
    withDetails details =
        defMessage
          & #message .~ "foo"
          & #details .~ details

{-------------------------------------------------------------------------------
  Protobuf-specific error details (which relies on 'Any')
-------------------------------------------------------------------------------}

testStatus :: Assertion
testStatus = testClientServer $ ClientServerTest {
      config = def {
          isExpectedServerException = \e ->
            case fromException e of
              Just err'  -> grpcError err' == GrpcNotFound
              _otherwise -> False
        }
    , server = [
          Server.fromMethod @Ping $ Server.ServerHandler $ \_req ->
            throwProtobufErrorHom protobufError
        ]
    , client = simpleTestClient $ \conn -> do
        mResp :: Either GrpcException (Proto PongMessage) <-
          try $ Client.nonStreaming conn (rpc @Ping) defMessage
        case mResp of
          Right _   -> assertFailure "Expected exception"
          Left  err ->
            assertEqual "" (Right protobufError) $
              toProtobufErrorHom err
    }
  where
    protobufError :: ProtobufError (Proto A)
    protobufError = ProtobufError {
          protobufErrorCode    = GrpcNotFound
        , protobufErrorMessage = Just "Not found"
        , protobufErrorDetails = [detail1, detail2]
        }


    detail1, detail2 :: Proto A
    detail1 = defMessage & #a .~ 1
    detail2 = defMessage & #a .~ 2
