{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Sanity.Compression (tests) where

import Control.Monad
import Data.IORef
import Data.Maybe (isJust)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server qualified as Server

import Test.Driver.ClientServer

import Proto.API.Helloworld

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.Compression" [
      testCase "multipleRPC"  test_multipleRPC
    , testCase "multipleMsgs" test_multipleMsgs
    ]

{-------------------------------------------------------------------------------
  Individual tests
-------------------------------------------------------------------------------}

-- | Test that compression is enabled for the /second/ RPC
test_multipleRPC :: Assertion
test_multipleRPC = do
    counter <- newIORef 0
    testClientServer $ ClientServerTest {
        config = def
      , server = [Server.someRpcHandler $ handleNonStreaming counter]
      , client = simpleTestClient $ \conn ->
            replicateM_ 2 $ do
              Client.withRPC conn def (Proxy @SayHello) $ \call -> do
                Client.sendFinalInput call req
                mResp <- StreamElem.value <$> Client.recvOutputWithMeta call
                case mResp of
                  Nothing -> assertFailure "Expected response"
                  Just (meta, resp) -> do
                    -- /All/ responses from the server should be compressed
                    -- (the request tells the server what the client supports)
                    assertEqual "" True $
                      isJust (inboundCompressedSize meta)
                    assertEqual "" compressibleName $
                      resp ^. #message
    }
  where
    req :: Proto HelloRequest
    req = defMessage & #name .~ compressibleName

-- | Test that multiple messages on /one/ RPC will either all be compressed or
-- all uncompressed.
test_multipleMsgs :: Assertion
test_multipleMsgs = do
    counter <- newIORef 0
    testClientServer $ ClientServerTest {
        config = def
      , server = [Server.someRpcHandler $ handleBidiStreaming counter]
      , client = simpleTestClient $ \conn ->
          replicateM_ 2 $
            Client.withRPC conn def (Proxy @SayHelloBidiStream) $ \call -> do
              replicateM_ 2 $ do
                Client.sendNextInput call req
                mResp <- StreamElem.value <$> Client.recvOutputWithMeta call
                case mResp of
                  Nothing -> assertFailure "Expected response"
                  Just (meta, resp) -> do
                    -- /All/ responses from the server should be compressed
                    -- (the request tells the server what the client supports)
                    assertEqual "" True $
                      isJust (inboundCompressedSize meta)
                    assertEqual "" compressibleName $
                      resp ^. #message
              Client.sendEndOfInput call

              -- Calling 'sendEndOfInput' merely tells the server that we won't
              -- be /sending/ it any further messages; it doesn't mean (as far
              -- as the gRPC spec is concerned) that we're not /expecting/ any
              -- further messages. For our /specific/ way of interacting this
              -- /is/ the case, but if we don't wait for the server to confirm
              -- that no more messages are coming, that is considered
              -- cancellation and the server will receive a RST_STREAM.
              Client.recvTrailers call
      }
  where
    req :: Proto HelloRequest
    req = defMessage & #name .~ compressibleName

{-------------------------------------------------------------------------------
  Server handlers
-------------------------------------------------------------------------------}

handleNonStreaming :: IORef Int -> Server.RpcHandler IO SayHello
handleNonStreaming counter = Server.mkRpcHandler $ \call -> do
    mElem <- Server.recvInputWithMeta call
    case mElem of
      FinalElem (meta, req) NoMetadata -> do
        callNo <- atomicModifyIORef counter $ \i -> (succ i, i)

        -- We expect all messages to be compressed except the first (the client
        -- does not yet know which compression algorithms the server supports)
        let expectCompression :: Bool
            expectCompression = callNo > 0
        assertEqual "" expectCompression $
          isJust (inboundCompressedSize meta)

        Server.sendFinalOutput call (
            defMessage & #message .~ (req ^. #name)
          , NoMetadata
          )
      _otherwise ->
        assertFailure "expected FinalElem"

handleBidiStreaming :: IORef Int -> Server.RpcHandler IO SayHelloBidiStream
handleBidiStreaming counter = Server.mkRpcHandler $ \call -> do
    isFirstCall <- atomicModifyIORef counter $ \i -> (succ i, i == 0)

    let loop :: IO ()
        loop = do
          mElem <- Server.recvInputWithMeta call
          case mElem of
            NoMoreElems NoMetadata ->
              Server.sendTrailers call NoMetadata
            StreamElem (meta, req) -> do
              -- The compression algorithm is established once at the start of
              -- the request; we cannot start compression halfway a conversation
              let expectCompression :: Bool
                  expectCompression = not isFirstCall
              assertEqual "" expectCompression $
                isJust (inboundCompressedSize meta)

              Server.sendNextOutput call $
                defMessage & #message .~ (req ^. #name)
              loop
            FinalElem{} ->
              assertFailure "Unexpected FinalElem"

    loop

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

compressibleName :: Text
compressibleName = mconcat (replicate 100 "John")
