{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sanity.EndOfStream (tests) where

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as BS.Lazy

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.StreamType qualified as Server

import Test.Tasty
import Test.Tasty.HUnit

import Test.Driver.ClientServer

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.EndOfStream" [
      testGroup "client" [
          testCase "sendAfterFinal" test_sendAfterFinal
        , testCase "recvAfterFinal" test_recvAfterFinal
        , testCase "recvTrailers"   test_recvTrailers
        ]
    , testGroup "server" [
          testCase "recvEndOfInput" test_recvEndOfInput
        ]
    ]

{-------------------------------------------------------------------------------
  Client tests
-------------------------------------------------------------------------------}

-- | Test that we get SendAfterFinal when we call 'sendInput' after the final
test_sendAfterFinal :: Assertion
test_sendAfterFinal = testClientServer $ ClientServerTest {
        config = def
      , server = [Server.someRpcHandler clientStreamingHandler]
      , client = simpleTestClient $ \conn -> do
          Client.withRPC conn def (Proxy @Absorb) $ \call -> do
            replicateM_ 10 $ Client.sendNextInput call BS.Lazy.empty
            Client.sendEndOfInput call

            -- The purpose of this test:
            mRes <- try $ Client.sendNextInput call BS.Lazy.empty
            case mRes of
              Left SendAfterFinal{} ->
                return ()
              _otherwise ->
                assertFailure "Expected SendAfterFinal"

            -- Communication with the server is unaffected
            (res, _) <- Client.recvFinalOutput call
            assertEqual "response" BS.Lazy.empty $ res
      }

-- | Test that we get RecvAfterFinal if we call 'recvOutput' after the final
test_recvAfterFinal :: Assertion
test_recvAfterFinal = testClientServer $ ClientServerTest {
      config = def
    , server = [Server.someRpcHandler serverStreamingHandler]
    , client = simpleTestClient $ \conn ->
        Client.withRPC conn def (Proxy @Spam) $ \call -> do
          Client.sendFinalInput call BS.Lazy.empty

          let loop :: IO ()
              loop = do
                mElem <- Client.recvOutput call
                case mElem of
                  StreamElem{}  -> loop
                  FinalElem{}   -> return ()
                  NoMoreElems{} -> return ()

          loop

          -- The purpose of this test:
          mRes <- try $ Client.recvOutput call
          case mRes of
            Left RecvAfterFinal{} ->
              return ()
            Right _ ->
              assertFailure "Expected RecvAfterFinal"

          return ()
    }

-- | Test that 'recvTrailers' does /not/ throw an exception, even if the
-- previous 'recvNextOutput' /happened/ to give us the final output.
test_recvTrailers :: Assertion
test_recvTrailers = testClientServer $ ClientServerTest {
      config = def
    , server = [Server.someRpcHandler nonStreamingHandler]
    , client = simpleTestClient $ \conn ->
        Client.withRPC conn def (Proxy @Trivial) $ \call -> do
          Client.sendFinalInput call BS.Lazy.empty

          resp <- Client.recvNextOutput call
          assertEqual "response" BS.Lazy.empty resp

          -- The purpose of this test:
          mTrailers <- try $ Client.recvTrailers call
          case mTrailers of
            Right _ ->
              return ()
            Left RecvAfterFinal{} ->
              assertFailure "Unexpected RecvAfterFinal"

          return ()
    }

{-------------------------------------------------------------------------------
  Auxiliary: server counter-parts to the client tests
-------------------------------------------------------------------------------}

-- | Receive any string, respond with a single 'mempty'
type Trivial = RawRpc "Test" "trivial"

-- | Service that simply absorbs all messages and then returns with 'mempty'
type Absorb = RawRpc "Test" "absorb"

-- | Service that waits for the go-ahead from the client and then spams the
-- client with a bunch of 'mempty' messages
type Spam = RawRpc "Test" "spam"

nonStreamingHandler :: Server.RpcHandler IO Trivial
nonStreamingHandler = Server.streamingRpcHandler $
    Server.mkNonStreaming $ \_inp ->
      return BS.Lazy.empty

clientStreamingHandler :: Server.RpcHandler IO Absorb
clientStreamingHandler = Server.streamingRpcHandler $
    Server.mkClientStreaming $ \recv -> do
      let loop :: IO ()
          loop = do
            mElem <- recv
            case mElem of
              StreamElem{}  -> loop
              FinalElem{}   -> return ()
              NoMoreElems{} -> return ()
      loop
      return mempty

serverStreamingHandler :: Server.RpcHandler IO Spam
serverStreamingHandler = Server.streamingRpcHandler $
    Server.mkServerStreaming $ \_inp send ->
      replicateM_ 10 $ send BS.Lazy.empty

{-------------------------------------------------------------------------------
  Server tests
-------------------------------------------------------------------------------}

-- | Test that 'recvEndOfInput' does /not/ throw an exception, even if the
-- previous 'recvNextInput' /happened/ to give us the final input.
--
-- This is the server equivalent of 'test_recvTrailers'.
test_recvEndOfInput :: Assertion
test_recvEndOfInput = testClientServer $ ClientServerTest {
      config = def
    , server = [Server.someRpcHandler handler]
    , client = simpleTestClient $ \conn ->
        Client.withRPC conn def (Proxy @Trivial) $ \call -> do
          Client.sendFinalInput call BS.Lazy.empty
          _resp <- Client.recvFinalOutput call
          return ()
    }
  where
    handler :: Server.RpcHandler IO Trivial
    handler = Server.mkRpcHandler $ \call -> do
        resp <- Server.recvNextInput call
        assertEqual "resp" BS.Lazy.empty $ resp

        -- The purpose of this test:
        res <- try $ Server.recvEndOfInput call
        case res of
          Right () ->
            return ()
          Left RecvAfterFinal{} ->
            assertFailure "Unexpected RecvAfterFinal"

        Server.sendFinalOutput call (mempty, NoMetadata)

{-------------------------------------------------------------------------------
  Auxiliary: metadata
-------------------------------------------------------------------------------}

type instance RequestMetadata          (RawRpc "Test" test) = NoMetadata
type instance ResponseInitialMetadata  (RawRpc "Test" test) = NoMetadata
type instance ResponseTrailingMetadata (RawRpc "Test" test) = NoMetadata

