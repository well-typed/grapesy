{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

-- | Handling of exceptions occurring in an RPC on a shared connection.
--
-- These tests check for the behavior described in
-- <https://github.com/well-typed/grapesy/issues/102>. In particular, there are
-- two conditions that should hold when an exception occurs in the scope of a
-- call (on either the server or client, e.g. inside either 'mkRpcHandler' or
-- 'withRPC'):
--
-- 1. Other ongoing calls on that connection are not terminated (client), and
--    handlers dealing with other calls on that connection are not terminated
--    (server), and
-- 2. future calls are still possible (client), and more handlers can be started
--    to deal with future calls (server).
module Test.Regression.Issue102 (tests) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Either
import Data.IORef
import Data.Text qualified as Text
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Binary

import Proto.API.Trivial

import Test.Driver.ClientServer
import Test.Util.Exception

tests :: TestTree
tests = testGroup "Issue102" [
      testCase "client" test_clientException
    , testCase "server" test_serverException

    , testCase "earlyTerminationNoWait"  test_earlyTerminationNoWait
    ]

-- | Client makes many concurrent calls, throws an exception during one of them.
test_clientException :: IO ()
test_clientException = testClientServer $ ClientServerTest {
      config = def {
           isExpectedClientException = isDeliberateException
         , isExpectedServerException = isClientDisconnected
         }
    , client = simpleTestClient $ \conn -> do
        -- Make 100 concurrent calls. 99 of them counting to 50, and one
        -- more that throws an exception once it reaches 10.
        let
          predicate  = (> 50)
          predicates =
            replicate 99 predicate ++
              [ \n ->
                     (n > 10)
                  && throw (DeliberateException $ SomeClientException 1)
              ]

        results <-
          mapConcurrently
            (   try @DeliberateException
              . Client.withRPC conn def (Proxy @Trivial)
              . countUntil
            )
            predicates

        -- Only one of the calls failed
        assertEqual "" (length $ lefts results) 1

        -- All others terminated with results satisfying the predicate
        assertBool "" (all predicate $ rights results)

        -- New calls still succeed
        assertBool "" . predicate
          =<< Client.withRPC conn def (Proxy @Trivial) (countUntil predicate)
    , server = [
          Server.someRpcHandler $
            Server.mkRpcHandler @Trivial incUntilFinal
        ]
    }

-- | Client makes many concurrent calls, the handler throws an exception during
-- one of them.
test_serverException :: IO ()
test_serverException = do
    handlerCounter <- newIORef @Int 0
    testClientServer $ ClientServerTest {
        config = def { isExpectedServerException = isDeliberateException }
      , client = simpleTestClient $ \conn -> do
          -- Make 100 concurrent calls counting to 50.
          let predicate = (> 50)
          results <-
            replicateConcurrently 100 $
                  try @GrpcException
                . Client.withRPC conn def (Proxy @Trivial)
                $ countUntil predicate

          -- Only one of the calls failed, and we got the appropriate
          -- exception
          case lefts results of
            [GrpcException GrpcUnknown (Just msg) []] -> do
              assertBool "" $ "DeliberateException"   `Text.isInfixOf` msg
              assertBool "" $ "SomeServerException 1" `Text.isInfixOf` msg
            _ ->
              assertFailure ""

          -- All others terminated with results satisfying the predicate
          assertBool "" (all predicate $ rights results)

          -- New calls still succeed
          assertBool "" . predicate
            =<< Client.withRPC conn def (Proxy @Trivial) (countUntil predicate)
      , server = [
              Server.someRpcHandler $
                Server.mkRpcHandler @Trivial $ \call -> do
                    handlerCount <-
                      atomicModifyIORef' handlerCounter (\n -> (n + 1, n))
                    when (handlerCount == 25) $
                      throwIO $
                        DeliberateException $ SomeServerException 1
                    incUntilFinal call
          ]
      }

-- | This is essentially 'Test.Prop.Dialogue.earlyTermination15', but the server
-- does not wait for client termination.
test_earlyTerminationNoWait :: IO ()
test_earlyTerminationNoWait = testClientServer $ ClientServerTest {
      config = def {
          isExpectedClientException = isDeliberateException
        , isExpectedServerException = isClientDisconnected
        }
    , client = simpleTestClient $ \conn -> do
        _mResult <-
          try @DeliberateException $
            Client.withRPC conn def (Proxy @Trivial) $ \_call ->
              throwIO (DeliberateException $ SomeServerException 0)

        result <- Client.withRPC conn def (Proxy @Trivial) $ \call -> do
          Binary.sendFinalInput @Word8 call 0
          Binary.recvFinalOutput @Word8 call

        assertEqual "" (1, NoMetadata) result
    , server = [
          Server.someRpcHandler $
            Server.mkRpcHandler @Trivial $ \call ->
              Binary.recvInput @Word8 call >>= \case
                _ -> Binary.sendFinalOutput @Word8 call (1, NoMetadata)
        ]
    }

{-------------------------------------------------------------------------------
  Client and handler functions
-------------------------------------------------------------------------------}

-- | Send numbers to the server until it responds with one that satisfies the
-- given predicate.
countUntil :: (Word64 -> Bool) -> Client.Call Trivial -> IO Word64
countUntil = go 0
  where
    go :: Word64 -> (Word64 -> Bool) -> Client.Call Trivial -> IO Word64
    go next p call
      | p next
      = do
        Binary.sendFinalInput @Word64 call next
        (_final, NoMetadata) <- Binary.recvFinalOutput @Word64 call
        return next
      | otherwise
      = do
        Binary.sendNextInput @Word64 call next
        next' <- Binary.recvNextOutput @Word64 call
        go next' p call

-- | Reads numbers from the client and sends them back incremented by one.
incUntilFinal :: Server.Call Trivial -> IO ()
incUntilFinal call = do
    Binary.recvInput call >>= \case
      StreamElem n -> do
        Binary.sendNextOutput @Word64 call $ succ n
        incUntilFinal call
      FinalElem n _ -> do
        Binary.sendFinalOutput @Word64 call (succ n, NoMetadata)
      NoMoreElems _ -> do
        -- This is never hit, since our client never sends 'NoMoreElems'.
        error "Test.Sanity.Exception.incUntilFinal: unexpected NoMoreElems"
