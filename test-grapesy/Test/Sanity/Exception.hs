{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Test.Sanity.Exception where

import Control.Concurrent.Async
import Control.Exception ( try, throw, Exception )
import Data.Either
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Binary
import Network.GRPC.Spec

import Test.Driver.ClientServer hiding (DeliberateException)
import Test.Driver.ClientServer (ClientServerConfig(expectEarlyServerTermination))

-- Server handler throws an exception:
-- 1. Current call should get that exception, concurrent calls should stay alive
--    (implying the other handlers stay alive)
-- 2. Future calls should succeed

-- Close 102 if this block and the property checks below pass:
-- Client throws an exception in the scope of withRPC (this is #102):
-- 1. Concurrent calls should stay alive, withRPC should not swallow the
--    exception (this implies that the handlers continue running)
-- 2. Future calls should succeed

-- 1. No exceptions, connection per RPC (simplest case)
-- 2. No exceptions, shared connection
-- 3. Exceptions, connection per RPC
-- 4. Exceptions, shared connection


tests :: TestTree
tests = testGroup "Test.Sanity.Exception" [
      testCase "client" test_clientException
    ]

test_clientException :: IO ()
test_clientException = testClientServer $ ClientServerTest {
      config = def -- { expectEarlyServerTermination = True }
    , client = simpleTestClient $ \conn -> do
        -- Make 100 concurrent calls. 99 of them counting to 50, and one
        -- more that throws an exception once it reaches 10.
        let
          predicate  = (> 50)
          predicates =
            replicate 99 predicate ++
              [ \n -> (n > 10) && throw DeliberateException
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
  where
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

    incUntilFinal :: Server.Call Trivial -> IO ()
    incUntilFinal call = do
        Binary.recvInput call >>= \case
          StreamElem n -> do
            Binary.sendNextOutput @Word64 call $ succ n
            incUntilFinal call
          FinalElem n _ -> do
            Binary.sendFinalOutput @Word64 call (succ n, NoMetadata)
          NoMoreElems _ -> do
            -- TODO: <https://github.com/well-typed/grapesy/issues/209>
            --
            -- We shouldn't need to handle this case, since our client never
            -- explicitly sends 'NoMoreElems'. However, see discussion in the
            -- ticket above.
            -- Server.sendTrailers call NoMetadata
            return ()

type Trivial = RawRpc "trivial" "trivial"

data DeliberateException = DeliberateException
  deriving (Show, Exception)

type instance RequestMetadata          Trivial = NoMetadata
type instance ResponseInitialMetadata  Trivial = NoMetadata
type instance ResponseTrailingMetadata Trivial = NoMetadata
