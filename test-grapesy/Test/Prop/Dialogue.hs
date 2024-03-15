{-# LANGUAGE OverloadedStrings #-}

module Test.Prop.Dialogue (tests) where

import Control.Exception
import Data.Map.Strict qualified as Map
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Network.GRPC.Common

import Test.Driver.ClientServer
import Test.Driver.Dialogue

tests :: TestTree
tests = testGroup "Test.Prop.Dialogue" [
      testGroup "Regression" [
          testCase "trivial1"           $ regression trivial1
        , testCase "trivial2"           $ regression trivial2
        , testCase "trivial3"           $ regression trivial3
        , testCase "concurrent1"        $ regression concurrent1
        , testCase "concurrent2"        $ regression concurrent2
        , testCase "concurrent3"        $ regression concurrent3
        , testCase "concurrent4"        $ regression concurrent4
        , testCase "exception1"         $ regression exception1
        , testCase "exception2"         $ regression exception2
        , testCase "earlyTermination01" $ regression earlyTermination01
        , testCase "earlyTermination02" $ regression earlyTermination02
        , testCase "earlyTermination03" $ regression earlyTermination03
        , testCase "earlyTermination04" $ regression earlyTermination04
        , testCase "earlyTermination05" $ regression earlyTermination05
        , testCase "earlyTermination06" $ regression earlyTermination06
        , testCase "earlyTermination07" $ regression earlyTermination07
        , testCase "earlyTermination08" $ regression earlyTermination08
        , testCase "earlyTermination09" $ regression earlyTermination09
        , testCase "earlyTermination10" $ regression earlyTermination10
        , testCase "earlyTermination11" $ regression earlyTermination11
        , testCase "earlyTermination12" $ regression earlyTermination12
        , testCase "earlyTermination13" $ regression earlyTermination13
        , testCase "earlyTermination14" $ regression earlyTermination14
        , testCase "allowHalfClosed1"   $ regression allowHalfClosed1
        , testCase "allowHalfClosed2"   $ regression allowHalfClosed2
        , testCase "allowHalfClosed3"   $ regression allowHalfClosed3
        ]
    , testGroup "Setup" [
          testProperty "shrinkingWellFounded" prop_shrinkingWellFounded
        ]
    , testGroup "Arbitrary" [
          testProperty "withoutExceptions" arbitraryWithoutExceptions
        , testProperty "withExceptions"    arbitraryWithExceptions
        ]
    ]

{-------------------------------------------------------------------------------
  Verify setup is correct
-------------------------------------------------------------------------------}

prop_shrinkingWellFounded :: Property
prop_shrinkingWellFounded =
    -- Explicit 'forAll' so that we can disable shrinking here
    -- (If shrinking is /not/ well-founded, then we should not try to shrink!)
    forAll arbitrary $ \(d :: DialogueWithoutExceptions) ->
      d `notElem` shrink d

{-------------------------------------------------------------------------------
  Running the tests
-------------------------------------------------------------------------------}

arbitraryWithoutExceptions :: DialogueWithoutExceptions -> Property
arbitraryWithoutExceptions (DialogueWithoutExceptions dialogue) =
    propDialogue dialogue

arbitraryWithExceptions :: DialogueWithExceptions -> Property
arbitraryWithExceptions (DialogueWithExceptions dialogue) =
    propDialogue dialogue

propDialogue :: Dialogue -> Property
propDialogue dialogue =
    counterexample (show globalSteps) $
      propClientServer $ execGlobalSteps globalSteps
  where
    globalSteps :: GlobalSteps
    globalSteps = dialogueGlobalSteps dialogue

regression :: Dialogue -> IO ()
regression dialogue =
    handle (throwIO . RegressionTestFailed globalSteps) $
      testClientServer =<< execGlobalSteps globalSteps
  where
    globalSteps :: GlobalSteps
    globalSteps = dialogueGlobalSteps dialogue

data RegressionTestFailed = RegressionTestFailed {
      regressionGlobalSteps :: GlobalSteps
    , regressionException   :: SomeException
    }
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Regression tests

  We use only 'NormalizedDialogue' here, so that /if/ we change normalization
  (because some additional invariant needs to be preserved, for example), we
  will get a useful test failure indicating which tests need to be adjusted.
-------------------------------------------------------------------------------}

-- | Sanity check: server tells client immediately that there is nothing to say
--
-- One of the things this test verifies (apart from the testing infrastructure
-- itself) is that the server gets the chance to terminate cleanly.
trivial1 :: Dialogue
trivial1 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Variation on 'trivial1' where the client sends a message before the
-- server closed the call
trivial2 :: Dialogue
trivial2 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ClientAction $ Send (StreamElem 1234))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Variation on 'trivial1' where the server closes the call unilaterally
trivial3 :: Dialogue
trivial3 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Verify that the test infrastructure does not confuse client/server threads
--
-- It's trickier than one might think to make sure that the server handler and
-- a client agree on a set of steps to execute. This test, along with
-- 'concurrent2' and 'concurrent3', and sanity checks to make sure that this
-- goes well.
concurrent1 :: Dialogue
concurrent1 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (1, ClientAction $ Initiate (Map.empty, RPC1))
    , (1, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    , (1, ServerAction $ Send (NoMoreElems Map.empty))
    ]

concurrent2 :: Dialogue
concurrent2 = NormalizedDialogue [
      (1, ClientAction $ Initiate (Map.empty, RPC1))
    , (1, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    , (1, ServerAction $ Send (NoMoreElems Map.empty))
    ]

concurrent3 :: Dialogue
concurrent3 = NormalizedDialogue [
      (1, ClientAction $ Initiate (Map.singleton "md2" (AsciiHeader "b"), RPC1))
    , (0, ClientAction $ Initiate (Map.singleton "md1" (AsciiHeader "a"), RPC1))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    , (1, ClientAction $ Send (NoMoreElems NoMetadata))
    , (1, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Test that the final message is received
--
-- See also <https://github.com/kazu-yamamoto/http2/issues/86>.
concurrent4 :: Dialogue
concurrent4 = NormalizedDialogue [
      (1, ClientAction $ Initiate (Map.empty, RPC1))
    , (1, ClientAction $ Send (FinalElem 1 NoMetadata))
    , (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ClientAction $ Send (FinalElem 2 NoMetadata))
    , (1, ServerAction $ Send (FinalElem 3 Map.empty))
    , (0, ServerAction $ Send (FinalElem 4 Map.empty))
    ]

-- | Server-side exception
--
-- Since the handler throws an exception without sending /anything/, the
-- exception will be reported to the client using the gRPC trailers-only case.
-- This test also verifies that the client can /receive/ this message
exception1 :: Dialogue
exception1 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ServerAction $ Terminate (Just $ SomeServerException 0))
    ]

-- | Client-side exception
exception2 :: Dialogue
exception2 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ClientAction $ Terminate (Just (SomeClientException 0)))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Server handler terminates before the client expects it
earlyTermination01 :: Dialogue
earlyTermination01 = NormalizedDialogue [
      (0, ClientAction (Initiate (Map.empty, RPC1)))
    , (0, ServerAction (Terminate Nothing))
    ]

-- | Client terminates before the server handler expects it
earlyTermination02 :: Dialogue
earlyTermination02 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ClientAction $ Terminate Nothing)
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Variation on early client-side termination that tends to trigger a
-- different code path, where we get a stream error; in @grapesy@ these
-- various kinds of exceptions we can get from @http2@ should all be reported
-- in the same manner (as 'ClientDisconnected' exceptions).
earlyTermination03 :: Dialogue
earlyTermination03 = NormalizedDialogue [
      (1, ClientAction $ Initiate (Map.empty, RPC1 ))
    , (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (1, ClientAction $ Terminate (Just (SomeClientException 0)))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (1, ServerAction $ Send (NoMoreElems Map.empty))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Another minor variation on 'earlyTermination03', which tends to trigger yet
-- another codepath
earlyTermination04 :: Dialogue
earlyTermination04 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ServerAction $ Initiate Map.empty)
    , (1, ClientAction $ Initiate (Map.empty, RPC1 ))
    , (1, ClientAction $ Terminate (Just (SomeClientException 0)))
    , (1, ServerAction $ Send (NoMoreElems Map.empty))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- Test that early termination in one call does not affect the other. This is
-- currently /only/ true if they use separate connections; see discussion in
-- 'clientGlobal'.
earlyTermination05 :: Dialogue
earlyTermination05 = NormalizedDialogue [
      (1, ClientAction $ Initiate (Map.empty,RPC1))
    , (1, ServerAction $ Terminate Nothing)
    , (0, ClientAction $ Initiate (Map.empty,RPC1))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Variation where the client does send some messages before throwing an
-- exception
--
-- This is mostly a check on the test infrastructure itself. In a test case
-- like this where a message is enqueued and then an exception is thrown, the
-- exception might " overtake " that message and the server will never
-- receive it. This motivates the " conservative " test mode where we test
-- each operation in a synchronous manner.
earlyTermination06 :: Dialogue
earlyTermination06 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ClientAction $ Send (StreamElem 0))
    , (0, ClientAction $ Terminate (Just (SomeClientException 0)))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Server-side early termination
earlyTermination07 :: Dialogue
earlyTermination07 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ServerAction $ Initiate Map.empty)
    , (0, ServerAction $ Terminate (Just (SomeServerException 0)))
    ]

-- | Server-side early termination, Trailers-Only case
--
-- This is like 'earlyTermination07', but in this case the server does not send
-- the initial metadata, which causes the server handler to use the gRPC
-- Trailers-Only case to send the error to the client.
earlyTermination08 :: Dialogue
earlyTermination08 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ServerAction $ Terminate (Just (SomeServerException 0)))
    ]

-- | Like 'earlyTermination07', but now without an exception
earlyTermination09 :: Dialogue
earlyTermination09 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ServerAction $ Initiate Map.empty)
    , (0, ServerAction $ Terminate Nothing)
    ]

-- | Client throws after the server sends their initial metadata
earlyTermination10 :: Dialogue
earlyTermination10 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ServerAction $ Initiate Map.empty)
    , (0, ClientAction $ Terminate (Just (SomeClientException 0)))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Like 'earlyTermination10', but server sends a message before closing
earlyTermination11 :: Dialogue
earlyTermination11 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ServerAction $ Initiate Map.empty)
    , (0, ClientAction $ Terminate (Just (SomeClientException 0)))
    , (0, ServerAction $ Send (StreamElem 0))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Client tries to send message but server has already terminated
--
-- This gets sandwiched between another interaction with no unexpected early
-- termination.
--
-- This is a test of the test suite itself: it verifies that we step the test
-- clock correctly in the presence of failures.
earlyTermination12 :: Dialogue
earlyTermination12 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC2))
    , (1, ClientAction $ Initiate (Map.empty, RPC1))
    , (1, ServerAction $ Terminate Nothing)
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Like earlyTermination12, but the sandwich the other way around
--
-- We also send more messages; this is mostly testing the test infrastructure
-- itself (specifically, that the test clock is used correctly).
earlyTermination13 :: Dialogue
earlyTermination13 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ServerAction $ Send (StreamElem 1))
    , (0, ServerAction $ Send (StreamElem 2))
    , (1, ClientAction $ Initiate (Map.empty, RPC1))
    , (1, ServerAction $ Send (NoMoreElems Map.empty))
    , (0, ServerAction $ Send (StreamElem 3))
    , (0, ServerAction $ Terminate (Just (SomeServerException 0)))
    ]

-- | Both the server /and/ the client terminate early
--
-- This is primarily a test of the test infrastructure itself: after the client
-- has terminated, it can no longer green-light the server.
earlyTermination14 :: Dialogue
earlyTermination14 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ClientAction $ Terminate Nothing)
    , (0, ServerAction $ Terminate (Just (SomeServerException 0)))
    ]

{-------------------------------------------------------------------------------
  Dealing correctly with 'AllowHalfClosed'

  NOTE: Once the client puts the connection in half-closed state, if it then
  subsequently crashes, the server may not notice as it is not listening for
  messages from the client anymore (it may notice it when it sends, though).
  For this reason we rule out test cases in which the client terminates "early"
  after having already put the connection in half-closed mode.
-------------------------------------------------------------------------------}

-- | We close the outbound stream in the client
--
-- This was previously causing the connection to break; see 'NoTrailers' for
-- detailed discussion.
allowHalfClosed1 :: Dialogue
allowHalfClosed1 = NormalizedDialogue [
      (1, ClientAction $ Initiate (Map.empty, RPC1))
    , (1, ServerAction $ Send (NoMoreElems Map.empty))
    , (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ServerAction $ Initiate Map.empty)
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Variation on 'allowHalfClosed1' without an explicit server initiation.
allowHalfClosed2 :: Dialogue
allowHalfClosed2 = NormalizedDialogue [
      (1, ClientAction $ Initiate (Map.empty, RPC1))
    , (1, ServerAction $ Send (NoMoreElems Map.empty))
    , (0, ClientAction $ Initiate (Map.empty, RPC1))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]

-- | Server initiates response after client half-closes
allowHalfClosed3 :: Dialogue
allowHalfClosed3 = NormalizedDialogue [
      (0, ClientAction $ Initiate (Map.empty,RPC1))
    , (0, ClientAction $ Terminate (Just (SomeClientException 0)))
    , (0, ServerAction $ Initiate Map.empty)
    , (0, ServerAction $ Send (NoMoreElems Map.empty))
    ]
