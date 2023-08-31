{-# LANGUAGE OverloadedStrings #-}

module Test.Prop.Dialogue (tests) where

import Control.Exception
import Data.Set qualified as Set
import GHC.Generics qualified as GHC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Show.Pretty

import Network.GRPC.Common

import Test.Driver.ClientServer
import Test.Driver.Dialogue
import Test.Util.PrettyVal

tests :: TestTree
tests = testGroup "Test.Prop.Dialogue" [
      testGroup "Regression" [
          testCaseInfo "trivial1"    $ regression trivial1
        , testCaseInfo "trivial2"    $ regression trivial2
        , testCaseInfo "trivial3"    $ regression trivial3
        , testCaseInfo "concurrent1" $ regression concurrent1
        , testCaseInfo "concurrent2" $ regression concurrent2
        , testCaseInfo "concurrent3" $ regression concurrent3
        , testCaseInfo "concurrent4" $ regression concurrent4
        ]
    , testGroup "Setup" [
          testProperty "shrinkingWellFounded" prop_shrinkingWellFounded
        ]
    , testGroup "Arbitrary" [
          testProperty "withoutExceptions" arbitraryWithoutExceptions
          -- TODO: Enable:
--        , testProperty "withExceptions"    arbitraryWithExceptions
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

_arbitraryWithExceptions :: DialogueWithExceptions -> Property
_arbitraryWithExceptions (DialogueWithExceptions dialogue) =
    propDialogue dialogue

propDialogue :: Dialogue -> Property
propDialogue dialogue =
    counterexample (show globalSteps) $
      propClientServer $ execGlobalSteps globalSteps
  where
    globalSteps :: GlobalSteps
    globalSteps = dialogueGlobalSteps dialogue

regression :: Dialogue -> IO String
regression dialogue = do
    handle annotate $
      testClientServer $
        execGlobalSteps globalSteps
  where
    globalSteps :: GlobalSteps
    globalSteps = dialogueGlobalSteps dialogue

    annotate :: SomeException -> IO String
    annotate e = throwIO $ RegressionFailed e globalSteps

data RegressionFailed = RegressionFailed {
      regressionFailedException :: SomeException
    , regressionFailedSteps     :: GlobalSteps
    }
  deriving stock (GHC.Generic)
  deriving anyclass (Exception, PrettyVal)
  deriving Show via ShowAsPretty RegressionFailed

{-------------------------------------------------------------------------------
  Regression tests
-------------------------------------------------------------------------------}

-- | Sanity check: server tells client immediately that there is nothing to say
--
-- One of the things this test verifies (apart from the testing infrastructure
-- itself) is that the server gets the chance to terminate cleanly.
trivial1 :: Dialogue
trivial1 = Dialogue [
      (0, ClientAction $ Initiate (Set.fromList [], RPC1))
    , (0, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
      -- Prevent client from closing the connection immediately:
    , (0, ClientAction $ SleepMilli 1)
    ]

-- | Like 'trivial1', but without the 'Sleep' (so that the client /does/
-- close the connection immediately)
trivial2 :: Dialogue
trivial2 = Dialogue [
      (0, ClientAction $ Initiate (Set.fromList [], RPC1))
    , (0, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    ]

-- | Variation on 'trivial1' where the client sends a message after the
-- server closed their end
trivial3 :: Dialogue
trivial3 = Dialogue [
      (0, ClientAction $ Initiate (Set.fromList [], RPC1))
    , (0, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    , (0, ClientAction $ Send (StreamElem 1234))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
      -- Prevent client from closing the connection immediately:
    , (0, ClientAction $ SleepMilli 1)
    ]

-- | Verify that the test infrastructure does not confuse client/server threads
--
-- It's trickier than one might think to make sure that the server handler and
-- a client agree on a set of steps to execute. This test, along with
-- 'concurrent2' and 'concurrent3', and sanity checks to make sure that this
-- goes well.
concurrent1 :: Dialogue
concurrent1 = Dialogue [
      (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (1, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    ]

concurrent2 :: Dialogue
concurrent2 = Dialogue [
      (1, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    ]

concurrent3 :: Dialogue
concurrent3 = Dialogue [
      (1, ClientAction $ Initiate  (Set.fromList [AsciiHeader "md2" "b"], RPC1))
    , (0, ClientAction $ Initiate  (Set.fromList [AsciiHeader "md1" "a"], RPC1))
    ]

-- | Test that the final is received
--
-- See also <https://github.com/kazu-yamamoto/http2/issues/86>.
concurrent4 :: Dialogue
concurrent4 = Dialogue [
      ( 1 , ClientAction (Send (FinalElem 1 NoMetadata)) )
    , ( 0 , ServerAction (Send (FinalElem 2 (Set.fromList []))) )
    , ( 1 , ServerAction (Send (FinalElem 3 (Set.fromList []))) )
    , ( 0 , ClientAction (Send (FinalElem 4 NoMetadata)) )
    ]
