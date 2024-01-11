{-# LANGUAGE OverloadedStrings #-}

module Test.Prop.Dialogue (tests) where

import Control.Exception
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Show.Pretty

import Network.GRPC.Client (ServerDisconnected(..))
import Network.GRPC.Common
import Network.GRPC.Internal
import Network.GRPC.Server (ClientDisconnected(..))

import Test.Driver.ClientServer
import Test.Driver.Dialogue
import Test.Util.PrettyVal

tests :: TestTree
tests = testGroup "Test.Prop.Dialogue" [
      testGroup "Regression" [
          testCaseInfo "trivial1"           $ regression trivial1
        , testCaseInfo "trivial2"           $ regression trivial2
        , testCaseInfo "trivial3"           $ regression trivial3
        , testCaseInfo "concurrent1"        $ regression concurrent1
        , testCaseInfo "concurrent2"        $ regression concurrent2
        , testCaseInfo "concurrent3"        $ regression concurrent3
        , testCaseInfo "concurrent4"        $ regression concurrent4
        , testCaseInfo "exception1"         $ regression exception1
        , testCaseInfo "exception2"         $ regression exception2
        , testCaseInfo "earlyTermination01" $ regression earlyTermination01
        , testCaseInfo "earlyTermination02" $ regression earlyTermination02
        , testCaseInfo "earlyTermination03" $ regression earlyTermination03
        , testCaseInfo "earlyTermination04" $ regression earlyTermination04
        , testCaseInfo "earlyTermination05" $ regression earlyTermination05
        , testCaseInfo "earlyTermination06" $ regression earlyTermination06
        , testCaseInfo "earlyTermination07" $ regression earlyTermination07
        , testCaseInfo "earlyTermination08" $ regression earlyTermination08
        , testCaseInfo "earlyTermination09" $ regression earlyTermination09
--        , testCaseInfo "earlyTermination10" $ regression earlyTermination10
        ]
    , testGroup "Setup" [
          testProperty "shrinkingWellFounded" prop_shrinkingWellFounded
        ]
    , testGroup "Arbitrary" [
          testProperty "withoutExceptions" arbitraryWithoutExceptions
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
      propClientServer assessCustomException $
        execGlobalSteps globalSteps
  where
    globalSteps :: GlobalSteps
    globalSteps = dialogueGlobalSteps dialogue

regression :: Dialogue -> IO String
regression dialogue = do
    handle annotate $
      testClientServer assessCustomException =<<
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

data ExpectedUserException =
    ExpectedClientException SomeClientException
  | ExpectedServerException SomeServerException
  | ExpectedForwardedToClient GrpcException
  | ExpectedClientDisconnected SomeException
  | ExpectedServerDisconnected SomeException
  | ExpectedEarlyTermination
  deriving stock (Show, GHC.Generic)
  deriving anyclass (PrettyVal)

-- | Custom exceptions
--
-- Technically we should only be expected custom user exceptions when we
-- generate them, but we're not concerned about accidental throws of these
-- custom exceptions.
--
-- TODO: However, it might be useful to be more precise about exactly which
-- gRPC exceptions we expect and when.
assessCustomException :: SomeException -> CustomException ExpectedUserException
assessCustomException err
    --
    -- Custom exceptions
    --

    | Just (userEx :: SomeServerException) <- fromException err
    = CustomExceptionExpected $ ExpectedServerException userEx

    | Just (userEx :: SomeClientException) <- fromException err
    = CustomExceptionExpected $ ExpectedClientException userEx

    -- Server-side exceptions are thrown as 'GrpcException' client-side
    | Just (grpc :: GrpcException) <- fromException err
    , GrpcUnknown <- grpcError grpc
    , Just msg <- grpcErrorMessage grpc
    , "SomeServerException" `Text.isInfixOf` msg
    = CustomExceptionExpected $ ExpectedForwardedToClient grpc

    -- For when the server disconnects /without/ an exception
    --
    -- TODO: Ideally, we should really allow for this only if we have reason to
    -- believe that this case can happen. Currently, this is hiding a previous
    -- bug where the exception from the handler was reported as
    -- 'ServerDisconnected' rather than a 'GrpcException'.
    | Just (ServerDisconnected e) <- fromException err
    = CustomExceptionExpected $
        ExpectedServerDisconnected (innerNestedException e)

    -- Client-side exceptions are reported as 'ClientDisconnected', but without
    -- additional information (gRPC does not support client-to-server trailers
    -- so we have no way of informing the server about what went wrong).
    | Just (ClientDisconnected e) <- fromException err
    = CustomExceptionExpected $
        ExpectedClientDisconnected (innerNestedException e)

    --
    -- Early termination
    --

    | Just (ChannelDiscarded _) <- fromException err
    = CustomExceptionExpected $ ExpectedEarlyTermination
    | Just (grpc :: GrpcException) <- fromException err
    , GrpcUnknown <- grpcError grpc
    , Just msg <- grpcErrorMessage grpc
    , "ChannelDiscarded" `Text.isInfixOf` msg
    = CustomExceptionExpected $ ExpectedForwardedToClient grpc

    --
    -- Catch-all
    --

    | otherwise
    = CustomExceptionUnexpected

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
      (1, ClientAction $ Initiate (Set.fromList [AsciiHeader "md2" "b"], RPC1))
    , (0, ClientAction $ Initiate (Set.fromList [AsciiHeader "md1" "a"], RPC1))
    ]

-- | Test that the final is received
--
-- See also <https://github.com/kazu-yamamoto/http2/issues/86>.
concurrent4 :: Dialogue
concurrent4 = Dialogue [
      (1 , ClientAction $ Send (FinalElem 1 NoMetadata))
    , (0 , ServerAction $ Send (FinalElem 2 (Set.fromList [])))
    , (1 , ServerAction $ Send (FinalElem 3 (Set.fromList [])))
    , (0 , ClientAction $ Send (FinalElem 4 NoMetadata))
    ]

-- | Server-side exception
--
-- NOTE: Since the handler throws an exception without sending /anything/, the
-- exception will be reported to the client using the gRPC trailers-only case.
-- This test also verifies that the client can /receive/ this message (and not
-- the exception thrown by the client API of the http2 library when the handler
-- disappears) -- instead the grpc failure should be raised as a gRPC exception
-- (this is different from when the server fails later).
--
-- TODO: I don't /think/ that behaviour is inconsistent: we cannot connect to
-- the server, we get an exception immediately; if an exception happens later,
-- we only get it during communication; the point being that we might not /need/
-- to communicate any further, and so the exception should not interrupt us.
--
-- TODO: We should test having /multiple/ RPC to the server, on the /same/
-- connection. (Here or as a sanity tests.)
exception1 :: Dialogue
exception1 = Dialogue [
      (0, ClientAction $ Initiate (Set.fromList [], RPC1))
    , (0, ServerAction $ Terminate (Just (ExceptionId 0)))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    ]

-- | Client-side exception
exception2 :: Dialogue
exception2 = Dialogue [
      (0, ClientAction $ Initiate (Set.fromList [], RPC1))
    , (0, ClientAction $ Terminate (Just (ExceptionId 0)))
    , (0, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    ]

-- | Server handler terminates before the client expects it
earlyTermination01 :: Dialogue
earlyTermination01 = Dialogue [
      (0, ClientAction (Initiate (Set.fromList [], RPC1)))
    , (0, ServerAction (Terminate Nothing))
    , (0, ClientAction (Send (NoMoreElems NoMetadata)))
    ]

-- | Client terminates before the server handler expects it
earlyTermination02 :: Dialogue
earlyTermination02 = Dialogue [
      (0, ClientAction $ Initiate (Set.fromList [], RPC1))
    , (0, ClientAction $ Terminate Nothing)
    , (0, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    ]

-- | Variation on early client-side termination that tends to trigger a
-- different code path, where we get a stream error; in @grapesy@ these
-- various kinds of exceptions we can get from @http2@ should all be reported
-- in the same manner (as 'ClientDisconnected' exceptions).
earlyTermination03 :: Dialogue
earlyTermination03 = Dialogue [
      (1, ClientAction $ Initiate (Set.fromList [], RPC1 ))
    , (0, ClientAction $ Initiate (Set.fromList [], RPC1))
    , (1, ClientAction $ Terminate (Just (ExceptionId 0)))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (1, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    , (0, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    ]

-- | Another minor variation on 'earlyTermination03', which tends to trigger yet
-- another codepath
earlyTermination04 :: Dialogue
earlyTermination04 = Dialogue [
      (0, ClientAction $ Initiate (Set.fromList [], RPC1))
    , (0, ServerAction $ Initiate (Set.fromList []))
    , (1, ClientAction $ Initiate (Set.fromList [], RPC1 ))
    , (1, ClientAction $ Terminate (Just (ExceptionId 0)))
    , (1, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    ]

-- Test that early termination in one call does not affect the other. This is
-- currently /only/ true if they use separate connections; see discussion in
-- 'clientGlobal'.
earlyTermination05 :: Dialogue
earlyTermination05 = Dialogue [
      (1, ClientAction $ Initiate (Set.fromList [],RPC1))
    , (1, ServerAction $ Terminate Nothing)
    , (0, ClientAction $ Initiate (Set.fromList [],RPC1))
    , (0, ClientAction $ Send (NoMoreElems NoMetadata))
    , (0, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    , (1, ClientAction $ Send (NoMoreElems NoMetadata))
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
earlyTermination06 = Dialogue [
      (0, ClientAction $ Initiate (Set.fromList [], RPC1))
    , (0, ClientAction $ Send (StreamElem 0))
    , (0, ClientAction $ Terminate (Just (ExceptionId 0)))
    , (0, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    ]

-- | Server-side early termination
earlyTermination07 :: Dialogue
earlyTermination07 = Dialogue [
      (0, ServerAction $ Initiate (Set.fromList []))
    , (0, ServerAction $ Terminate (Just (ExceptionId 0)))
    ]

-- | Server-side early termination, Trailers-Only case
--
-- This is like 'earlyTermination07', but in this case the server does not send
-- the initial metadata, which causes the server handler to use the gRPC
-- Trailers-Only case to send the error to the client.
earlyTermination08 :: Dialogue
earlyTermination08 = Dialogue [
      (0, ServerAction $ Terminate (Just (ExceptionId 0)))
    ]

-- | Like 'earlyTermination07', but now without an exception
earlyTermination09 :: Dialogue
earlyTermination09 = Dialogue [
      (0, ServerAction $ Initiate (Set.fromList []))
    , (0, ServerAction $ Terminate Nothing)
    ]

-- | Client throws after the server sends their initial metadata
_earlyTermination10 :: Dialogue
_earlyTermination10 = Dialogue [
      (0, ClientAction $ Initiate (Set.fromList [], RPC1))
    , (0, ServerAction $ Initiate (Set.fromList []))
    , (0, ClientAction $ Terminate (Just (ExceptionId 1)))
    , (0, ServerAction $ Send (NoMoreElems (Set.fromList [])))
    ]
