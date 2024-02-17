{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.Dialogue.Generation (
    Dialogue(..)
  , dialogueGlobalSteps
  , DialogueWithoutExceptions(..)
  , DialogueWithExceptions(..)
  , ensureCorrectUsage
  ) where

import Control.Monad
import Data.ByteString qualified as BS.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics qualified as GHC
import Test.QuickCheck

import Network.GRPC.Common

import Test.Driver.Dialogue.Definition
import Test.Driver.Dialogue.TestClock

{-------------------------------------------------------------------------------
  Metadata

  We don't (usually) want to have to look at tests failures with unnecessarily
  complicated names or values, this simply distracts. The only case where we
  /do/ want that is if we are specifically testing parsers/builders; we do this
  in separate serialization tests ("Test.Prop.Serialization").
-------------------------------------------------------------------------------}

simpleHeaderName :: [HeaderName]
simpleHeaderName = ["md1", "md2", "md3"]

simpleAsciiValue :: [AsciiValue]
simpleAsciiValue = ["a", "b", "c"]

genMetadata :: Gen (Set CustomMetadata)
genMetadata = do
    n <- choose (0, 2)

    -- TODO: For now we avoid generating duplicates; it's not entirely clear
    -- what the semantics of that is.
    names <- replicateM n genHeaderName `suchThat` allDisjoint
    fmap Set.fromList $ forM names $ \nm -> oneof [
        BinaryHeader nm <$> genBinaryValue
      , AsciiHeader  nm <$> genAsciiValue
      ]
  where
    genHeaderName :: Gen HeaderName
    genHeaderName = elements simpleHeaderName

    genBinaryValue :: Gen BinaryValue
    genBinaryValue = sized $ \sz -> do
        n <- choose (0, sz)
        BinaryValue . BS.Strict.pack <$> replicateM n arbitrary

    genAsciiValue :: Gen AsciiValue
    genAsciiValue = elements simpleAsciiValue

{-------------------------------------------------------------------------------
  Local steps
-------------------------------------------------------------------------------}

-- | Generate 'LocalStep's
--
-- We do not insert timing here; we do this globally.
genLocalSteps ::
     Bool  -- ^ Should we generate exceptions?
  -> Gen [LocalStep]
genLocalSteps genExceptions = sized $ \sz -> do
    n <- choose (0, sz)
    (:) <$> genInitStep <*> replicateM n genStep
  where
    genInitStep :: Gen LocalStep
    genInitStep = do
        metadata <- genMetadata
        rpc      <- elements [RPC1, RPC2, RPC3]
        return $ ClientAction $ Initiate (metadata, rpc)

    genStep :: Gen LocalStep
    genStep = frequency [
          (if genExceptions then 1 else 0, genException)
        , (3, ServerAction . Initiate <$> genMetadata)
        , (3, ClientAction . Send     <$> genElem (pure NoMetadata))
        , (3, ServerAction . Send     <$> genElem genMetadata)
        ]

    genException :: Gen LocalStep
    genException = oneof [
          ClientAction . Terminate . Just <$> genExceptionId
        , ServerAction . Terminate . Just <$> genExceptionId
        , pure $ ClientAction . Terminate $ Nothing
        , pure $ ServerAction . Terminate $ Nothing
        ]

    genExceptionId :: Gen ExceptionId
    genExceptionId = ExceptionId <$> choose (0, 5)

    genElem :: Gen b -> Gen (StreamElem b Int)
    genElem genTrailers = oneof [
           StreamElem  <$> genMsg
         , FinalElem   <$> genMsg <*> genTrailers
         , NoMoreElems <$> genTrailers
        ]

    genMsg :: Gen Int
    genMsg = choose (0, 99)

{-------------------------------------------------------------------------------
  Ensure correct library usage

  We have two essentially different kinds of tests: correctness of the library
  given correct library usage, and reasonable error reporting given incorrect
  library usage. We focus on the former here.
-------------------------------------------------------------------------------}

data LocalGenState = LocalGenState {
      clientInitiatedRequest  :: Bool
    , serverInitiatedResponse :: Bool
    , clientTerminated        :: Bool
    , serverTerminated        :: Bool
    }

initLocalGenState :: LocalGenState
initLocalGenState = LocalGenState {
      clientInitiatedRequest  = False
    , serverInitiatedResponse = False
    , clientTerminated        = False
    , serverTerminated        = False
    }

ensureCorrectUsage :: [(Int, LocalStep)] -> [(Int, LocalStep)]
ensureCorrectUsage = go Map.empty []
  where
    go ::
         Map Int LocalGenState  -- State of each concurrent conversation
      -> [(Int, LocalStep)]     -- Accumulator (reverse order)
      -> [(Int, LocalStep)]     -- Still to consider
      -> [(Int, LocalStep)]     -- Result
    go sts acc [] = concat [
          reverse acc
        , concatMap (\(i, st) -> (i,) <$> ensureCleanClose st) $ Map.toList sts
        ]
    go sts acc ((i, s):ss) =
        case s of
          ClientAction action ->
            case action of
              -- Request must be initiated before any messages can be sent
              --
              -- During generation we will generate this 'ClientInitiate' step
              -- as the first step for each channel, which is then interleaved
              -- with all the other steps. If however during shrinking that
              -- 'ClientInitiate' got removed, we have to insert it before the
              -- first action.
              --
              -- We don't have to worry about /multiple/ ClientInitiate messages
              -- (they are not generated).
              Initiate{} ->
                go (upd st{clientInitiatedRequest = True}) ((i, s) : acc) ss

              _ | not (clientInitiatedRequest st) ->
                go sts acc $ (i, ClientAction $ Initiate (Set.empty, RPC1))
                           : (i, s)
                           : ss

              -- After the client has terminated, it cannot execute any other
              -- actions (this is not really about correct usage per se but
              -- simply about sensible tests)

              _ | clientTerminated st
                -> go sts acc ss

              Terminate _ ->
                go (upd st{clientTerminated = True}) ((i, s) : acc) ss

              -- Make sure no messages are sent after the final one

              Send StreamElem{} ->
                go sts ((i, s) : acc) ss

              Send{} ->
                go (upd st{clientTerminated = True}) ((i, s) : acc) ss

              -- Unless we terminated or have not yet started the request the
              -- client, we can always sleep.

              SleepMilli _ ->
                go sts ((i, s): acc) ss

          ServerAction action ->
            case action of
              -- Server actions cannot happen until the client has initiated the
              -- request (before that the handler is not running)

              _ | not (clientInitiatedRequest st) ->
                go sts acc $ (i, ClientAction $ Initiate (Set.empty, RPC1))
                           : (i, s)
                           : ss

              -- After the server has terminates, it can't execute anything else

              _ | serverTerminated st
                -> go sts acc ss

              Terminate _ ->
                go (upd st{serverTerminated = True}) ((i, s) : acc) ss

              -- Response can only be initiated once, and is initiated
              -- implicitly on the first response if not initiated explicitly

              Initiate{} | serverInitiatedResponse st ->
                go sts acc ss

              Initiate{} ->
                go (upd st{serverInitiatedResponse = True}) ((i, s) : acc) ss

              Send{} | not (serverInitiatedResponse st) ->
                go (upd st{serverInitiatedResponse = True}) acc ((i, s) : ss)

              -- Make sure no messages are sent after the final one

              Send StreamElem{} ->
                go sts ((i, s) : acc) ss

              Send{} ->
                go (upd st{serverTerminated = True}) ((i, s) : acc) ss

              -- Unless we terminated or have not yet started the request the
              -- client, we can always sleep.

              SleepMilli _ ->
                go sts ((i, s): acc) ss
      where
        st :: LocalGenState
        st = Map.findWithDefault initLocalGenState i sts

        upd :: LocalGenState -> Map Int LocalGenState
        upd st' = Map.insert i st' sts

    -- Make sure all channels are closed cleanly
    ensureCleanClose :: LocalGenState -> [LocalStep]
    ensureCleanClose st = concat [
          [ ClientAction $ Send $ NoMoreElems NoMetadata
          | clientInitiatedRequest st
          , not $ clientTerminated st
          ]

        , [ ServerAction $ Send $ NoMoreElems Set.empty
          | clientInitiatedRequest st
          , not $ serverTerminated st
          ]
        ]

{-------------------------------------------------------------------------------
  Dialogue

  We generate the steps for each channel separately first, and then choose a
  particular interleaving ('interleave'). Before execution, we then assign
  timings ('assignTimings'), separating the channels again. The advantage of
  this representation is that it is easier to shrink: we can shrink the /global/
  step of steps, whilst keeping the choice of interleaving; that is much harder
  to do when the channels are kept separate.

  In addition, we apply 'ensureCorrectUsage' just before execution. We do /not/
  do this as part of generation/shrinking, as doing so could shrinking
  non-wellfounded (shrinking might remove some action which gets reinserted by
  'ensureCorrectUsage', and shrinking will loop).
-------------------------------------------------------------------------------}

newtype Dialogue = Dialogue {
      getDialogue :: [(Int, LocalStep)]
    }
  deriving stock (Show, Eq, GHC.Generic)

dialogueGlobalSteps :: Dialogue -> GlobalSteps
dialogueGlobalSteps =
      GlobalSteps
    . map LocalSteps
    . assignTimings
    . ensureCorrectUsage
    . getDialogue

-- | Shrink dialogue
--
-- We ignore 'dialogueCleanup', and simply reconstruct it. See 'Dialogue' for
-- further discussion.
shrinkDialogue :: Dialogue -> [Dialogue]
shrinkDialogue =
      map Dialogue
    . shrinkList (shrinkInterleaved shrinkLocalStep)
    . getDialogue

{-------------------------------------------------------------------------------
  Shrinking
-------------------------------------------------------------------------------}

shrinkLocalStep :: LocalStep -> [LocalStep]
shrinkLocalStep = \case
    ClientAction (Initiate (metadata, rpc)) -> concat [
        [ ClientAction $ Initiate (metadata', rpc)
        | metadata' <- shrinkMetadataSet metadata
        ]
      , [ ClientAction $ Initiate (metadata, rpc')
        | rpc' <- shrinkRPC rpc
        ]
      ]
    ServerAction (Initiate metadata) ->
      map (ServerAction . Initiate) $ shrinkMetadataSet metadata
    ClientAction (Send x) ->
      map (ClientAction . Send) $ shrinkElem (const []) x
    ServerAction (Send x) ->
      map (ServerAction . Send) $ shrinkElem shrinkMetadataSet x
    ClientAction (Terminate (Just (ExceptionId exceptionId))) ->
      map (ClientAction . Terminate . Just . ExceptionId) (shrink exceptionId)
    ServerAction (Terminate (Just (ExceptionId exceptionId))) ->
      map (ServerAction . Terminate . Just . ExceptionId) (shrink exceptionId)
    ClientAction (Terminate Nothing) ->
      []
    ServerAction (Terminate Nothing) ->
      []
    ClientAction (SleepMilli _) ->
      []
    ServerAction (SleepMilli _) ->
      []

shrinkRPC :: RPC -> [RPC]
shrinkRPC RPC1 = []
shrinkRPC RPC2 = [RPC1]
shrinkRPC RPC3 = [RPC1, RPC2]

shrinkMetadataSet :: Set CustomMetadata -> [Set CustomMetadata]
shrinkMetadataSet =
      map Set.fromList
    . filter (allDisjoint . map customHeaderName)
    . shrinkList shrinkMetadata
    . Set.toList

shrinkMetadata :: CustomMetadata -> [CustomMetadata]
shrinkMetadata (BinaryHeader nm (BinaryValue val)) = concat [
      -- prefer ASCII headers over binary headers
      [ AsciiHeader nm val'
      | val' <- simpleAsciiValue
      ]
      -- shrink the name
    , [ BinaryHeader nm' (BinaryValue val)
      | nm' <- filter (< nm) simpleHeaderName
      ]
      -- aggressively try to shrink to a single byte
    , [ BinaryHeader nm (BinaryValue (BS.Strict.pack [x]))
      | x:_:_ <- [BS.Strict.unpack val]
      ]
      -- normal shrinking of binary values
    , [ BinaryHeader nm (BinaryValue (BS.Strict.pack val'))
      | val' <- shrink (BS.Strict.unpack val)
      ]
    ]
shrinkMetadata (AsciiHeader nm val) = concat [
      [ AsciiHeader nm' val
      | nm' <- filter (< nm) simpleHeaderName
      ]
    , [ AsciiHeader nm val'
      | val' <- filter (< val) simpleAsciiValue
      ]
    ]

-- TODO: We currently don't change the nature of the elem. Not sure what the
-- right definition of "simpler" is here
shrinkElem :: (a -> [a]) -> StreamElem a Int -> [StreamElem a Int]
shrinkElem _ (StreamElem x) = concat [
      [ StreamElem x'
      | x' <- shrink x
      ]
    ]
shrinkElem f (FinalElem x y) = concat [
      [ FinalElem x' y
      | x' <- shrink x
      ]
    , [ FinalElem x y'
      | y' <- f y
      ]
    ]
shrinkElem f (NoMoreElems y) = concat [
      [ NoMoreElems y'
      | y' <- f y
      ]
    ]

shrinkInterleaved :: (a -> [a]) -> (Int, a) -> [(Int, a)]
shrinkInterleaved f (i, a) = (i, ) <$> f a

{-------------------------------------------------------------------------------
  Arbitrary instance
-------------------------------------------------------------------------------}

newtype DialogueWithoutExceptions = DialogueWithoutExceptions Dialogue
  deriving stock (Show, Eq, GHC.Generic)

newtype DialogueWithExceptions = DialogueWithExceptions Dialogue
  deriving stock (Show, Eq, GHC.Generic)

instance Arbitrary DialogueWithoutExceptions where
  arbitrary = do
      concurrency <- choose (1, 3)
      threads     <- replicateM concurrency $ genLocalSteps False
      DialogueWithoutExceptions . Dialogue <$> interleave threads

  shrink (DialogueWithoutExceptions dialogue) =
      DialogueWithoutExceptions <$> shrinkDialogue dialogue

instance Arbitrary DialogueWithExceptions where
  arbitrary = do
      concurrency <- choose (1, 3)
      threads     <- replicateM concurrency $ genLocalSteps True
      DialogueWithExceptions . Dialogue <$> interleave threads

  shrink (DialogueWithExceptions dialogue) =
      DialogueWithExceptions <$> shrinkDialogue dialogue

{-------------------------------------------------------------------------------
  Auxiliary: QuickCheck
-------------------------------------------------------------------------------}

allDisjoint :: Ord a => [a] -> Bool
allDisjoint xs = Set.size (Set.fromList xs) == length xs
