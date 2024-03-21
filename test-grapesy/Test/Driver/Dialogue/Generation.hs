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
import Data.ByteString qualified as Strict (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Stack
import Test.QuickCheck

import Network.GRPC.Common

import Test.Driver.Dialogue.Definition
import Test.Driver.Dialogue.TestClock qualified as TestClock
import Test.Util (dropEnd)

{-------------------------------------------------------------------------------
  Metadata

  We don't (usually) want to have to look at tests failures with unnecessarily
  complicated names or values, this simply distracts. The only case where we
  /do/ want that is if we are specifically testing parsers/builders; we do this
  in separate serialization tests ("Test.Prop.Serialization").
-------------------------------------------------------------------------------}

simpleHeaderName :: [Strict.ByteString]
simpleHeaderName = ["md1", "md2", "md3"]

simpleAsciiValue :: [Strict.ByteString]
simpleAsciiValue = ["a", "b", "c"]

genMetadata :: Gen Metadata
genMetadata = do
    n <- choose (0, 2)

    names <- replicateM n genHeaderName
    fmap Metadata $ forM names $ \nm -> oneof [
        (\v -> CustomMetadata (BinaryHeader (nm <> "-bin")) v) <$> genBinaryValue
      , (\v -> CustomMetadata (AsciiHeader   nm           ) v) <$> genAsciiValue
      ]
  where
    genHeaderName :: Gen Strict.ByteString
    genHeaderName = elements simpleHeaderName

    genBinaryValue :: Gen Strict.ByteString
    genBinaryValue = sized $ \sz -> do
        n <- choose (0, sz)
        BS.Strict.pack <$> replicateM n arbitrary

    genAsciiValue :: Gen Strict.ByteString
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
          ClientAction . Terminate . Just . SomeClientException <$> choose (0, 5)
        , ServerAction . Terminate . Just . SomeServerException <$> choose (0, 5)
        , pure $ ClientAction . Terminate $ Nothing
        , pure $ ServerAction . Terminate $ Nothing
        ]

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
  library usage. We focus on the former here. For example:

  * We /are/ interested in testing what happens when a client sends a message
    to the server, but the server handler threw an exception.
  * We are /not/ interested in testing what happens when a client tries to send
    another message after having told the server that they sent their last
    message, or after the server told the client that the call is over.

  We also want to make sure the tests are not non-sensical (for example, it does
  not make sense for the client to send a message after it has terminated).
-------------------------------------------------------------------------------}

-- | State of a single RPC call
--
-- We use this to determine when certain actions can happen.
--
-- Invariants:
--
-- * The server response cannot be initiated until the client request has been
--   (until that time the server handler is not even running).
-- * The client request must be closed when the server response closes.
--
-- That second point is a bit subtle. Normally it is the responsibility of both
-- the client and the server to indicate when they won't send any more messages;
-- if they do not, an exception is raised. There is however one exception to
-- this rule: the server can unilaterally decide to close the entire RPC. When
-- this happens, the client (of course) does not have to send any more messages.
-- (Under normal circumstance this does not happen: the server would not close
-- the RPC until the client has closed their end.) We treat the case where the
-- server throws an exception the same: in both cases the RPC is closed and the
-- client does not need to send its final message.
--
-- Of course, this /does/ mean that the client needs to /notice/ that the server
-- has closed the call, even when it's an exception. We therefore implement a
-- 'Terminate' on one side as a receive on the other (see 'clientLocal' and
-- 'serverLocal').
--
-- TODO: We need to update the docs of withRPC.
data LocalGenState = LocalGenState {
      localGenClient :: LocalUniState
    , localGenServer :: LocalUniState
    }
  deriving (Show)

-- | Unidirectional state
--
-- The tests describe a dialogue in a one-sided manner: we talk about /sending/,
-- but not about receiving. In a way \"sending\" is something you /do/, whereas
-- \"receiving\" is something that's done /to/ you. The state we record here is
-- therefore the /outbound/ state only.
data LocalUniState =
    -- | The outbound stream has yet been initialized
    --
    -- Nothing can happen until the client initiates the request. The server can
    -- choose to initiate the response at any point during the conversation.
    UniUninit

    -- | The outbound stream has been established
  | UniOpen

    -- | The outbound stream has been closed
    --
    -- In the case of the client, closing the stream (cleanly) is referred to
    -- in gRPC documentation as \"putting the call in half-closed state\".
    -- In the case of the server, closing the stream involves sending the
    -- trailers and signals the end of the RPC.
    --
    -- The stream is closed \"uncleanly\" if the client or server handler
    -- simply disappears, or when it throws an exception.
  | UniClosed
  deriving (Show)

initLocalGenState :: LocalGenState
initLocalGenState = LocalGenState {
      localGenClient = UniUninit
    , localGenServer = UniUninit
    }

establishInvariant :: HasCallStack => LocalGenState -> LocalGenState
establishInvariant st =
    case (localGenClient st, localGenServer st) of
      (UniUninit , UniUninit) -> st
      (UniUninit , _)         -> error "Response initiated before request"
      (_         , UniClosed) -> st { localGenClient = UniClosed }
      _otherwise              -> st

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
      where
        -- Make sure all channels are closed cleanly
        --
        -- We could do this in two ways: we can either have the server close
        -- the call unilaterally, or we could first have the client close their
        -- end and then the server its own. We opt for the latter, as it is the
        -- more "clean" one, but if a particular test case is generated in which
        -- the client does not close its own end before the server does, that is
        -- also ok (see further discussion in 'LocalGenState').
        ensureCleanClose :: LocalGenState -> [LocalStep]
        ensureCleanClose st = concat [
              [ ClientAction $ Send $ NoMoreElems NoMetadata
              | case localGenClient st of
                  UniUninit -> False
                  UniOpen   -> True
                  UniClosed -> False
              ]
            , [ ServerAction $ Send $ NoMoreElems (Metadata [])
              | case (localGenClient st, localGenServer st) of
                  (UniUninit, _) -> False
                  (_, UniUninit) -> True
                  (_, UniOpen)   -> True
                  (_, UniClosed) -> False
              ]
            ]
    go sts acc ((i, s):ss) =
        case s of
          ClientAction Initiate{} ->
            case localGenClient st of
              UniUninit  -> contWith $ updClient UniOpen
              _otherwise -> skip

          ClientAction Terminate{} ->
            case localGenClient st of
              UniUninit  -> skip
              UniOpen    -> contWith $ updClient UniClosed
              UniClosed  -> skip

          ClientAction (Send (StreamElem _)) ->
            case localGenClient st of
              UniUninit  -> insert $ ClientAction (Initiate (Metadata [], RPC1))
              UniOpen    -> contWith $ id
              UniClosed  -> skip

          ClientAction (Send _) -> -- FinalElem or NoMoreElems
            case localGenClient st of
              UniUninit  -> insert (ClientAction (Initiate (Metadata [], RPC1)))
              UniOpen    -> contWith $ updClient UniClosed
              UniClosed  -> skip

          -- The server cannot do anything until the request is initiated
          -- (until that point the server handler is not even running)
          ServerAction _ | UniUninit <- localGenClient st ->
            skip

          ServerAction Initiate{} ->
            case localGenServer st of
             UniUninit   -> contWith $ updServer UniOpen
             _otherwise  -> skip

          ServerAction (Send (StreamElem _)) ->
            case localGenServer st of
              UniUninit  -> contWith $ updServer UniOpen -- implicitly opened
              UniOpen    -> contWith $ id
              UniClosed  -> skip

          ServerAction (Send _) -> -- FinalElem or NoMoreElems
            case localGenServer st of
              UniClosed  -> skip
              _otherwise -> contWith $ updServer UniClosed

          ServerAction Terminate{} ->
            case localGenServer st of
              UniClosed  -> skip
              _otherwise -> contWith $ updServer UniClosed
      where
        --
        -- Three different ways to continue
        --

        -- 1. Normal case: update the state and move on the next action
        contWith ::
             (Map Int LocalGenState -> Map Int LocalGenState)
          -> [(Int, LocalStep)]
        contWith f = go (Map.map establishInvariant $ f sts) ((i, s) : acc) ss

        -- 2. Skip this action
        skip :: [(Int, LocalStep)]
        skip = go sts acc ss

        -- 3. First execute a different action
        insert :: LocalStep -> [(Int, LocalStep)]
        insert newStep = go sts acc ((i, newStep) : (i, s) : ss)

        --
        -- Updating the state
        --

        st :: LocalGenState
        st = Map.findWithDefault initLocalGenState i sts

        updClient ::
             LocalUniState
          -> Map Int LocalGenState -> Map Int LocalGenState
        updClient client' =
            Map.alter (Just . aux . fromMaybe initLocalGenState) i
          where
            aux :: LocalGenState -> LocalGenState
            aux st' = st' { localGenClient = client' }

        updServer ::
             LocalUniState
          -> Map Int LocalGenState -> Map Int LocalGenState
        updServer server' =
            Map.alter (Just . aux . fromMaybe initLocalGenState) i
          where
            aux :: LocalGenState -> LocalGenState
            aux st' = st' { localGenServer = server' }

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

data Dialogue =
    -- | Dialogue that is already in "correct" form
    --
    -- Calling 'ensureCorrectUsage' on such a dialogue should be a no-op.
    NormalizedDialogue [(Int, LocalStep)]

    -- | Dialogue that may still need to be corrected
  | UnnormalizedDialogue [(Int, LocalStep)]
  deriving stock (Show, Eq)

getNormalizedDialogue :: HasCallStack => Dialogue -> [(Int, LocalStep)]
getNormalizedDialogue (UnnormalizedDialogue steps) = ensureCorrectUsage steps
getNormalizedDialogue (NormalizedDialogue   steps)
  | steps == normalized
  = steps

  | otherwise
  = error $ "getNormalizedDialogue: normal form is " ++ show normalized
  where
    normalized :: [(Int, LocalStep)]
    normalized = ensureCorrectUsage steps

getRawDialogue :: Dialogue -> [(Int, LocalStep)]
getRawDialogue (NormalizedDialogue   steps) = steps
getRawDialogue (UnnormalizedDialogue steps) = steps

dialogueGlobalSteps :: Dialogue -> GlobalSteps
dialogueGlobalSteps =
      GlobalSteps
    . map LocalSteps
    . TestClock.assignTimings
    . getNormalizedDialogue

-- | Shrink dialogue
shrinkDialogue :: Dialogue -> [Dialogue]
shrinkDialogue =
      map UnnormalizedDialogue
    . shrinkList (shrinkInterleaved shrinkLocalStep)
    . getRawDialogue

{-------------------------------------------------------------------------------
  Shrinking
-------------------------------------------------------------------------------}

shrinkLocalStep :: LocalStep -> [LocalStep]
shrinkLocalStep = \case
    ClientAction (Initiate (metadata, rpc)) -> concat [
        [ ClientAction $ Initiate (metadata', rpc)
        | metadata' <- shrinkMetadata metadata
        ]
      , [ ClientAction $ Initiate (metadata, rpc')
        | rpc' <- shrinkRPC rpc
        ]
      ]
    ServerAction (Initiate metadata) ->
      map (ServerAction . Initiate) $ shrinkMetadata metadata
    ClientAction (Send x) ->
      map (ClientAction . Send) $ shrinkElem (const []) x
    ServerAction (Send x) ->
      map (ServerAction . Send) $ shrinkElem shrinkMetadata x
    ClientAction (Terminate (Just (SomeClientException n))) ->
      map (ClientAction . Terminate . Just . SomeClientException) (shrink n)
    ServerAction (Terminate (Just (SomeServerException n))) ->
      map (ServerAction . Terminate . Just . SomeServerException) (shrink n)
    ClientAction (Terminate Nothing) ->
      []
    ServerAction (Terminate Nothing) ->
      []

shrinkRPC :: RPC -> [RPC]
shrinkRPC RPC1 = []
shrinkRPC RPC2 = [RPC1]
shrinkRPC RPC3 = [RPC1, RPC2]

shrinkMetadata :: Metadata -> [Metadata]
shrinkMetadata =
      map Metadata
    . shrinkList shrinkCustomMetadata
    . getMetadata

shrinkCustomMetadata :: CustomMetadata -> [CustomMetadata]
shrinkCustomMetadata (CustomMetadata (BinaryHeader name) val) = concat [
      -- prefer ASCII headers over binary headers
      [ CustomMetadata (AsciiHeader nameWithoutSuffix) val'
      | val' <- simpleAsciiValue
      ]
      -- shrink the name
    , [ CustomMetadata (BinaryHeader (nm' <> "-bin")) val
      | nm' <- filter (< nameWithoutSuffix) simpleHeaderName
      ]
      -- aggressively try to shrink to a single byte
    , [ CustomMetadata (BinaryHeader name) (BS.Strict.pack [x])
      | x:_:_ <- [BS.Strict.unpack val]
      ]
      -- normal shrinking of binary values
    , [ CustomMetadata (BinaryHeader name) (BS.Strict.pack val')
      | val' <- shrink (BS.Strict.unpack val)
      ]
    ]
  where
    nameWithoutSuffix :: Strict.ByteString
    nameWithoutSuffix = dropEnd 4 name
shrinkCustomMetadata (CustomMetadata (AsciiHeader name) val) = concat [
      [ CustomMetadata (AsciiHeader name') val
      | name' <- filter (< name) simpleHeaderName
      ]
    , [ CustomMetadata (AsciiHeader name) val'
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
  deriving stock (Show, Eq)

newtype DialogueWithExceptions = DialogueWithExceptions Dialogue
  deriving stock (Show, Eq)

instance Arbitrary DialogueWithoutExceptions where
  arbitrary = do
      concurrency <- choose (1, 3)
      threads     <- replicateM concurrency $ genLocalSteps False
      DialogueWithoutExceptions . UnnormalizedDialogue <$>
        TestClock.interleave threads

  shrink (DialogueWithoutExceptions dialogue) =
      DialogueWithoutExceptions <$> shrinkDialogue dialogue

instance Arbitrary DialogueWithExceptions where
  arbitrary = do
      concurrency <- choose (1, 3)
      threads     <- replicateM concurrency $ genLocalSteps True
      DialogueWithExceptions . UnnormalizedDialogue <$>
        TestClock.interleave threads

  shrink (DialogueWithExceptions dialogue) =
      DialogueWithExceptions <$> shrinkDialogue dialogue
