{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.Dialogue (
    execGlobalSteps
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.ByteString qualified as Strict
import Data.Default
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Proxy
import Data.Text qualified as Text
import GHC.Stack
import Test.QuickCheck

import Network.GRPC.Client (Timeout)
import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Common.Binary
import Network.GRPC.Common.CustomMetadata
import Network.GRPC.Common.Exceptions
import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary

import Test.Driver.ClientServer

{-------------------------------------------------------------------------------
  RPC
-------------------------------------------------------------------------------}

type TestRpc = BinaryRpc "binary" "test"

{-------------------------------------------------------------------------------
  Test clock

  These are concurrent tests, looking at the behaviour of @n@ concurrent
  connections between a client and a server. To make the interleaving of actions
  easier to see, we introduce a global "test clock". Every test action is
  annotated with a particular "test tick" on this clock. If multiple actions are
  annotated with the same tick, their interleaving is essentially
  non-deterministic.
-------------------------------------------------------------------------------}

newtype TestClock = TestClock (TVar TestClockTick)

newtype TestClockTick = TestClockTick Word
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

withTestClock :: (TestClock -> IO a) -> IO a
withTestClock k = do
    tvar <- newTVarIO (TestClockTick 0)
    withAsync (tickClock tvar) $ \_ ->
      k $ TestClock tvar
  where
    tickClock :: TVar TestClockTick -> IO ()
    tickClock tvar = forever $ do
        threadDelay $ fromIntegral (tickDuration * 1_000)
        atomically $ modifyTVar tvar succ

-- | Tick duration, in millseconds
--
-- Setting this is finicky: too long and the test take a long time to run,
-- too short and the illusion that individual actions take no time fails.
--
-- TODO: For now we set this to 1second, which is almost certainly too long.
tickDuration :: Word
tickDuration = 1_000

waitForTestClock :: MonadIO m => TestClock -> TestClockTick -> m ()
waitForTestClock (TestClock tvar) tick = liftIO $ atomically $ do
    clock <- readTVar tvar
    unless (clock >= tick) retry

{-------------------------------------------------------------------------------
  Timeouts

  We use the test clock also to test timeouts: a slow server is modelled by one
  that waits for a particular test clock tick. To avoid ambiguity, we use
  /even/ clockticks for actions and /odd/ clockticks for timeouts.
-------------------------------------------------------------------------------}

-- | Does the current 'TestClock' time exceed an (optional) timeout?
exceedsTimeout :: TestClockTick -> Maybe TestClockTick -> Bool
now `exceedsTimeout` Just timeout | now == timeout = error "impossible"
now `exceedsTimeout` Just timeout = now > timeout
_   `exceedsTimeout` Nothing      = False

tickToTimeout :: TestClockTick -> Timeout
tickToTimeout (TestClockTick n)
  | even n    = error "tickToTimeout: expected odd timeout"
  | otherwise = Client.Timeout Client.Millisecond $
                  Client.TimeoutValue (n * tickDuration)

{-------------------------------------------------------------------------------
  Test failures
-------------------------------------------------------------------------------}

data TestFailure = TestFailure CallStack Failure
  deriving stock (Show)
  deriving anyclass (Exception)

data Failure =
    -- | Thrown by the server when an unexpected new RPC is initiated
    UnexpectedRequest

    -- | Received an unexpected value
  | Unexpected ReceivedUnexpected
  deriving stock (Show)

data ReceivedUnexpected = forall a. Show a => ReceivedUnexpected {
      expected :: a
    , received :: a
    }

deriving stock instance Show ReceivedUnexpected

expect ::
     (MonadThrow m, Eq a, Show a, HasCallStack)
  => a  -- ^ Expected
  -> a  -- ^ Actually received
  -> m ()
expect expected received
  | expected == received
  = return ()

  | otherwise
  = throwM $ TestFailure callStack $
      Unexpected $ ReceivedUnexpected{expected, received}

{-------------------------------------------------------------------------------
  Single channel

  TODO: We should test that the Trailers-Only case gets triggered if no messages
  were exchanged before the exception (not sure this is observable without
  Wireshark..?).
-------------------------------------------------------------------------------}

data LocalStep =
    -- | Server initiates response to the client
    --
    -- If there is no explicit 'InitiateResponse', there will be an implicit
    -- one, with empty metadata.
    InitiateResponse [CustomMetadata]

    -- | Client sends a message the server
  | ClientToServer (StreamElem () Int)

    -- | Server sends a message to the client
  | ServerToClient (StreamElem [CustomMetadata] Int)

    -- | Client throws an exception
  | ClientException

    -- | Server throws an exception
  | ServerException
  deriving stock (Show)

newtype LocalSteps = LocalSteps {
      getLocalSteps :: [(TestClockTick, LocalStep)]
    }
  deriving stock (Show)

data SomeServerException = SomeServerException TestClockTick
  deriving stock (Show)
  deriving anyclass (Exception)

data SomeClientException = SomeClientException TestClockTick
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Many channels (birds-eye view)
-------------------------------------------------------------------------------}

data GlobalStep =
    -- | Spawn another channel
    --
    -- The timeout (if given) is defined in terms of 'TimeClockTick'.
    Spawn (Maybe TestClockTick) [CustomMetadata] LocalSteps
  deriving stock (Show)

newtype GlobalSteps = GlobalSteps {
      getGlobalSteps :: [(TestClockTick, GlobalStep)]
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Health
-------------------------------------------------------------------------------}

-- | Health
--
-- When the client is expecting a response from the server, it needs to know the
-- "health" of the server, that is, is the server still alive, or did it fail
-- with some kind exception? The same is true for the server when it expects a
-- response from the client. Therefore, the client interpretation keeps track of
-- the health of the server, and vice versa.
data Health e = Alive | Failed e

type ServerHealth = Health SomeServerException
type ClientHealth = Health SomeClientException

{-------------------------------------------------------------------------------
  Client-side interpretation
-------------------------------------------------------------------------------}

clientGlobal :: TestClock -> Client.Connection -> GlobalSteps -> IO ()
clientGlobal testClock conn =
    mapM_ (uncurry go) . getGlobalSteps
  where
    go :: TestClockTick -> GlobalStep -> IO ()
    go tick = \case
        Spawn timeout metadata localSteps -> do
          waitForTestClock testClock tick
          let params :: Client.CallParams
              params = Client.CallParams {
                    callTimeout         = tickToTimeout <$> timeout
                  , callRequestMetadata = metadata
                  }
          void $ forkIO $
            Client.withRPC conn params (Proxy @TestRpc) $ \call ->
              clientLocal testClock call timeout localSteps

clientLocal ::
     HasCallStack
  => TestClock
  -> Client.Call TestRpc
  -> Maybe TestClockTick       -- ^ Timeout specified to the server
  -> LocalSteps
  -> IO ()
clientLocal testClock call serverTimeout =
    flip evalStateT Alive . mapM_ (uncurry go) . getLocalSteps
  where
    go :: TestClockTick -> LocalStep -> StateT ServerHealth IO ()
    go tick = \case
        InitiateResponse expectedMetadata -> liftIO $ do
          receivedMetadata <- atomically $ Client.recvResponseMetadata call
          expect expectedMetadata $ receivedMetadata
        ClientToServer x -> do
          waitForTestClock testClock tick
          expected <- adjustExpectation tick ()
          expect expected =<< liftIO (try $ Client.Binary.sendInput call x)
        ServerToClient expectedElem -> do
          expected <- adjustExpectation tick expectedElem
          expect expected =<< liftIO (try $ Client.Binary.recvOutput call)
        ClientException -> do
          waitForTestClock testClock tick
          throwM $ SomeClientException tick
        ServerException -> do
          previousServerFailure <- adjustExpectation tick ()
          case previousServerFailure of
            Left  _  -> return ()
            Right () -> put $ Failed (SomeServerException tick)

    -- Adjust expectation when communicating with the server
    --
    -- If the server handler died for some reason, we won't get the regular
    -- result, but should instead see the exception reported to the client.
    adjustExpectation :: forall a.
         TestClockTick
      -> a
      -> StateT ServerHealth IO (Either GrpcException a)
    adjustExpectation now x =
        return . aux =<< get
     where
       aux :: ServerHealth -> Either GrpcException a
       aux serverHealth
         -- The gRPC specification does not actually say anywhere which gRPC
         -- error code should be used for timeouts, but
         -- <https://grpc.io/blog/deadlines/> suggests @CANCELLED@.
         | now `exceedsTimeout` serverTimeout
         = Left GrpcException {
               grpcError         = GrpcCancelled
             , grpcErrorMessage  = Nothing
             , grpcErrorMetadata = []
             }

         | Failed err <- serverHealth
         = Left GrpcException {
               grpcError         = GrpcUnknown
             , grpcErrorMessage  = Just $ Text.pack $ show err
             , grpcErrorMetadata = []
             }

         | Alive <- serverHealth
         = Right x

{-------------------------------------------------------------------------------
  Server-side interpretation

  The server-side is slightly different, since the infrastructure spawns
  threads on our behalf (one for each incoming RPC).
-------------------------------------------------------------------------------}

serverGlobal ::
     HasCallStack
  => TestClock
  -> MVar GlobalSteps
  -> Server.Call TestRpc
  -> IO ()
serverGlobal testClock globalStepsVar call = do
        serverLocal testClock call
    =<< modifyMVar globalStepsVar (getNextSteps . getGlobalSteps)
  where
    -- The client starts new calls from a /single/ thread, so the order that
    -- these come in is determinstic. Here we peel off the next 'LocalSteps'
    -- whilst holding the lock to ensure the same determinism server-side.
    getNextSteps :: [(TestClockTick, GlobalStep)] -> IO (GlobalSteps, LocalSteps)
    getNextSteps [] =
        throwM $ TestFailure callStack $ UnexpectedRequest
    -- We don't care about the timeout the client sets; if the server takes too
    -- long, the /client/ will check that it gets the expected exception.
    getNextSteps ((tick, Spawn _timeout metadata localSteps) : global') = do
        receivedMetadata <- Server.getRequestMetadata call
        expect metadata $ receivedMetadata
        waitForTestClock testClock tick
        return (GlobalSteps global', localSteps)

serverLocal ::
     TestClock
  -> Server.Call TestRpc
  -> LocalSteps -> IO ()
serverLocal testClock call =
    flip evalStateT Alive . mapM_ (uncurry go) . getLocalSteps
  where
    go :: TestClockTick -> LocalStep -> StateT ClientHealth IO ()
    go tick = \case
        InitiateResponse metadata -> liftIO $ do
          waitForTestClock testClock tick
          Server.setResponseMetadata call metadata
          void $ Server.initiateResponse call
        ClientToServer expectedElem -> do
          expected <- adjustExpectation expectedElem
          expect expected =<< liftIO (try $ Server.Binary.recvInput call)
        ServerToClient x -> do
          waitForTestClock testClock tick
          expected <- adjustExpectation ()
          expect expected =<< liftIO (try $ Server.Binary.sendOutput call x)
        ServerException -> do
          waitForTestClock testClock tick
          throwM $ SomeServerException tick
        ClientException -> do
          previousClientFailure <- adjustExpectation ()
          case previousClientFailure of
            Left  _  -> return ()
            Right () -> put $ Failed (SomeClientException tick)

    -- Adjust expectation when communicating with the server
    --
    -- If the server handler died for some reason, we won't get the regular
    -- result, but should instead see the exception reported to the client.
    adjustExpectation :: forall a.
         a
      -> StateT ClientHealth IO (Either ClientDisconnected a)
    adjustExpectation x =
        return . aux =<< get
     where
       aux :: ClientHealth -> Either ClientDisconnected a
       aux clientHealth
         | Failed _err <- clientHealth
         = Left ClientDisconnected

         | Alive <- clientHealth
         = Right x

-- | Exception raised in a handler when the client disappeared
--
-- TODO: This is merely wishful thinking at the moment; this needs to move to
-- the main library and be implemented there.
data ClientDisconnected = ClientDisconnected
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

execGlobalSteps :: GlobalSteps -> (ClientServerTest -> IO a) -> IO a
execGlobalSteps steps k = do
    globalStepsVar <- newMVar steps
    withTestClock $ \testClock ->
      k $ def {
        client = \conn -> clientGlobal testClock conn steps
      , server = [ Server.mkRpcHandler (Proxy @TestRpc) $
                     serverGlobal testClock globalStepsVar
                 ]
      }

{-------------------------------------------------------------------------------
  QuickCheck: simple values

  We don't (usually) want to have to look at tests failures with unnecessarily
  complicated names or values, this simply distracts. The only case where we
  /do/ want that is if we are specifically testing parsers/builders.
-------------------------------------------------------------------------------}

simpleHeaderName :: [HeaderName]
simpleHeaderName = ["md1", "md2", "md3"]

simpleAsciiValue :: [AsciiValue]
simpleAsciiValue = ["a", "b", "c"]

{-------------------------------------------------------------------------------
  QuickCheck
-------------------------------------------------------------------------------}

data LocalGenState = LocalGenState {
      generatedInitiateResponse :: Bool
    , clientSentFinalMessage    :: Bool
    , serverSentFinalMessage    :: Bool
    }

initLocalGenState :: LocalGenState
initLocalGenState = LocalGenState {
      generatedInitiateResponse = False
    , clientSentFinalMessage    = False
    , serverSentFinalMessage    = False
    }

-- | Generate 'LocalStep's
--
-- We are careful which actions we generate: the goal of these tests is to
-- ensure correct behaviour of the library, /provided/ the library is used
-- correctly.
--
-- We do not insert timing here; we do this globally.
genLocalSteps :: Gen [LocalStep]
genLocalSteps = sized $ \sz -> do
    n <- choose (0, sz)
    flip evalStateT initLocalGenState $
      replicateM n (StateT go)
  where
    go :: LocalGenState -> Gen (LocalStep, LocalGenState)
    go st = oneof $ concat [
          [ (,st) <$> goStateless ]
        , [ clientSendMessage st
          | not (clientSentFinalMessage st)
          ]
        , [ serverSendMessage st
          | not (serverSentFinalMessage st)
          ]
        , [ do metadata <- genMetadata
               return (
                   InitiateResponse metadata
                 , st { generatedInitiateResponse = True }
                 )
          | not (generatedInitiateResponse st)
          ]
        ]

    -- Steps that do not depend on the 'LocalGenState'
    goStateless :: Gen LocalStep
    goStateless = oneof [
          return ClientException
        , return ServerException
        ]

    -- Precondition: @not clientSentFinalMessage@
    clientSendMessage :: LocalGenState -> Gen (LocalStep, LocalGenState)
    clientSendMessage st = oneof [
           do msg <- genMsg
              return (
                  ClientToServer $ StreamElem msg
                , st
                )
         , do msg <- genMsg
              return (
                  ClientToServer $ FinalElem msg ()
                , st { clientSentFinalMessage = True }
                )
         , do return (
                  ClientToServer $ NoMoreElems ()
                , st { clientSentFinalMessage = True }
                )
        ]

    -- Precondition: @not serverSentFinalMessage@
    serverSendMessage :: LocalGenState -> Gen (LocalStep, LocalGenState)
    serverSendMessage st = oneof [
          do msg <- genMsg
             return (
                 ServerToClient (StreamElem msg)
               , st
               )
        , do msg      <- genMsg
             metadata <- genMetadata
             return (
                 ServerToClient $ FinalElem msg metadata
               , st { serverSentFinalMessage = True }
               )
        , do metadata <- genMetadata
             return (
                 ServerToClient $ NoMoreElems metadata
               , st { serverSentFinalMessage = True }
               )
        ]

    genMsg :: Gen Int
    genMsg = choose (0, 99)

-- | Generate 'GlobalSteps' by assigning timing to local threads
genGlobalSteps :: [[LocalStep]] -> Gen GlobalSteps
genGlobalSteps =
    -- start with clocktick 2, so that we have space for a timeout before
    go [] (TestClockTick 2) . mapMaybe initAcc
  where
    initAcc :: [a] -> Maybe (Maybe b, NonEmpty a)
    initAcc []     = Nothing -- filter out empty threads
    initAcc (s:ss) = Just (Nothing, s :| ss)

    go ::
         -- Fully completed threads
         [(TestClockTick, LocalSteps)]
         -- Next available clock tick (incremented by two each step)
      -> TestClockTick
         -- Not yet completed threads, with an accumulator per thread
      -> [(Maybe (TestClockTick, LocalSteps), NonEmpty LocalStep)]
      -> Gen GlobalSteps
    go completed _   []   = do
        GlobalSteps <$> mapM mkGlobalStep (sortBy (comparing fst) completed)
    go completed now todo = do
        (t1, (mSpawned, left@(s :| ss)), t2) <- isolate todo
        case mSpawned of
          Nothing ->
            go completed
               (succ (succ now))
               (t1 ++ [(Just (now, LocalSteps []), left)] ++ t2)
          Just (spawned, LocalSteps acc) -> do
            let acc' = ((now, s) : acc)
            case ss of
              [] ->
                go ((spawned, LocalSteps (reverse acc')) : completed)
                   (succ (succ now))
                   (t1 ++ t2)
              s':ss' ->
                go completed
                   (succ (succ now))
                   (t1 ++ [(Just (spawned, LocalSteps acc'), s' :| ss')] ++ t2)

    -- Once we have assigned timestamps to all actions, the only thing left to
    -- do is to assign timeouts and initial metadata
    mkGlobalStep ::
         (TestClockTick, LocalSteps)
      -> Gen (TestClockTick, GlobalStep)
    mkGlobalStep (spawned, steps) = do
        metadata   <- genMetadata
        -- Too many timeouts might reduce test coverage
        genTimeout <- frequency [(3, return False), (1, return True)]
        timeout    <- if genTimeout
                        then Just <$> elements (pickTimeout steps)
                        else return Nothing
        return (spawned, Spawn timeout metadata steps)

pickTimeout :: LocalSteps -> [TestClockTick]
pickTimeout = concatMap (\t -> [pred t, succ t]) . map fst . getLocalSteps

genMetadata :: Gen [CustomMetadata]
genMetadata = do
    n <- choose (0, 2)
    replicateM n $ oneof [
        BinaryHeader <$> genHeaderName <*> genBinaryValue
      , AsciiHeader  <$> genHeaderName <*> genAsciiValue
      ]
  where
    -- TODO: Test invalid names
    -- TODO: Test "awkward" names
    -- TODO: Does it matter if there are duplicates..?
    genHeaderName :: Gen HeaderName
    genHeaderName = elements simpleHeaderName

    genBinaryValue :: Gen Strict.ByteString
    genBinaryValue = sized $ \sz -> do
        n <- choose (0, sz)
        Strict.pack <$> replicateM n arbitrary

    -- TODO: Test invalid values
    -- TODO: Test "awkward" values
    genAsciiValue :: Gen AsciiValue
    genAsciiValue = elements simpleAsciiValue

{-------------------------------------------------------------------------------
  Shrinking

  Shrinking is simpler than generation: we need 'LocalGenState' to prevent
  generating some actions /after/ certain other actions, but it's fine to
  /remove/ arbitrary actions from the list.
-------------------------------------------------------------------------------}

shrinkLocalSteps :: LocalSteps -> [LocalSteps]
shrinkLocalSteps =
      map LocalSteps
    . shrinkList (shrinkTimed shrinkLocalStep)
    . getLocalSteps

shrinkLocalStep :: LocalStep -> [LocalStep]
shrinkLocalStep = \case
    InitiateResponse metadata ->
      map InitiateResponse $ shrinkMetadataList metadata
    ClientToServer x ->
      map ClientToServer $ shrinkElem (const []) x
    ServerToClient x ->
      map ServerToClient $ shrinkElem shrinkMetadataList x
    ClientException ->
      []
    ServerException ->
      []

shrinkGlobalStep :: GlobalStep -> [GlobalStep]
shrinkGlobalStep = \case
     Spawn mTimeout metadata steps -> concat [
         [ Spawn mTimeout metadata steps'
         | steps' <- shrinkLocalSteps steps
         ]
       , [ Spawn mTimeout metadata' steps
         | metadata' <- shrinkMetadataList metadata
         ]

         -- Disable timeout
       , [ Spawn Nothing metadata steps
         | Just _timeout <- [mTimeout]
         ]

         -- Timeouts complicate the interpretation of a test, so moving the
         -- timeout later in the test reduces how many we need to consider the
         -- timeout, and hence results in a simpler test
       , [ Spawn (Just timeout') metadata steps
         | Just timeout <- [mTimeout]
         , timeout' <- pickTimeout steps
         , timeout' > timeout
         ]
       ]

-- TODO: If duplicate header names are a problem (see generation), we need to be
-- more careful there too
shrinkMetadataList :: [CustomMetadata] -> [[CustomMetadata]]
shrinkMetadataList = shrinkList shrinkMetadata

shrinkMetadata :: CustomMetadata -> [CustomMetadata]
shrinkMetadata (BinaryHeader nm val) = concat [
      [ BinaryHeader nm' val
      | nm' <- filter (< nm) simpleHeaderName
      ]
    , [ BinaryHeader nm (Strict.pack val')
      | val' <- shrink (Strict.unpack val)
      ]
    , [ AsciiHeader nm val'
      | val' <- simpleAsciiValue
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

shrinkTimed :: (a -> [a]) -> (TestClockTick, a) -> [(TestClockTick, a)]
shrinkTimed f (tick, a) = (tick, ) <$> f a

{-------------------------------------------------------------------------------
  'Arbitrary' instance

  TODO: When we shrink, we might drop steps, but we don't change timings.
  This shouldn't really matter, except that it might mean that the test takes
  longer than it really needs to.
-------------------------------------------------------------------------------}

instance Arbitrary GlobalSteps where
  arbitrary = do
      concurrency <- choose (1, 3)
      threads     <- replicateM concurrency genLocalSteps
      genGlobalSteps threads

  shrink (GlobalSteps steps) =
      GlobalSteps <$> shrinkList (shrinkTimed shrinkGlobalStep) steps

{-------------------------------------------------------------------------------
  Auxiliary: QuickCheck
-------------------------------------------------------------------------------}

isolate :: [a] -> Gen ([a], a, [a])
isolate = \case
    [] -> error "isolate: empty list"
    xs -> do n <- choose (0, length xs - 1)
             return $ go [] xs n
  where
    go :: [a] -> [a] -> Int -> ([a], a, [a])
    go _    []     _ = error "isolate: impossible"
    go prev (x:xs) 0 = (reverse prev, x, xs)
    go prev (x:xs) n = go (x:prev) xs (pred n)

