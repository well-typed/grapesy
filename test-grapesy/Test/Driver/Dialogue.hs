{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.Dialogue (
    execGlobalSteps
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Thread.Delay
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.Default
import Data.Proxy
import Data.Text qualified as Text
import GHC.Stack
import Test.QuickCheck

import Network.GRPC.Client (Timeout, timeoutToMicro)
import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Common.Binary
import Network.GRPC.Common.CustomMetadata
import Network.GRPC.Common.Exceptions
import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary

import Test.Driver.ClientServer
import Data.ByteString qualified as Strict
import Control.Applicative

{-------------------------------------------------------------------------------
  RPC
-------------------------------------------------------------------------------}

type TestRpc = BinaryRpc "binary" "test"

{-------------------------------------------------------------------------------
  Barrier

  These are concurrent tests, but it can be difficult to pinpoint bugs if the
  ordering of actions is not determistic. We therefore add specific "barrier"
  points, which allow to shrink tests towards more deterministic.
-------------------------------------------------------------------------------}

newtype Barrier = Barrier Int
  deriving stock (Show, Eq, Ord)

newBarrier :: IO (TVar Barrier)
newBarrier = newTVarIO (Barrier 0)

waitForBarrier :: TVar Barrier -> Barrier -> IO ()
waitForBarrier var target = atomically $ do
    actual <- readTVar var
    when (actual < target) retry

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
    -- | Concurrency barrier
    --
    -- See documentation of 'Barrier'.
    LocalBarrier Barrier

    -- | Server initiates response to the client
    --
    -- If there is no explicit 'InitiateResponse', there will be an implicit
    -- one, with empty metadata.
  | InitiateResponse [CustomMetadata]

    -- | Server delay
    --
    -- This simulates a slow server
  | ServerDelay Timeout

    -- | Client sends a message the server
  | ClientToServer (StreamElem () Int)

    -- | Server sends a message to the client
  | ServerToClient (StreamElem [CustomMetadata] Int)

    -- | Client throws an exception
  | ClientException SomeException

    -- | Server throws an exception
  | ServerException SomeException
  deriving stock (Show)

newtype LocalSteps = LocalSteps {
      getLocalSteps :: [LocalStep]
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Many channels (birds-eye view)
-------------------------------------------------------------------------------}

data GlobalStep =
    Spawn (Maybe Timeout) [CustomMetadata] LocalSteps
  | GlobalBarrier Barrier
  deriving stock (Show)

newtype GlobalSteps = GlobalSteps {
      getGlobalSteps :: [GlobalStep]
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Client-side interpretation
-------------------------------------------------------------------------------}

clientGlobal :: TVar Barrier -> Client.Connection -> GlobalSteps -> IO ()
clientGlobal barrierVar conn =
    mapM_ go . getGlobalSteps
  where
    go :: GlobalStep -> IO ()
    go (GlobalBarrier b) = do
        waitForBarrier barrierVar b
    go (Spawn timeout metadata localSteps) =
        void $ forkIO $
          Client.withRPC conn params (Proxy @TestRpc) $ \call ->
            clientLocal barrierVar call timeout localSteps
      where
        params :: Client.CallParams
        params = Client.CallParams {
              callTimeout         = timeout
            , callRequestMetadata = metadata
            }

-- | Model of the server, from the client's point of view
data ServerModel = ServerModel {
      -- | Handler health
      --
      -- This is not directly observable in the client: only when we try to
      -- communicate with the server do we expect to see what the exception is.
      serverHealth :: Maybe SomeException

      -- | Handler execution time, in microseconds
      --
      -- We pretend that time only passes with explicit 'Delay's. This is of
      -- course not true, but provided that the granularity of the timeouts and
      -- sleeps in the test is sufficiently coarse, it is good enough.
      --
      -- This is closely related to 'serverHealth': when 'serverTime' exceeds
      -- the client-specified timeout, the handler is killed and 'serverHealth'
      -- should be set accordingly.
    , serverTime :: Integer
    }

data ServerTimeout = ServerTimeout
  deriving stock (Show)
  deriving anyclass (Exception)

initServerModel :: ServerModel
initServerModel = ServerModel {
      serverHealth = Nothing
    , serverTime   = 0
    }

clientLocal ::
     HasCallStack
  => TVar Barrier
  -> Client.Call TestRpc
  -> Maybe Timeout       -- ^ Timeout specified to the server
  -> LocalSteps
  -> IO ()
clientLocal barrierVar call serverTimeout =
    flip evalStateT initServerModel . mapM_ go . getLocalSteps
  where
    go :: LocalStep -> StateT ServerModel IO ()
    go (LocalBarrier barrier) = liftIO $
        waitForBarrier barrierVar barrier
    go (InitiateResponse expectedMetadata) = liftIO $ do
        receivedMetadata <- atomically $ Client.recvResponseMetadata call
        expect expectedMetadata $ receivedMetadata
    go (ServerDelay timeout) = modify $ \st ->
        let newServerTime = serverTime st + timeoutToMicro timeout
        in st {
               serverTime = newServerTime
             , serverHealth = serverHealth st
                          <|> ( if exceedsTimeout newServerTime
                                  then Just $ toException ServerTimeout
                                  else Nothing
                              )
             }
    go (ClientToServer x) = do
        expected <- adjustExpectation ()
        expect expected =<< liftIO (try $ Client.Binary.sendInput call x)
    go (ServerToClient expectedElem) = do
        expected <- adjustExpectation expectedElem
        expect expected =<< liftIO (try $ Client.Binary.recvOutput call)
    go (ClientException err) =
        throwM err
    go (ServerException err) = modify $ \st -> st {
          serverHealth = serverHealth st <|> Just (toException err)
        }

    -- Adjust expectation when communicating with the server
    --
    -- If the server handler died for some reason, we won't get the regular
    -- result, but should instead see the exception reported to the client.
    adjustExpectation :: a -> StateT ServerModel IO (Either GrpcException a)
    adjustExpectation x = do
        st <- get
        case serverHealth st of
          Nothing  -> return $ Right x
          Just err
              -- The gRPC specification does not actually say anywhere which
              -- gRPC error code should be used for timeouts, but
              -- <https://grpc.io/blog/deadlines/> suggests @CANCELLED@.
            | Just ServerTimeout <- fromException err
            -> return $ Left GrpcException {
                   grpcError         = GrpcCancelled
                 , grpcErrorMessage  = Nothing
                 , grpcErrorMetadata = []
                 }

            | otherwise
            -> return $ Left GrpcException {
                   grpcError         = GrpcUnknown
                 , grpcErrorMessage  = Just $ Text.pack $ show err
                 , grpcErrorMetadata = []
                 }

    exceedsTimeout :: Integer -> Bool
    exceedsTimeout n
      | Just timeout <- serverTimeout
      = n > timeoutToMicro timeout

      | otherwise
      = False

{-------------------------------------------------------------------------------
  Server-side interpretation

  The server-side is slightly different, since the infrastructure spawns
  threads on our behalf (one for each incoming RPC).
-------------------------------------------------------------------------------}

-- | Model of the client, from the server's point of view
--
-- See 'ServerModel' for additional discussion.
data ClientModel = ClientModel {
      clientHealth :: Maybe SomeException
    }

initClientModel :: ClientModel
initClientModel = ClientModel {
      clientHealth = Nothing
    }

serverGlobal ::
     HasCallStack
  => TVar Barrier
  -> MVar GlobalSteps
  -> Server.Call TestRpc
  -> IO ()
serverGlobal barrierVar globalStepsVar call = do
    -- The client will only start new calls from a /single/ thread, so the order
    -- that these come in is determinstic. Here we peel off the next
    -- 'LocalSteps', pending barriers perhaps, whilst holding the lock, to
    -- ensure the same determinism server-side.
        flip evalStateT initClientModel . mapM_ go . getLocalSteps
    =<< modifyMVar globalStepsVar (getNextSteps . getGlobalSteps)
  where
    getNextSteps :: [GlobalStep] -> IO (GlobalSteps, LocalSteps)
    getNextSteps [] =
        throwM $ TestFailure callStack $ UnexpectedRequest
    getNextSteps (GlobalBarrier barrier : global') = do
        waitForBarrier barrierVar barrier
        getNextSteps global'
    getNextSteps (Spawn _timeout expectedMetadata localSteps : global') = do
        -- We don't care about the timeout the client sets; if the server takes
        -- too long (see 'Delay'), the framework should kill the server
        -- and the /client/ will check that it gets the expected exception.
        receivedMetadata <- Server.getRequestMetadata call
        expect expectedMetadata $ receivedMetadata
        return (GlobalSteps global', localSteps)

    go :: LocalStep -> StateT ClientModel IO ()
    go (LocalBarrier barrier) = liftIO $
        waitForBarrier barrierVar barrier
    go (InitiateResponse metadata) = liftIO $ do
        Server.setResponseMetadata call metadata
        void $ Server.initiateResponse call
    go (ServerDelay timeout) = liftIO $
        delay $ timeoutToMicro timeout
    go (ClientToServer expectedElem) = do
        expected <- adjustExpectation expectedElem
        expect expected =<< liftIO (try $ Server.Binary.recvInput call)
    go (ServerToClient x) = do
        expected <- adjustExpectation ()
        expect expected =<< liftIO (try $ Server.Binary.sendOutput call x)
    go (ClientException err) = modify $ \st -> st {
          clientHealth = clientHealth st <|> Just err
        }
    go (ServerException err) =
        throwM err

    -- Adjust expectation when communicating with the client
    --
    -- If the server handler died for some reason, we won't get the regular
    -- result, but should instead see the exception reported to the client.
    adjustExpectation :: a -> StateT ClientModel IO (Either ClientDisconnected a)
    adjustExpectation x = do
        st <- get
        case clientHealth st of
          Nothing   -> return $ Right x
          Just _err -> return $ Left ClientDisconnected

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

execGlobalSteps :: GlobalSteps -> IO ClientServerTest
execGlobalSteps steps = do
    barrierVar     <- newBarrier
    globalStepsVar <- newMVar steps
    return $ def {
        client = \conn -> clientGlobal barrierVar conn steps
      , server = [ Server.mkRpcHandler (Proxy @TestRpc) $
                     serverGlobal barrierVar globalStepsVar
                 ]
      }

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

-- | Generate 'LocalSteps'
--
-- We are careful which actions we generate: the goal of these tests is to
-- ensure correct behaviour of the library, /provided/ the library is used
-- correctly.
--
-- We insert barriers only in shrinking.
instance Arbitrary LocalSteps where
  arbitrary = sized $ \sz -> do
      n <- choose (0, sz)
      fmap LocalSteps $ flip evalStateT initLocalGenState $
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
            do let timeout :: Timeout
                   timeout = Client.Timeout
                               Client.Millisecond
                               (Client.TimeoutValue 1_000)
               return $ ServerDelay timeout
          , ClientException . toException . SomeClientException <$> arbitrary
          , ServerException . toException . SomeServerException <$> arbitrary
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

      genMetadata :: Gen [CustomMetadata]
      genMetadata = do
          n <- choose (0, 2)
          replicateM n $ oneof [
              BinaryHeader <$> genHeaderName <*> genBinaryValue
            , AsciiHeader  <$> genHeaderName <*> genAsciiValue
            ]

      -- TODO: Test invalid names
      -- TODO: Test "awkward" names
      genHeaderName :: Gen HeaderName
      genHeaderName = HeaderName <$> elements ["md1", "md2", "md3"]

      genBinaryValue :: Gen Strict.ByteString
      genBinaryValue = sized $ \sz -> do
          n <- choose (0, sz)
          Strict.pack <$> replicateM n arbitrary

      -- TODO: Test invalid values
      -- TODO: Test "awkward" values
      genAsciiValue :: Gen AsciiValue
      genAsciiValue = AsciiValue <$> elements ["a", "b", "c"]

  shrink = error "todo"

data SomeServerException = SomeServerException Int
  deriving stock (Show)
  deriving anyclass (Exception)

data SomeClientException = SomeClientException Int
  deriving stock (Show)
  deriving anyclass (Exception)
