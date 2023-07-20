{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.Dialogue.Execution (
    execGlobalSteps
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.Bifunctor
import Data.Default
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Proxy
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Text.Show.Pretty

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary

import Test.Driver.ClientServer
import Test.Driver.Dialogue.Definition
import Test.Driver.Dialogue.TestClock
import Test.Util.PrettyVal
import System.IO.Unsafe (unsafePerformIO)

{-------------------------------------------------------------------------------
  Endpoints
-------------------------------------------------------------------------------}

type TestRpc1 = BinaryRpc "dialogue" "test1"
type TestRpc2 = BinaryRpc "dialogue" "test2"
type TestRpc3 = BinaryRpc "dialogue" "test3"

withProxy ::
     RPC
  -> (forall serv meth.
          IsRPC (BinaryRpc serv meth)
       => Proxy (BinaryRpc serv meth)
       -> a)
  -> a
withProxy RPC1 k = k (Proxy @TestRpc1)
withProxy RPC2 k = k (Proxy @TestRpc2)
withProxy RPC3 k = k (Proxy @TestRpc3)

{-------------------------------------------------------------------------------
  Test failures
-------------------------------------------------------------------------------}

data TestFailure = TestFailure PrettyCallStack Failure
  deriving stock (GHC.Generic)
  deriving anyclass (Exception, PrettyVal)
  deriving Show via ShowAsPretty TestFailure

data Failure =
    -- | Thrown by the server when an unexpected new RPC is initiated
    UnexpectedRequest

    -- | Received an unexpected value
  | Unexpected ReceivedUnexpected
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Exception, PrettyVal)

data ReceivedUnexpected = forall a. (Show a, PrettyVal a) => ReceivedUnexpected {
      expected :: a
    , received :: a
    }

deriving stock instance Show ReceivedUnexpected

instance PrettyVal ReceivedUnexpected where
  prettyVal (ReceivedUnexpected{expected, received}) =
      Rec "ReceivedUnexpected" [
          ("expected", prettyVal expected)
        , ("received", prettyVal received)
        ]

expect ::
     (MonadThrow m, Eq a, Show a, PrettyVal a, HasCallStack)
  => a  -- ^ Expected
  -> a  -- ^ Actually received
  -> m ()
expect expected received
  | expected == received
  = return ()

  | otherwise
  = throwM $ TestFailure prettyCallStack $
      Unexpected $ ReceivedUnexpected{expected, received}

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
data Health e a = Alive a | Failed e | Disappeared

type ServerHealth = Health SomeServerException
type ClientHealth = Health SomeClientException

instance Functor (Health e) where
  fmap  = liftM
instance Applicative (Health e) where
  pure  = Alive
  (<*>) = ap
instance Monad (Health e) where
  return = pure
  Alive x     >>= f = f x
  Failed e    >>= _ = Failed e
  Disappeared >>= _ = Disappeared

ifAlive :: (a -> Health e b) -> Health e a -> Health e b
ifAlive = (=<<)

{-------------------------------------------------------------------------------
  Exceptions

  When the dialogue calls for the client or the server to throw an exception,
  we throw one of these. Their sole purpose is to be "any" kind of exception
  (not a specific one).
-------------------------------------------------------------------------------}

data SomeServerException = SomeServerException ExceptionId
  deriving stock (Show)
  deriving anyclass (Exception)

data SomeClientException = SomeClientException ExceptionId
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Client-side interpretation
-------------------------------------------------------------------------------}

clientLocal ::
     HasCallStack
  => TestClock
  -> Client.Call (BinaryRpc meth srv)
  -> LocalSteps
  -> IO ()
clientLocal testClock call = \(LocalSteps steps) ->
    flip evalStateT (Alive ()) $ go steps
  where
    go :: [(TestClockTick, LocalStep)] -> StateT (ServerHealth ()) IO ()
    go []                     = return ()
    go ((tick, step) : steps) = do
        waitFor "client"
        case step of
          ClientAction action -> do
            liftIO $ waitForTestClockTick testClock tick
            continue <-   goClient action
                        `finally`
                          liftIO (advanceTestClock testClock)
            when continue $ go steps
          ServerAction action -> do
            goServer action
            go steps

    -- Client action
    --
    -- Returns 'True' if we should continue executing more actions, or
    -- exit (thereby closing the RPC call)
    goClient ::
         Action (Metadata, RPC) NoMetadata
      -> StateT (ServerHealth ()) IO Bool
    goClient = \case
        Initiate _ ->
          error "clientLocal: unexpected Initiate"
        Send x -> do
          expected <- adjustExpectation ()
          expect expected =<< liftIO (try $ Client.Binary.sendInput call x)

          -- This is making a difference:  we see an extra frame, that is
          -- termating the stream. However, the server recvMessageLoop for some
          -- reason is not receiving it. I don't know why.
          --
          -- Without this, we see GO_AWAY instead, which i guess on the server
          -- side is killing the handler, without it getting a chance to
          -- intercept it. At this point, recvMessageLoop will die because the
          -- call to getChunk is blocked indefinitely.
          return True
        Terminate (Just exceptionId) -> do
          throwM $ SomeClientException exceptionId
        Terminate Nothing ->
          -- This is a sign to the server that the client will terminate; we
          -- don't have to do anything here.
          return False
        SleepMilli n -> do
          liftIO $ threadDelay (n * 1_000)
          return True

    goServer ::
           Action Metadata Metadata
        -> StateT (ServerHealth ()) IO ()
    goServer = \case
        Initiate expectedMetadata -> liftIO $ do
          receivedMetadata <- atomically $ Client.recvResponseMetadata call
          expect expectedMetadata $ Set.fromList receivedMetadata
        Send (FinalElem a b) -> do
          -- Known bug (limitation in http2). See recvMessageLoop.
          goServer $ Send (StreamElem a)
          goServer $ Send (NoMoreElems b)
        Send expectedElem -> do
          expected <- adjustExpectation expectedElem
          received <- liftIO . try $
                        fmap (first Set.fromList) $
                          Client.Binary.recvOutput call
          expect expected received
        Terminate (Just exceptionId) -> do
          previousServerFailure <- adjustExpectation ()
          case previousServerFailure of
            Left  _  -> return ()
            Right () -> put $ Failed $ SomeServerException exceptionId
        Terminate Nothing ->
          -- The server disappearing won't be noticed in the client until we
          -- try to send or receive a message
          modify $ ifAlive $ \() -> Disappeared
        SleepMilli _ ->
          return ()

    -- Adjust expectation when communicating with the server
    --
    -- If the server handler died for some reason, we won't get the regular
    -- result, but should instead see the exception reported to the client.
    adjustExpectation :: forall a.
         a
      -> StateT (ServerHealth ()) IO (Either GrpcException a)
    adjustExpectation x =
        return . aux =<< get
     where
       aux :: ServerHealth () -> Either GrpcException a
       aux serverHealth =
           case serverHealth of
             Failed err ->
               Left GrpcException {
                   grpcError         = GrpcUnknown
                 , grpcErrorMessage  = Just $ Text.pack $ show err
                 , grpcErrorMetadata = []
                 }
             Disappeared ->
               -- TODO: Not really sure what exception we get here actually
               Left GrpcException {
                   grpcError         = GrpcUnknown
                 , grpcErrorMessage  = Just "TODO"
                 , grpcErrorMetadata = []
                 }
             Alive () ->
               Right x

clientGlobal :: TestClock -> Client.Connection -> GlobalSteps -> IO ()
clientGlobal testClock conn = \(GlobalSteps globalSteps) ->
    go [] globalSteps
  where
    go :: [Async ()] -> [LocalSteps] -> IO ()
    go threads [] = do
         -- Wait for all threads to finish
         --
         -- This also ensures that if any of these threads threw an exception,
         -- that is now rethrown here in the main test.
        mapM_ wait threads
    go threads (c:cs) =
        withAsync (runLocalSteps c) $ \newThread ->
          go (newThread:threads) cs

    runLocalSteps :: LocalSteps -> IO ()
    runLocalSteps (LocalSteps steps) = do
        case steps of
          (tick, ClientAction (Initiate (metadata, rpc))) : steps' -> do
            waitForTestClockTick testClock tick

            -- Timeouts are outside the scope of these tests: it's too finicky
            -- to relate timeouts (in seconds) to specific test execution.
            -- We do test exceptions in general here; the specific exception
            -- arising from a timeout we test elsewhere.
            let params :: Client.CallParams
                params = def {
                    Client.callRequestMetadata = Set.toList metadata
                  }

            withProxy rpc $ \proxy ->
              Client.withRPC conn params proxy $ \call -> do
                -- We wait for the /server/ to advance the test clock (so that
                -- we are use the next step doesn't happen until the connection
                -- is established).
                clientLocal testClock call (LocalSteps steps')

          _otherwise ->
            error $ "clientGlobal: expected Initiate, got " ++ show steps

{-------------------------------------------------------------------------------
  Debugging: control who is allowed to take a step
-------------------------------------------------------------------------------}

promptLock :: MVar ()
{-# NOINLINE promptLock #-}
promptLock = unsafePerformIO $ newMVar ()

promptVar :: TVar [String]
{-# NOINLINE promptVar #-}
promptVar = unsafePerformIO $ newTVarIO []

waitFor :: MonadIO m => String -> m ()
-- waitFor = _waitForEnabled
waitFor _ = return ()

_waitForEnabled :: MonadIO m => String -> m ()
_waitForEnabled label = liftIO $ do
    -- We spawn a prompt for each call to 'waitFor': @n@ calls to 'waitFor' will
    -- need @n@ prompts. The order doesn't matter, each prompt is equivalent to
    -- every other, but we don't want multiple prompts at once, so we require
    -- that we hold the 'promptLock'.
    _ <- forkIO $ do
      line <- withMVar promptLock $ \() -> do
        let loop :: IO String
            loop = do
                str <- getLine
                if null str
                  then loop
                  else return str
        loop
      atomically $ modifyTVar promptVar (line :)
    -- Only consume our own prompt
    putStrLn $ "Waiting for " ++ show label
    atomically $ do
      prompts <- readTVar promptVar
      case break (== label) prompts of
        (_ , []   ) -> retry
        (ps, _:ps') -> writeTVar promptVar (ps ++ ps')
    putStrLn $ "Got " ++ show label

{-------------------------------------------------------------------------------
  Server-side interpretation

  The server-side is slightly different, since the infrastructure spawns
  threads on our behalf (one for each incoming RPC).
-------------------------------------------------------------------------------}

serverLocal ::
     TestClock
  -> Server.Call (BinaryRpc serv meth)
  -> LocalSteps -> IO ()
serverLocal testClock call = \(LocalSteps steps) -> do
    flip evalStateT (Alive ()) $ go steps
  where
    go :: [(TestClockTick, LocalStep)] -> StateT (ClientHealth ()) IO ()
    go []                     = return ()
    go ((tick, step) : steps) = do
        waitFor "server"
        case step of
          ServerAction action -> do
            liftIO $ waitForTestClockTick testClock tick
            continue <-   goServer action
                        `finally`
                          liftIO (advanceTestClock testClock)
            when continue $ go steps
          ClientAction action -> do
            goClient action
            go steps

    -- Server action
    --
    -- Returns 'True' if we should continue executing the other actions, or
    -- terminate (thereby terminating the handler)
    goServer ::
         Action Metadata Metadata
      -> StateT (ClientHealth ()) IO Bool
    goServer = \case
        Initiate metadata -> liftIO $ do
          Server.setResponseMetadata call (Set.toList metadata)
          void $ Server.initiateResponse call
          return True
        Send x -> do
          expected <- adjustExpectation ()
          received <- try . liftIO $
                        Server.Binary.sendOutput call (first Set.toList x)
          expect expected received
          return True
        Terminate (Just exceptionId) -> do
          throwM $ SomeServerException exceptionId
        Terminate Nothing ->
          -- Nothing to do; this is simply the last instruction we execute
          return False
        SleepMilli n -> do
          liftIO $ threadDelay (n * 1_000)
          return True

    goClient ::
           Action (Metadata, RPC) NoMetadata
        -> StateT (ClientHealth ()) IO ()
    goClient = \case
        Initiate _ ->
          error "serverLocal: unexpected ClientInitiateRequest"
        Send (FinalElem a b) -> do
          -- Known bug (limitation in http2). See recvMessageLoop.
          goClient $ Send (StreamElem a)
          goClient $ Send (NoMoreElems b)
        Send expectedElem -> do
          expected <- adjustExpectation expectedElem
          expect expected =<< liftIO (try $ Server.Binary.recvInput call)
        Terminate (Just exceptionId) -> do
          previousClientFailure <- adjustExpectation ()
          case previousClientFailure of
            Left  _  -> return ()
            Right () -> put $ Failed $ SomeClientException exceptionId
        Terminate Nothing -> do
          -- The server disappearing won't be noticed in the client until we
          -- try to send or receive a message
          modify $ ifAlive $ \() -> Disappeared
        SleepMilli _ ->
          return ()

    -- Adjust expectation when communicating with the server
    --
    -- If the server handler died for some reason, we won't get the regular
    -- result, but should instead see the exception reported to the client.
    adjustExpectation :: forall a.
         a
      -> StateT (ClientHealth ()) IO (Either ClientDisconnected a)
    adjustExpectation x =
        return . aux =<< get
     where
       aux :: ClientHealth () -> Either ClientDisconnected a
       aux clientHealth =
           case clientHealth of
             Failed _err ->
               Left ClientDisconnected
             Disappeared ->
               Left ClientDisconnected
             Alive () ->
               Right x

serverGlobal ::
     HasCallStack
  => TestClock
  -> MVar GlobalSteps
    -- ^ Unlike in the client case, the grapesy infrastructure spawns a new
    -- thread for each incoming connection. To know which part of the test this
    -- particular handler corresponds to, we take the next 'LocalSteps' from
    -- this @MVar@. Since all requests are started by the client from /one/
    -- thread, the order of these incoming requests is deterministic.
  -> Server.Call (BinaryRpc serv meth)
  -> IO ()
serverGlobal testClock globalStepsVar call = do
    steps <- modifyMVar globalStepsVar (getNextSteps . getGlobalSteps)
    -- See discussion in clientGlobal (runLocalSteps)
    advanceTestClock testClock

    handle (annotate steps) $ do
     case getLocalSteps steps of
       -- It is important that we do this 'expect' outside the scope of the
       -- @modifyMVar@: if we do not, then if the expect fails, we'd leave the
       -- @MVar@ unchanged, and the next request would use the wrong steps.
       (_tick, ClientAction (Initiate (metadata, _rpc))) : steps' -> do
         -- We don't care about the timeout the client sets; if the server
         -- takes too long, the /client/ will check that it gets the expected
         -- exception.
         receivedMetadata <- Server.getRequestMetadata call
         expect metadata $ Set.fromList receivedMetadata
         serverLocal testClock call $ LocalSteps steps'
       _otherwise ->
          error "serverGlobal: expected ClientInitiateRequest"
  where
    getNextSteps :: [LocalSteps] -> IO (GlobalSteps, LocalSteps)
    getNextSteps [] = do
        throwM $ TestFailure prettyCallStack $ UnexpectedRequest
    getNextSteps (LocalSteps steps:global') =
        return (GlobalSteps global', LocalSteps steps)

    annotate :: LocalSteps -> SomeException -> IO ()
    annotate steps err = throwM $ ServerGlobalException {
          serverGlobalException          = err
        , serverGlobalExceptionSteps     = steps
        , serverGlobalExceptionCallStack = prettyCallStack
        }

-- | Exception raised in a handler when the client disappeared
--
-- TODO: This is merely wishful thinking at the moment; this needs to move to
-- the main library and be implemented there.
data ClientDisconnected = ClientDisconnected
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (Exception, PrettyVal)

-- | Annotated server handler exception
--
-- When a server handler throws an exception, it is useful to know what that
-- particular handler was executing.
data ServerGlobalException = ServerGlobalException {
       serverGlobalException          :: SomeException
     , serverGlobalExceptionSteps     :: LocalSteps
     , serverGlobalExceptionCallStack :: PrettyCallStack
     }
  deriving stock (GHC.Generic)
  deriving anyclass (Exception, PrettyVal)
  deriving Show via ShowAsPretty ServerGlobalException

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

execGlobalSteps :: GlobalSteps -> (ClientServerTest -> IO a) -> IO a
execGlobalSteps steps k = do
    globalStepsVar <- newMVar (order steps)
    testClock      <- newTestClock

    let handler ::
             IsRPC (BinaryRpc serv meth)
          => Proxy (BinaryRpc serv meth) -> Server.RpcHandler IO
        handler rpc = Server.mkRpcHandler rpc $ \call ->
                        serverGlobal testClock globalStepsVar call

    mRes :: Either SomeException a <- try $ k $ def {
        client = \conn -> clientGlobal testClock conn steps
      , server = [ handler (Proxy @TestRpc1)
                 , handler (Proxy @TestRpc2)
                 , handler (Proxy @TestRpc3)
                 ]
      }
    case mRes of
      Left err -> throwM err
      Right a  -> return a
  where
    -- For 'clientGlobal' the order doesn't matter, because it spawns a thread
    -- for each 'LocalSteps'. The server however doesn't get this option; the
    -- threads /get/ spawnwed for each incoming connection, and must feel off
    -- the appropriate steps. It's therefore important that it will get these
    -- in the order that they come in.
    order :: GlobalSteps -> GlobalSteps
    order (GlobalSteps threads) = GlobalSteps $
        sortBy (comparing firstTick) threads
     where
       firstTick :: LocalSteps -> TestClockTick
       firstTick (LocalSteps []) =
           error "execGlobalSteps: unexpected empty LocalSteps"
       firstTick (LocalSteps ((tick, _):_)) =
           tick
