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
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Proxy
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
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
      received :: a
    }

deriving stock instance Show ReceivedUnexpected

instance PrettyVal ReceivedUnexpected where
  prettyVal (ReceivedUnexpected{received}) =
      Rec "ReceivedUnexpected" [
          ("received", prettyVal received)
        ]

expect ::
     (MonadThrow m, Show a, PrettyVal a, HasCallStack)
  => (a -> Bool)  -- ^ Expected
  -> a            -- ^ Actually received
  -> m ()
expect isExpected received
  | isExpected received
  = return ()

  | otherwise
  = throwM $ TestFailure prettyCallStack $
      Unexpected $ ReceivedUnexpected{received}

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
  deriving stock (Show)

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
            continue <-   clientAct action
                        `finally`
                          liftIO (advanceTestClock testClock)
            if continue then
              go steps
            else do
              -- See discussion in serverLocal
              let ourStep :: (TestClockTick, LocalStep) -> Maybe TestClockTick
                  ourStep (tick' , ClientAction _) = Just tick'
                  ourStep (_     , ServerAction _) = Nothing
              liftIO $ do
                void $ forkIO $
                  advanceTestClockAtTimes testClock $
                    mapMaybe ourStep steps
          ServerAction action -> do
            reactToServer action
            go steps

    -- Client action
    --
    -- Returns 'True' if we should continue executing more actions, or
    -- exit (thereby closing the RPC call)
    clientAct ::
         Action (Metadata, RPC) NoMetadata
      -> StateT (ServerHealth ()) IO Bool
    clientAct = \case
        Initiate _ ->
          error "clientLocal: unexpected Initiate"
        Send x -> do
          isExpected <- adjustExpectation ()
          expect isExpected =<< liftIO (try $ Client.Binary.sendInput call x)
          return True
        Terminate (Just exceptionId) -> do
          throwM $ SomeClientException exceptionId
        Terminate Nothing ->
          return False
        SleepMilli n -> do
          liftIO $ threadDelay (n * 1_000)
          return True

    reactToServer ::
           Action Metadata Metadata
        -> StateT (ServerHealth ()) IO ()
    reactToServer = \case
        Initiate expectedMetadata -> liftIO $ do
          receivedMetadata <- atomically $ Client.recvResponseMetadata call
          expect (== expectedMetadata) $ Set.fromList receivedMetadata
        Send (FinalElem a b) -> do
          -- Known bug (limitation in http2). See recvMessageLoop.
          reactToServer $ Send (StreamElem a)
          reactToServer $ Send (NoMoreElems b)
        Send expectedElem -> do
          expected <- adjustExpectation expectedElem
          received <- liftIO . try $
                        fmap (first Set.fromList) $
                          Client.Binary.recvOutput call
          expect expected received
        Terminate mErr -> do
          reactToServerTermination mErr
        SleepMilli _ ->
          return ()

    -- See 'reactToClientTermination' for discussion.
    reactToServerTermination ::
         Maybe ExceptionId
      -> StateT (ServerHealth ()) IO ()
    reactToServerTermination mErr = do
        liftIO $ void . timeout 5_000_000 $ atomically $ do
          healthy <- Client.isCallHealthy call
          when healthy $ retry
        modify $ ifAlive $ \() ->
          case mErr of
            Just i  -> Failed $ SomeServerException i
            Nothing -> Disappeared

    -- Adjust expectation when communicating with the server
    --
    -- If the server handler died for some reason, we won't get the regular
    -- result, but should instead see the exception reported to the client.
    adjustExpectation :: forall a.
         Eq a
      => a
      -> StateT (ServerHealth ()) IO (Either GrpcException a -> Bool)
    adjustExpectation x =
        return . aux =<< get
     where
       aux :: ServerHealth () -> Either GrpcException a -> Bool
       aux serverHealth result =
           case serverHealth of
             Failed err ->
               case result of
                 Left (GrpcException GrpcUnknown msg [])
                   | msg == Just (Text.pack $ show err)
                   -> True
                 _otherwise -> False
             Disappeared ->
               -- TODO: Not really sure what exception we get here actually
               case result of
                 Left (GrpcException GrpcUnknown msg [])
                   | msg == Just "TODO"
                   -> True
                 _otherwise -> False
             Alive () ->
               case result of
                 Right x'   -> x == x'
                 _otherwise -> False

clientGlobal ::
     TestClock
  -> (forall a. (Client.Connection -> IO a) -> IO a)
  -> GlobalSteps
  -> IO ()
clientGlobal testClock withConn = \steps@(GlobalSteps globalSteps) ->
    -- TODO: The tests assume that different calls are independent from each
    -- other. This is mostly true, but not completely: when a client or a server
    -- terminates early, the entire connection (supporting potentially many
    -- calls) is reset. It's not entirely clear why; it feels like an
    -- unnecessary limitation in @http2@.
    --
    -- Ideally, we would either (1) randomly assign connections to calls and
    -- then test that an early termination only affects calls using the same
    -- connection, or better yet, (2), remove this limitation from @http2@.
    --
    -- For now, we do neither: /if/ a test includes early termination, we give
    -- each call its own connection, thereby regaining independence.
    if hasEarlyTermination steps
      then go Nothing [] globalSteps
      else withConn $ \conn -> go (Just conn) [] globalSteps
  where
    go :: Maybe Client.Connection -> [Async ()] -> [LocalSteps] -> IO ()
    go _ threads [] = do
         -- Wait for all threads to finish
         --
         -- This also ensures that if any of these threads threw an exception,
         -- that is now rethrown here in the main test.
        mapM_ wait threads
    go mConn threads (c:cs) =
        withAsync (runLocalSteps mConn c) $ \newThread ->
          go mConn (newThread:threads) cs

    runLocalSteps :: Maybe Client.Connection -> LocalSteps -> IO ()
    runLocalSteps mConn (LocalSteps steps) = do
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
              (case mConn of
                  Just conn -> ($ conn)
                  Nothing   -> withConn) $ \conn ->
                Client.withRPC conn params proxy $ \call -> do
                  -- We wait for the /server/ to advance the test clock (so that
                  -- we are use the next step doesn't happen until the connection
                  -- is established).
                  --
                  -- NOTE: We could instead wait for the server to send the
                  -- initial metadata; this too would provide evidence that the
                  -- conneciton has been established. However, doing so precludes
                  -- a class of correct behaviour: the server might not respond
                  -- with that initial metadata until the client has sent some
                  -- messages.
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
            continue <-   serverAct action
                        `finally`
                          liftIO (advanceTestClock testClock)
            if continue then
              go steps
            else do
              -- We need to exit the scope of the handler, but we do want to
              -- keep advancing the test clock when its our turn, so that we
              -- don't interfere with the timing of other threads.
              let ourStep :: (TestClockTick, LocalStep) -> Maybe TestClockTick
                  ourStep (tick' , ServerAction _) = Just tick'
                  ourStep (_     , ClientAction _) = Nothing
              liftIO $ do
                void $ forkIO $
                  advanceTestClockAtTimes testClock $
                    mapMaybe ourStep steps
          ClientAction action -> do
            reactToClient action
            go steps

    -- Server action
    --
    -- Returns 'True' if we should continue executing the other actions, or
    -- terminate (thereby terminating the handler)
    serverAct ::
         Action Metadata Metadata
      -> StateT (ClientHealth ()) IO Bool
    serverAct = \case
        Initiate metadata -> liftIO $ do
          Server.setResponseMetadata call (Set.toList metadata)
          void $ Server.initiateResponse call
          return True
        Send x -> do
          isExpected <- adjustExpectation ()
          received   <- try . liftIO $
                          Server.Binary.sendOutput call (first Set.toList x)
          expect isExpected received
          return True
        Terminate (Just exceptionId) -> do
          throwM $ SomeServerException exceptionId
        Terminate Nothing ->
          return False
        SleepMilli n -> do
          liftIO $ threadDelay (n * 1_000)
          return True

    reactToClient ::
           Action (Metadata, RPC) NoMetadata
        -> StateT (ClientHealth ()) IO ()
    reactToClient = \case
        Initiate _ ->
          error "serverLocal: unexpected ClientInitiateRequest"
        Send (FinalElem a b) -> do
          -- Known bug (limitation in http2). See recvMessageLoop.
          reactToClient $ Send (StreamElem a)
          reactToClient $ Send (NoMoreElems b)
        Send expectedElem -> do
          isExpected <- adjustExpectation expectedElem
          expect isExpected =<< liftIO (try $ Server.Binary.recvInput call)
        Terminate mErr -> do
          reactToClientTermination mErr
        SleepMilli _ ->
          return ()

    reactToClientTermination ::
         Maybe ExceptionId
      -> StateT (ClientHealth ()) IO ()
    reactToClientTermination mErr = do
        -- Wait for the client-side exception to become noticable server-side.
        -- Under normal circumstances the server will only notice this when
        -- trying to communicate; by explicitly checking we avoid
        -- non-determinism in the test (where the exception may or may not
        -- already have been come visible server-side).
        liftIO $ void . timeout 5_000_000 $ atomically $ do
          healthy <- Server.isCallHealthy call
          when healthy $ retry

        -- Update client health, /if/ the client was alive at this point
        modify $ ifAlive $ \() ->
          case mErr of
            Just i  -> Failed $ SomeClientException i
            Nothing -> Disappeared

    -- Adjust expectation when communicating with the client
    adjustExpectation :: forall a.
         Eq a
      => a
      -> StateT (ClientHealth ()) IO (Either Server.ClientDisconnected a -> Bool)
    adjustExpectation x =
        return . aux =<< get
     where
       aux :: ClientHealth () -> Either Server.ClientDisconnected a -> Bool
       aux clientHealth result =
           case clientHealth of
             Failed _err ->
               case result of
                 Left (Server.ClientDisconnected _) -> True
                 _otherwise                         -> False
             Disappeared ->
               case result of
                 Left (Server.ClientDisconnected _) -> True
                 _otherwise                         -> False
             Alive () ->
               case result of
                 Right x'   -> x == x'
                 _otherwise -> False

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
         expect (== metadata) $ Set.fromList receivedMetadata
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
    annotate steps err = throwM $ AnnotatedServerException {
          serverGlobalException          = err
        , serverGlobalExceptionSteps     = steps
        , serverGlobalExceptionCallStack = prettyCallStack
        }

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
