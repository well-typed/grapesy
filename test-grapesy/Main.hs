{-
   inlined version of this tset case:
   (reverse ordering for the client is important for the bug to materialize)

   > globalSteps :: GlobalSteps
   > globalSteps = [
   >     [ ( TestClockTick 1, ClientAction $ Initiate ( Set.fromList [] , RPC1 ))
   >     , ( TestClockTick 3, ClientAction $ Send (NoMoreElems NoMetadata))
   >     , ( TestClockTick 5, ServerAction $ Send (NoMoreElems (Set.fromList [])))
   >     ]
   >   , [ ( TestClockTick 0, ClientAction $ Initiate ( Set.fromList [] , RPC1 ))
   >     , ( TestClockTick 2, ClientAction $ Terminate (Just (ExceptionId 0)))
   >     , ( TestClockTick 4, ServerAction $ Send (NoMoreElems (Set.fromList [])))
   >     ]
   >   ]
-}

-- with O1 the bug still triggers, but it's (much?) less likely
{-# OPTIONS_GHC -O0 #-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (BlockedIndefinitelyOnSTM(..))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Control.Tracer
import Data.Bifunctor
import Data.Default
import Data.Maybe (mapMaybe)
import Data.Proxy
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Network.HTTP2.Internal qualified as HTTP2
import Network.HTTP2.Server qualified as HTTP2
import System.Exit
import System.Timeout (timeout)
import Text.Show.Pretty

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary
import Network.GRPC.Server.Run qualified as Server

import Test.Driver.Dialogue.TestClock
import Test.Util.PrettyVal

import Debug.Trace

-- ========================================================================== --

_traceLabelled :: Show a => String -> a -> a
_traceLabelled label x = trace (label ++ ": " ++ show x) x

-- ========================================================================== --

{-------------------------------------------------------------------------------
  Server handler lock
-------------------------------------------------------------------------------}

newtype ServerHandlerLock = ServerHandlerLock (TVar (Either SomeException Int))

newServerHandlerLock :: IO ServerHandlerLock
newServerHandlerLock = ServerHandlerLock <$> newTVarIO (Right 0)

waitForHandlerTermination :: ServerHandlerLock -> STM ()
waitForHandlerTermination (ServerHandlerLock lock) = do
    mActiveHandlers <- readTVar lock
    case mActiveHandlers of
      Left err -> throwSTM err
      Right 0  -> return ()
      Right _  -> retry

serverHandlerLockHook :: ServerHandlerLock -> HTTP2.Server -> HTTP2.Server
serverHandlerLockHook (ServerHandlerLock lock) server request aux respond =
    mask $ \unmask -> do
      register
      result <- try $ unmask $ server request aux respond
      -- We don't rethrow the exception (instead 'waitForHandlerTermination' will)
      unregister result
  where
    register :: IO ()
    register = atomically $ modifyTVar lock $ bimap id succ

    unregister :: Either SomeException () -> IO ()
    unregister result = atomically $ modifyTVar lock $ either Left $
        case result of
          Right () ->
            Right . pred
          Left e ->
            case fromException e of
              Just (HTTP2.KilledByHttp2ThreadManager _) ->
                -- If we are shutting down because of a test failure, we don't
                -- want to get confused by any other handlers being shut down
                Right . pred
              Nothing ->
                const (Left e)

{-------------------------------------------------------------------------------
  Server
-------------------------------------------------------------------------------}

runTestServer ::
     Tracer IO SomeException
  -> ServerHandlerLock
  -> [Server.RpcHandler IO]
  -> IO ()
runTestServer serverExceptions handlerLock serverHandlers = do
    let serverSetup :: Server.ServerSetup
        serverSetup = def {
              Server.serverHandlerHook = serverHandlerLockHook handlerLock
            }

        serverConfig :: Server.ServerConfig
        serverConfig = Server.ServerConfig {
              serverSetup    = serverSetup
            , serverInsecure = Just Server.InsecureConfig {
                  insecureHost = Nothing
                , insecurePort = "50051"
                }
            , serverSecure   = Nothing
            }

        serverParams :: Server.ServerParams
        serverParams = Server.ServerParams {
              serverCompression     = Compr.none
            , serverExceptionTracer = serverExceptions
            , serverDebugTracer     = nullTracer
            }

    Server.withServer serverParams serverHandlers $
      Server.runServer serverConfig

{-------------------------------------------------------------------------------
  Client
-------------------------------------------------------------------------------}

runTestClient :: forall a.
     ((forall b. (Client.Connection -> IO b) -> IO b) -> IO a)
  -> IO a
runTestClient clientRun = do
    let clientParams :: Client.ConnParams
        clientParams = Client.ConnParams {
              connDebugTracer     = nullTracer
            , connCompression     = Compr.none
            , connDefaultTimeout  = Nothing

              -- We need a single reconnect, to enable wait-for-ready.
              -- This avoids a race condition between the server starting first
              -- and the client starting first.
            , connReconnectPolicy =
                  Client.ReconnectAfter (0.1, 0.2)
                $ Client.DontReconnect
            }

        clientServer :: Client.Server
        clientServer = Client.ServerInsecure clientAuthority

        clientAuthority :: Client.Authority
        clientAuthority = Client.Authority "localhost" 50051

    clientRun $ Client.withConnection clientParams clientServer

{-------------------------------------------------------------------------------
  Main entry point: run server and client together
-------------------------------------------------------------------------------}

runTestClientServer :: forall a.
     ((forall b. (Client.Connection -> IO b) -> IO b) -> IO a)
  -> [Server.RpcHandler IO]
  -> IO a
runTestClientServer clientRun serverHandlers = do
    -- Normally, when a handler throws an exception, that request is simply
    -- aborted, but the server does not shut down. However, for the sake of
    -- testing, if a handler throws an unexpected exception, the test should
    -- fail. We therefore monitor for these exceptions.
    serverHandlerExceptions <- newEmptyTMVarIO
    let serverExceptions :: Tracer IO SomeException
        serverExceptions = arrow $ emit $ \err ->
            atomically $ void $ tryPutTMVar serverHandlerExceptions err

    -- Start server
    serverHandlerLock <- newServerHandlerLock
    server <- async $ do
      runTestServer
        serverExceptions
        serverHandlerLock
        serverHandlers

    -- Start client
    --
    -- We run this in its own thread, so we can catch its exceptions separately
    -- from the one from the server (see below)
    client <- async $ runTestClient clientRun

    -- The server never shuts down under normal circumstances; so we wait for
    -- the client to terminate, then wait for any potential still-running
    -- server handlers to terminate, monitoring for exceptions, and then shut
    -- down the server.
    clientRes <- waitCatch client
    serverRes <- atomically $
                     (Left <$> readTMVar serverHandlerExceptions)
                   `orElse`
                     (Right <$> waitForHandlerTermination serverHandlerLock)
    cancel server

    case (serverRes, clientRes) of
      (Right (), Right a) ->
        return a
      (Left serverErr, Right _) ->
        throwM serverErr
      (Right (), Left clientErr) ->
        throwM clientErr
      (Left serverErr, Left clientErr) ->
          -- We are hunting for BlockedIndefinitelyOnSTM
          case (fromException serverErr, fromException clientErr) of
            (Just BlockedIndefinitelyOnSTM{}, _) ->
              throwM serverErr
            (_, Just BlockedIndefinitelyOnSTM{}) ->
              throwM clientErr
            _otherwise ->
              -- doesn't really matter which one we throw
              throwM clientErr

-- ========================================================================== --

data LocalStep =
    ClientAction (Action (Metadata, RPC) NoMetadata)
  | ServerAction (Action Metadata        Metadata)
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (PrettyVal)

data Action a b =
    -- | Initiate request and response
    --
    -- When the client initiates a request, they can specify a timeout, initial
    -- metadata for the request, as well as which endpoint to connect to. This
    -- must happen before anything else.
    --
    -- On the server side an explicit 'Initiate' is not required; if not
    -- present, there will be an implicit one, with empty metadata, on the first
    -- 'Send'.
    Initiate a

    -- | Send a message to the peer
  | Send (StreamElem b Int)

    -- | Early termination (cleanly or with an exception)
  | Terminate (Maybe ExceptionId)

    -- | Sleep specified number of milliseconds
    --
    -- This is occassionally useful, for example to have the client keep the
    -- connection open to the server for a bit longer, without actually doing
    -- anything with that connection.
  | SleepMilli Int
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (PrettyVal)

data RPC = RPC1 | RPC2 | RPC3
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (PrettyVal)

-- | Metadata
--
-- We use 'Set' for 'CustomMetadata' rather than a list, because we do not
-- want to test that the /order/ of the metadata is matched.
type Metadata = Set CustomMetadata

{-------------------------------------------------------------------------------
  Many channels (bird's-eye view)
-------------------------------------------------------------------------------}

type LocalSteps  = [(TestClockTick, LocalStep)]
type GlobalSteps = [LocalSteps]

{-------------------------------------------------------------------------------
  User exceptions

  When a test calls for the client or the server to throw an exception, we throw
  one of these. Their sole purpose is to be "any" kind of exception (not a
  specific one).
-------------------------------------------------------------------------------}

data SomeServerException = SomeServerException ExceptionId
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Exception, PrettyVal)

data SomeClientException = SomeClientException ExceptionId
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Exception, PrettyVal)

-- | We distinguish exceptions from each other simply by a number
newtype ExceptionId = ExceptionId Int
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (PrettyVal)

-- ========================================================================== --

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
clientLocal testClock call = \steps ->
    flip evalStateT (Alive ()) $ go steps
  where
    go :: [(TestClockTick, LocalStep)] -> StateT (ServerHealth ()) IO ()
    go []                     = return ()
    go ((tick, step) : steps) = do
        case step of
          ClientAction action -> do
            liftIO $ waitForTestClockTick testClock tick
            continue <-
              clientAct action
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
            reactToServer action `finally` liftIO (advanceTestClock testClock)
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
  -> IO ()
clientGlobal testClock withConn =
    go Nothing [] globalSteps
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
    runLocalSteps mConn steps = do
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
                  clientLocal testClock call steps'

          _otherwise ->
            error $ "clientGlobal: expected Initiate, got " ++ show steps

    -- reverse ordering here is important for the bug to materialize
    globalSteps :: GlobalSteps
    globalSteps = [
        [ ( TestClockTick 1, ClientAction $ Initiate ( Set.fromList [] , RPC1 ))
        , ( TestClockTick 3, ClientAction $ Send (NoMoreElems NoMetadata))
        , ( TestClockTick 5, ServerAction $ Send (NoMoreElems (Set.fromList [])))
        ]
      , [ ( TestClockTick 0, ClientAction $ Initiate ( Set.fromList [] , RPC1 ))
        , ( TestClockTick 2, ClientAction $ Terminate (Just (ExceptionId 0)))
        , ( TestClockTick 4, ServerAction $ Send (NoMoreElems (Set.fromList [])))
        ]
      ]

{-------------------------------------------------------------------------------
  Server-side interpretation

  The server-side is slightly different, since the infrastructure spawns
  threads on our behalf (one for each incoming RPC).
-------------------------------------------------------------------------------}

serverLocal0 ::
     TestClock
  -> Server.Call (BinaryRpc serv meth)
  -> IO ()
serverLocal0 testClock call = do
    -- Wait for client to close their end
    expect expectedRecvResult =<< try (Server.Binary.recvInput call)
    void $ advanceTestClock testClock

    -- Close our end
    waitForTestClockTick testClock $ TestClockTick 5
    received <- try $ Server.Binary.sendOutput call (NoMoreElems [] :: (StreamElem [CustomMetadata] Int))
    expect expectedSendResult received
  where
    expectedRecvResult :: Either Server.ClientDisconnected (StreamElem NoMetadata Int) -> Bool
    expectedRecvResult result =
        case result of
          Right x'   -> x' == NoMoreElems NoMetadata
          _otherwise -> False

    expectedSendResult :: Either Server.ClientDisconnected () -> Bool
    expectedSendResult result =
        case result of
          Right x'   -> x' == ()
          _otherwise -> False

serverLocal1 ::
     TestClock
  -> Server.Call (BinaryRpc serv meth)
  -> IO ()
serverLocal1 testClock call = do
    -- Wait for client termination to become visible
    atomically $ do
      healthy <- Server.isCallHealthy call
      when healthy $ retry

    -- At this point, sending anything should fail
    waitForTestClockTick testClock $ TestClockTick 4
    let isExpected result = case result of
                              Left (Server.ClientDisconnected _) -> True
                              _otherwise                         -> False
    received  <- try $ Server.Binary.sendOutput call (NoMoreElems [] :: (StreamElem [CustomMetadata] Int))
    expect isExpected received

serverGlobal ::
     HasCallStack
  => TestClock
  -> Server.Call (BinaryRpc serv meth)
  -> IO ()
serverGlobal testClock call = do
    -- See discussion in clientGlobal (runLocalSteps)
    now <- advanceTestClock testClock

    case now of
      0 -> serverLocal0 testClock call
      1 -> serverLocal1 testClock call
      _ -> undefined

-- ========================================================================== --

main :: IO ()
main = do
    testClock      <- newTestClock

    let client :: (forall a. (Client.Connection -> IO a) -> IO a) -> IO ()
        client conn = clientGlobal testClock conn

        server :: [Server.RpcHandler IO]
        server = [ Server.mkRpcHandler (Proxy @TestRpc1) $ \call ->
                        serverGlobal testClock call ]

    mRes :: Either SomeException () <- try $ runTestClientServer client server
    case mRes of
      Left err | Just err'@BlockedIndefinitelyOnSTM{} <- fromException err -> do
        print err'
        exitWith $ ExitFailure 1
      Left err -> do
        putStrLn "Got expected exception"
        print err
        exitWith $ ExitSuccess
      _otherwise -> do
        putStrLn "No expection was thrown"
        exitWith $ ExitSuccess

