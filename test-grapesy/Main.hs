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

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Tracer
import Data.Bifunctor
import Data.Default
import Data.Proxy
import Data.Set (Set)
import GHC.Generics qualified as GHC
import Network.HTTP2.Internal qualified as HTTP2
import Network.HTTP2.Server qualified as HTTP2
import System.Exit
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
        throwIO serverErr
      (Right (), Left clientErr) ->
        throwIO clientErr
      (Left serverErr, Left clientErr) ->
          -- We are hunting for BlockedIndefinitelyOnSTM
          case (fromException serverErr, fromException clientErr) of
            (Just BlockedIndefinitelyOnSTM{}, _) ->
              throwIO serverErr
            (_, Just BlockedIndefinitelyOnSTM{}) ->
              throwIO clientErr
            _otherwise ->
              -- doesn't really matter which one we throw
              throwIO clientErr

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

{-------------------------------------------------------------------------------
  Client-side interpretation

  After connecting, we wait for the /server/ to advance the test clock (so that
  we are use the next step doesn't happen until the connection is established).
-------------------------------------------------------------------------------}

clientLocal1 ::
     HasCallStack
  => TestClock
  -> (forall a. (Client.Connection -> IO a) -> IO a)
  -> IO ()
clientLocal1 testClock withConn = do
    waitForTestClockTick testClock $ TestClockTick 0
    withConn $ \conn ->
      Client.withRPC conn def (Proxy @TestRpc1) $ \_call -> do
        waitForTestClockTick testClock $ TestClockTick 2
        throwIO $ SomeClientException (ExceptionId 0)

clientLocal2 ::
     HasCallStack
  => TestClock
  -> (forall a. (Client.Connection -> IO a) -> IO a)
  -> IO ()
clientLocal2 testClock withConn = do
    waitForTestClockTick testClock $ TestClockTick 1
    withConn $ \conn ->
      Client.withRPC conn def (Proxy @TestRpc2) $ \call -> do
        waitForTestClockTick testClock $ TestClockTick 3
        Client.Binary.sendInput call (NoMoreElems NoMetadata :: (StreamElem NoMetadata Int))
        _ :: StreamElem [CustomMetadata] Int <- Client.Binary.recvOutput call
        advanceTestClock testClock

clientGlobal ::
     TestClock
  -> (forall a. (Client.Connection -> IO a) -> IO a)
  -> IO ()
clientGlobal testClock withConn =
    -- The bug is /very/ sensitive to what exactly we do here. The bug does not
    -- materialize if we
    --
    -- * @wait@ in the opposite order
    -- * use @async@ instead of @withAsync@
    withAsync (clientLocal2 testClock withConn) $ \thread2 ->
    withAsync (clientLocal1 testClock withConn) $ \thread1 ->
    mapM_ wait [thread1, thread2]

{-------------------------------------------------------------------------------
  Server-side interpretation

  The server-side is slightly different, since the infrastructure spawns
  threads on our behalf (one for each incoming RPC).
-------------------------------------------------------------------------------}

serverLocal1 ::
     TestClock
  -> Server.Call (BinaryRpc serv meth)
  -> IO ()
serverLocal1 testClock call = do
    advanceTestClock testClock

    -- Wait for client early termination to become visible
    atomically $ do
      healthy <- Server.isCallHealthy call
      when healthy $ retry

    -- At this point, sending anything should fail
    waitForTestClockTick testClock $ TestClockTick 4
    Server.Binary.sendOutput call (NoMoreElems [] :: (StreamElem [CustomMetadata] Int))

serverLocal2 ::
     TestClock
  -> Server.Call (BinaryRpc serv meth)
  -> IO ()
serverLocal2 testClock call = do
    advanceTestClock testClock

    -- Wait for client to tell us they will send no more elements
    _ :: StreamElem NoMetadata Int <- Server.Binary.recvInput call
    advanceTestClock testClock

    -- Tell the client we won't send them more elements either
    waitForTestClockTick testClock $ TestClockTick 5
    Server.Binary.sendOutput call (NoMoreElems [] :: (StreamElem [CustomMetadata] Int))

-- ========================================================================== --

main :: IO ()
main = do
    testClock <- newTestClock

    let client :: (forall a. (Client.Connection -> IO a) -> IO a) -> IO ()
        client conn = clientGlobal testClock conn

        server :: [Server.RpcHandler IO]
        server = [
              Server.mkRpcHandler (Proxy @TestRpc1) $ serverLocal1 testClock
            , Server.mkRpcHandler (Proxy @TestRpc2) $ serverLocal2 testClock
            ]

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

