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

import Control.Exception
import Control.Monad
import Control.Tracer
import Data.Default
import Data.Proxy
import Debug.Trace
import GHC.Stack

import Network.GRPC.Client.Call qualified as Client
import Network.GRPC.Client.Connection qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Run qualified as Server
import Network.GRPC.Spec
import Network.GRPC.Util.Concurrency

-- ========================================================================== --

_traceLabelled :: Show a => String -> a -> a
_traceLabelled lbl x = trace (lbl ++ ": " ++ show x) x

-- ========================================================================== --


-- ========================================================================== --

{-------------------------------------------------------------------------------
  Test clock

  These are concurrent tests, looking at the behaviour of @n@ concurrent
  connections between a client and a server. To make the interleaving of actions
  easier to see, we introduce a global "test clock". Every test action is
  annotated with a particular "test tick" on this clock. The clock is advanced
  after each step: this is a logical clock, unrelated to the passing of time.
-------------------------------------------------------------------------------}

type TestClock     = TVar TestClockTick
type TestClockTick = Int

newTestClock :: IO TestClock
newTestClock = newTVarIO 0

waitForTestClockTick :: HasCallStack => TestClock -> TestClockTick -> IO ()
waitForTestClockTick clock tick = atomically $ do
    currentTick <- readTVar clock
    unless (currentTick == tick) retry

advanceTestClock :: TestClock -> IO ()
advanceTestClock clock = atomically $ modifyTVar clock succ

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
clientLocal1 testClock withConn = handle showExceptions $ do
    waitForTestClockTick testClock 0
    withConn $ \conn -> do
      Client.withRPC conn def (Proxy @TestRpc1) $ \_call -> do
        waitForTestClockTick testClock 2
        throwIO $ userError "this models some kind of client exception"
  where
    showExceptions :: SomeException -> IO ()
    showExceptions err = do
        putStrLn $ "clientLocal1: " ++ show err
        throwIO err

clientLocal2 ::
     HasCallStack
  => TestClock
  -> (forall a. (Client.Connection -> IO a) -> IO a)
  -> IO ()
clientLocal2 testClock withConn = handle showExceptions $ do
    waitForTestClockTick testClock 1
    withConn $ \conn -> do
      Client.withRPC conn def (Proxy @TestRpc2) $ \call -> do
        waitForTestClockTick testClock 3
        Client.sendInput call $ NoMoreElems NoMetadata
        _ <- Client.recvOutput call
        advanceTestClock testClock
  where
    showExceptions :: SomeException -> IO ()
    showExceptions err = do
        putStrLn $ "clientLocal2: " ++ show err
        throwIO err

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
    --
    -- Explanation: thread1 throws an exception; this will cause the wait on
    -- thread1 to throw an exception also, so that we will exit the scope of
    -- both calls to @withAsync@, thereby cancelling thread2. The client
    -- disappearing causes the recv in serverLocal2 to become impossible
    -- (though we get get the blocked indefinitely exception in STM is still
    -- unclear, of course).
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
serverLocal1 testClock call = handle showExceptions $ do
    advanceTestClock testClock

    -- Wait for client early termination to become visible
    atomically $ do
      healthy <- Server.isCallHealthy call
      when healthy $ retry

    -- At this point, sending anything should fail
    waitForTestClockTick testClock 4
    Server.sendOutput call $ NoMoreElems []
  where
    showExceptions :: SomeException -> IO ()
    showExceptions err = do
        putStrLn $ "serverLocal1: " ++ show err
        throwIO err

serverLocal2 ::
     TestClock
  -> Server.Call (BinaryRpc serv meth)
  -> IO ()
serverLocal2 testClock call = handle showExceptions $ do
    advanceTestClock testClock

    -- Wait for client to tell us they will send no more elements
    _ <- Server.recvInput call
    advanceTestClock testClock

    -- Tell the client we won't send them more elements either
    waitForTestClockTick testClock 5
    Server.sendOutput call $ NoMoreElems []
  where
    showExceptions :: SomeException -> IO ()
    showExceptions err = do
        putStrLn $ "serverLocal2: " ++ show err
        throwIO err

-- ========================================================================== --

main :: IO ()
main = do
    testClock <- newTestClock

    _server <- async $ do
      let serverHandlers :: [Server.RpcHandler IO]
          serverHandlers = [
                Server.mkRpcHandler (Proxy @TestRpc1) $ serverLocal1 testClock
              , Server.mkRpcHandler (Proxy @TestRpc2) $ serverLocal2 testClock
              ]

          serverConfig :: Server.ServerConfig
          serverConfig = Server.ServerConfig {
                serverSetup    = def
              , serverInsecure = Just Server.InsecureConfig {
                    insecureHost = Nothing
                  , insecurePort = "50051"
                  }
              , serverSecure   = Nothing
              }

          serverParams :: Server.ServerParams
          serverParams = def {
                -- Don't print exceptions (to avoid confusing test output)
                Server.serverExceptionTracer = nullTracer
              }

      Server.withServer serverParams serverHandlers $
        Server.runServer serverConfig

    -- Give the server a chance to start
    -- (We can automatically reconnect, but this keeps the test simpler)
    threadDelay 500_000

    -- Start client
    --
    -- We run this in its own thread, so we can catch its exceptions separately
    -- from the one from the server (see below)
    _client <- async $ do
      let clientParams :: Client.ConnParams
          clientParams = Client.ConnParams {
                connDebugTracer     = nullTracer
              , connCompression     = Compr.none
              , connDefaultTimeout  = Nothing
              , connReconnectPolicy = def
              }

          clientServer :: Client.Server
          clientServer = Client.ServerInsecure clientAuthority

          clientAuthority :: Authority
          clientAuthority = Authority "localhost" 50051

      clientGlobal testClock $
        Client.withConnection clientParams clientServer

    -- Wait for the test clock to reach 4 (which it won't)
    -- (This just prevents the test from terminating too soon)
    waitForTestClockTick testClock 4
