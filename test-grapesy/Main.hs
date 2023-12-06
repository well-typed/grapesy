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
import Control.Monad.Catch (ExitCase(..))
import Control.Tracer
import Data.Bifunctor
import Data.Default
import Data.Proxy
import Data.Text qualified as Text
import Data.Void
import Debug.Trace
import GHC.Stack
import GHC.TypeLits
import Network.HPACK qualified as HPACK
import Network.HTTP2.Client qualified as HTTP2.Client
import Network.Run.TCP qualified as Run
import Network.Socket

import Network.GRPC.Client.Session
import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Run qualified as Server
import Network.GRPC.Spec
import Network.GRPC.Util.Concurrency
import Network.GRPC.Util.Session qualified as Session

-- ========================================================================== --

_traceLabelled :: Show a => String -> a -> a
_traceLabelled lbl x = trace (lbl ++ ": " ++ show x) x

-- ========================================================================== --

clientWithRPC :: forall rpc.
     (IsRPC rpc, HasCallStack)
  => ClientConnection -> CallParams -> Proxy rpc -> (ClientCall rpc -> IO ()) -> IO ()
clientWithRPC ClientConnection{clientConnState} callParams _proxy k =
    mask $ \unmask -> do
      (_, conn) <-
        atomically $ do
          connState <- readTVar clientConnState
          case connState of
            ConnectionNotReady              -> retry
            ConnectionReady connClosed conn -> return (connClosed, conn)
            ConnectionAbandoned err         -> throwSTM err
            ConnectionClosed                -> error "impossible"

      channel <-
        Session.setupRequestChannel
          ClientSession
          nullTracer
          conn
          flowStart

      mb :: Either SomeException () <- unmask $ try $ k (ClientCall channel)
      _ <- Session.close channel (ExitCaseSuccess ()) -- simplification
      either throwIO return mb
  where
    flowStart :: Session.FlowStart (ClientOutbound rpc)
    flowStart = Session.FlowStartRegular $ OutboundHeaders {
        outHeaders = requestHeaders
      }

    requestHeaders :: RequestHeaders
    requestHeaders = RequestHeaders{
          requestMetadata = callRequestMetadata callParams
        }

clientSendInput ::
     HasCallStack
  => ClientCall rpc
  -> StreamElem NoMetadata (Input rpc)
  -> IO ()
clientSendInput call msg = do
    atomically $ Session.send (clientCallChannel call) msg
    StreamElem.whenDefinitelyFinal msg $ \_ ->
      void $ Session.waitForOutbound (clientCallChannel call)

clientRecvOutput ::
     ClientCall rpc
  -> IO (StreamElem [CustomMetadata] (Output rpc))
clientRecvOutput call = atomically $
    first collapseTrailers <$> Session.recv (clientCallChannel call)
  where
    -- No difference between 'ProperTrailers' and 'TrailersOnly'
    collapseTrailers ::
         Either [CustomMetadata] [CustomMetadata]
      -> [CustomMetadata]
    collapseTrailers = either id id

-- ========================================================================== --

data ClientConnection = ClientConnection {
      clientConnState :: TVar ConnectionState
    }

data ClientCall rpc = IsRPC rpc => ClientCall {
      clientCallChannel :: Session.Channel (ClientSession rpc)
    }

clientWithConnection ::
     HasCallStack
  => Authority -- ^ What to connect to
  -> (ClientConnection -> IO a)
  -> IO a
clientWithConnection server k = do
    clientConnState <- newTVarIO ConnectionNotReady

    connCanClose <- newEmptyMVar
    let stayConnectedThread :: IO ()
        stayConnectedThread =
          clientStayConnected server clientConnState connCanClose

    void $ forkIO $ stayConnectedThread
    k ClientConnection{clientConnState}
      `finally` putMVar connCanClose ()

data ConnectionState =
    ConnectionNotReady
  | ConnectionReady (TMVar (Maybe SomeException)) Session.ConnectionToServer
  | ConnectionAbandoned SomeException
  | ConnectionClosed

clientStayConnected ::
     Authority
  -> TVar ConnectionState
  -> MVar ()
  -> IO ()
clientStayConnected auth connVar connCanClose =
    loop
  where
    loop :: IO ()
    loop = do
        connClosed <- newEmptyTMVarIO

        mRes <- try $
          runTCPClient $ \sock ->
            bracket (HTTP2.Client.allocSimpleConfig sock writeBufferSize)
                    HTTP2.Client.freeSimpleConfig $ \conf ->
              HTTP2.Client.run
                    (clientConfig Http)
                    conf
                  $ \sendRequest _aux -> do
                let conn = Session.ConnectionToServer sendRequest
                atomically $ writeTVar connVar $ ConnectionReady connClosed conn
                takeMVar connCanClose

        atomically $ putTMVar connClosed $ either Just (\() -> Nothing) mRes

        atomically $ do
          case mRes of
            Right () -> writeTVar connVar $ ConnectionClosed
            Left err -> writeTVar connVar $ ConnectionAbandoned err

    runTCPClient :: (Socket -> IO a) -> IO a
    runTCPClient =
        Run.runTCPClient (authorityHost auth) (show $ authorityPort auth)

    writeBufferSize :: HPACK.BufferSize
    writeBufferSize = 4096

    clientConfig :: Scheme -> HTTP2.Client.ClientConfig
    clientConfig scheme = HTTP2.Client.defaultClientConfig {
          HTTP2.Client.scheme    = rawScheme serverPseudoHeaders
        , HTTP2.Client.authority = rawAuthority serverPseudoHeaders
        }
      where
        serverPseudoHeaders :: RawServerHeaders
        serverPseudoHeaders = buildServerHeaders $ ServerHeaders scheme auth

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

-- RPC calls that exchange no data
data TrivialRpc (endpoint :: Symbol)

instance KnownSymbol endpoint => IsRPC (TrivialRpc endpoint) where
  type Input  (TrivialRpc endpoint) = Void
  type Output (TrivialRpc endpoint) = Void

  serializationFormat _ = "trivial"
  serviceName         _ = "trivial"
  methodName          _ = Text.pack $ symbolVal (Proxy @endpoint)
  messageType         _ = "Void"
  serializeInput      _ = absurd
  serializeOutput     _ = absurd
  deserializeInput    _ = const $ Left "Unexpected value of type Void"
  deserializeOutput   _ = const $ Left "Unexpected value of type Void"

type TestRpc1 = TrivialRpc "test1"
type TestRpc2 = TrivialRpc "test2"

{-------------------------------------------------------------------------------
  Client-side interpretation

  After connecting, we wait for the /server/ to advance the test clock (so that
  we are use the next step doesn't happen until the connection is established).
-------------------------------------------------------------------------------}

clientLocal1 :: HasCallStack => TestClock -> Authority -> IO ()
clientLocal1 testClock auth = handle showExceptions $ do
    waitForTestClockTick testClock 0
    clientWithConnection auth $ \conn -> do
      clientWithRPC conn def (Proxy @TestRpc1) $ \_call -> do
        waitForTestClockTick testClock 2
        throwIO $ userError "this models some kind of client exception"
  where
    showExceptions :: SomeException -> IO ()
    showExceptions err = do
        putStrLn $ "clientLocal1: " ++ show err
        throwIO err

clientLocal2 :: HasCallStack => TestClock -> Authority -> IO ()
clientLocal2 testClock auth = handle showExceptions $ do
    waitForTestClockTick testClock 1
    clientWithConnection auth $ \conn -> do
      clientWithRPC conn def (Proxy @TestRpc2) $ \call -> do
        waitForTestClockTick testClock 3
        clientSendInput call $ NoMoreElems NoMetadata
        _ <- clientRecvOutput call
        advanceTestClock testClock
  where
    showExceptions :: SomeException -> IO ()
    showExceptions err = do
        putStrLn $ "clientLocal2: " ++ show err
        throwIO err

clientGlobal :: TestClock -> Authority -> IO ()
clientGlobal testClock auth =
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
    withAsync (clientLocal2 testClock auth) $ \thread2 ->
    withAsync (clientLocal1 testClock auth) $ \thread1 ->
    mapM_ wait [thread1, thread2]

{-------------------------------------------------------------------------------
  Server-side interpretation

  The server-side is slightly different, since the infrastructure spawns
  threads on our behalf (one for each incoming RPC).
-------------------------------------------------------------------------------}

serverLocal1 ::
     TestClock
  -> Server.Call (TrivialRpc meth)
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
  -> Server.Call (TrivialRpc meth)
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
      let auth :: Authority
          auth = Authority "localhost" 50051

      clientGlobal testClock auth

    -- Wait for the test clock to reach 4 (which it won't)
    -- (This just prevents the test from terminating too soon)
    waitForTestClockTick testClock 4
