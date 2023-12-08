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
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.C8
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Void
import Debug.Trace
import GHC.Stack
import GHC.TypeLits
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Client qualified as HTTP2.Client
import Network.HTTP2.Internal qualified as HTTP2
import Network.HTTP2.Server qualified as HTTP2
import Network.Run.TCP (runTCPServer)
import Network.Run.TCP qualified as Run
import Network.Socket

import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Spec
import Network.GRPC.Util.Concurrency
import Network.GRPC.Util.HTTP2.Stream (ClientDisconnected(..))
import Network.GRPC.Util.Parser
import Network.GRPC.Util.Session qualified as Session

-- ========================================================================== --

_traceLabelled :: Show a => String -> a -> a
_traceLabelled lbl x = trace (lbl ++ ": " ++ show x) x

-- ========================================================================== --

clientWithRPC ::
     (HasCallStack, KnownSymbol meth)
  => ClientConnection
  -> Proxy meth
  -> (ClientCall meth -> IO ())
  -> IO ()
clientWithRPC clientConnState _proxy k =
    mask $ \unmask -> do
      (_, conn) <-
        atomically $ do
          connState <- readTVar clientConnState
          case connState of
            ConnectionNotReady              -> retry
            ConnectionReady connClosed conn -> return (connClosed, conn)
            ConnectionAbandoned err         -> throwSTM err
            ConnectionClosed                -> error "impossible"

      channel <- Session.setupRequestChannel TrivialClient nullTracer conn flowStart

      mb :: Either SomeException () <- unmask $ try $ k channel
      _ <- Session.close channel (ExitCaseSuccess ()) -- simplification
      either throwIO return mb
  where
    flowStart :: Session.FlowStart (ClientOutbound rpc)
    flowStart = Session.FlowStartRegular ClientOutboundHeaders

clientSendInput :: HasCallStack => ClientCall meth -> StreamElem () Void -> IO ()
clientSendInput channel msg = do
    atomically $ Session.send channel msg
    StreamElem.whenDefinitelyFinal msg $ \_ ->
      void $ Session.waitForOutbound channel

clientRecvOutput :: ClientCall meth -> IO (StreamElem () Void)
clientRecvOutput channel = atomically $
    first (const ()) <$> Session.recv channel

-- ========================================================================== --

type ClientConnection = TVar ConnectionState
type ClientCall meth  = Session.Channel (TrivialClient meth)

clientWithConnection ::
     HasCallStack
  => Authority -- ^ What to connect to
  -> (ClientConnection -> IO a)
  -> IO a
clientWithConnection auth k = do
    clientConnState <- newTVarIO ConnectionNotReady

    connCanClose <- newEmptyMVar
    let stayConnectedThread :: IO ()
        stayConnectedThread =
          clientStayConnected auth clientConnState connCanClose

    void $ forkIO $ stayConnectedThread
    k clientConnState `finally` putMVar connCanClose ()

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
            bracket (HTTP2.Client.allocSimpleConfig sock 4096)
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

    clientConfig :: Scheme -> HTTP2.Client.ClientConfig
    clientConfig scheme = HTTP2.Client.defaultClientConfig {
          HTTP2.Client.scheme    = rawScheme serverPseudoHeaders
        , HTTP2.Client.authority = rawAuthority serverPseudoHeaders
        }
      where
        serverPseudoHeaders :: RawServerHeaders
        serverPseudoHeaders = buildServerHeaders $ ServerHeaders scheme auth

-- ========================================================================== --

data TrivialClient  (meth :: Symbol) = TrivialClient
data ClientInbound  (meth :: Symbol)
data ClientOutbound (meth :: Symbol)

instance Session.DataFlow (ClientInbound meth) where
  data Headers    (ClientInbound meth) = ClientInboundHeaders deriving (Show)
  type Message    (ClientInbound meth) = Void
  type Trailers   (ClientInbound meth) = ()
  type NoMessages (ClientInbound meth) = Void

instance Session.DataFlow (ClientOutbound meth) where
  data Headers    (ClientOutbound meth) = ClientOutboundHeaders deriving (Show)
  type Message    (ClientOutbound meth) = Void
  type Trailers   (ClientOutbound meth) = ()
  type NoMessages (ClientOutbound meth) = Void

instance Session.IsSession (TrivialClient meth) where
  type Inbound  (TrivialClient meth) = ClientInbound  meth
  type Outbound (TrivialClient meth) = ClientOutbound meth

  buildOutboundTrailers _ _ = []
  parseInboundTrailers  _ _ = return ()
  buildMsg              _ _ = absurd
  parseMsg              _ _ = absurdParser

instance KnownSymbol meth => Session.InitiateSession (TrivialClient meth) where
  parseResponseRegular    _ _ = return ClientInboundHeaders
  parseResponseNoMessages _ _ = throwIO $ userError "Unexpected Trailers-Only"

  buildRequestInfo _ _ = Session.RequestInfo {
        requestMethod  = "POST"
      , requestPath    = "/trivial/" <> BS.C8.pack (symbolVal (Proxy @meth))
      , requestHeaders = []
      }

-- ========================================================================== --

data TrivialServer = TrivialServer
data ServerInbound
data ServerOutbound

instance Session.DataFlow ServerInbound where
  data Headers    ServerInbound = ServerInboundHeaders deriving (Show)
  type Message    ServerInbound = Void
  type Trailers   ServerInbound = ()
  type NoMessages ServerInbound = Void

instance Session.DataFlow ServerOutbound where
  data Headers    ServerOutbound = ServerOutboundHeaders deriving (Show)
  type Message    ServerOutbound = Void
  type Trailers   ServerOutbound = ()
  type NoMessages ServerOutbound = Void

instance Session.IsSession TrivialServer where
  type Inbound  TrivialServer = ServerInbound
  type Outbound TrivialServer = ServerOutbound

  buildOutboundTrailers _ _ = []
  parseInboundTrailers  _ _ = return ()
  buildMsg              _ _ = absurd
  parseMsg              _ _ = absurdParser

instance Session.AcceptSession TrivialServer where
  parseRequestRegular    _ _ = return ServerInboundHeaders
  parseRequestNoMessages _ _ = throwIO $ userError "Unexpected Trailers-Only"
  buildResponseInfo      _ _ = Session.ResponseInfo HTTP.ok200 []

-- ========================================================================== --

runServer :: ServiceName -> HTTP2.Server -> IO ()
runServer port server =
    runTCPServer Nothing port $ \sock -> do
      bracket (HTTP2.allocSimpleConfig sock 4096)
              HTTP2.freeSimpleConfig $ \config ->
        HTTP2.run HTTP2.defaultServerConfig config server

-- ========================================================================== --

type RpcHandler = ServerCall -> IO ()
type HandlerMap = [(Strict.ByteString, RpcHandler)]

mkServer :: HandlerMap -> HTTP2.Server
mkServer handlerMap = withServerConnection $ handleRequest handlerMap

handleRequest :: HasCallStack => HandlerMap -> ServerConnection -> IO ()
handleRequest handlers conn@(path, _) =
    serverAcceptCall conn handler
  where
    handler :: RpcHandler
    handler = case lookup path handlers of
                Just h  -> h
                Nothing -> error "withHandler: unknown path"

-- ========================================================================== --

type ServerCall = Session.Channel TrivialServer

serverAcceptCall :: ServerConnection -> (ServerCall -> IO ()) -> IO ()
serverAcceptCall (_path, conn) k = do
    let setupResponseChannel :: IO (Session.Channel TrivialServer)
        setupResponseChannel =
            Session.setupResponseChannel
              callSession
              nullTracer
              conn
              (\_ -> return $ Session.FlowStartRegular ServerOutboundHeaders)

        handlerTeardown :: ServerCall -> Either SomeException () -> IO ()
        handlerTeardown channel mRes = void $
            case mRes of
              Right () -> Session.close channel $ ExitCaseSuccess ()
              Left err -> Session.close channel $ ExitCaseException err

    let handler ::
             Session.Channel TrivialServer
          -> (forall a. IO a -> IO a)
          -> IO ()
        handler channel unmask = do
            mRes <- try $ unmask $ k channel
            handlerTeardown channel mRes

    runHandler setupResponseChannel handler
  where
    callSession :: TrivialServer
    callSession = TrivialServer

runHandler ::
     IO (Session.Channel TrivialServer)
  -> (Session.Channel TrivialServer -> (forall a. IO a -> IO a) -> IO ())
  -> IO ()
runHandler setupResponseChannel handler = do
    mask $ \unmask -> do
      -- This sets up the channel but no response is sent yet
      callChannel   <- setupResponseChannel
      handlerThread <- asyncWithUnmask $ handler callChannel

      let loop :: IO ()
          loop = do
             -- We do not need to use 'waitCatch': the handler installs its
             -- own exception handler and will not throw any exceptions.
             mRes <- try $ unmask $ wait handlerThread
             case mRes of
               Right ()  -> return ()
               Left  err -> do
                 case fromException err of
                   Just (HTTP2.KilledByHttp2ThreadManager mErr) -> do
                     let exitReason :: ExitCase ()
                         exitReason =
                           case mErr of
                             Nothing -> ExitCaseSuccess ()
                             Just exitWithException ->
                               ExitCaseException . toException $
                                 ClientDisconnected exitWithException
                     _mAlreadyClosed <- Session.close callChannel exitReason
                     loop
                   Nothing -> do
                     cancelWith handlerThread err
                     throwIO err

      loop

serverRecvInput :: ServerCall -> IO (StreamElem () Void)
serverRecvInput channel = atomically $
    first (const ()) <$> Session.recv channel

serverSendOutput :: ServerCall -> StreamElem () Void -> IO ()
serverSendOutput channel msg = do
    atomically $ Session.send channel msg
    StreamElem.whenDefinitelyFinal msg $ \_ ->
      void $ Session.waitForOutbound channel

serverIsCallHealthy :: ServerCall -> STM Bool
serverIsCallHealthy = Session.isChannelHealthy

-- ========================================================================== --

type ServerConnection = (Strict.ByteString, Session.ConnectionToClient)

withServerConnection ::  (ServerConnection -> IO ()) -> HTTP2.Server
withServerConnection k request _aux respond' =
    k conn
  where
    connectionToClient :: Session.ConnectionToClient
    connectionToClient = Session.ConnectionToClient{request, respond}

    conn :: ServerConnection
    conn = (
          fromMaybe "" $ HTTP2.requestPath request
        , connectionToClient
        )

    respond :: HTTP2.Response -> IO ()
    respond = flip respond' []

-- ========================================================================== --

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

clientLocal1 :: HasCallStack => TestClock -> Authority -> IO ()
clientLocal1 testClock auth = handle showExceptions $ do
    waitForTestClockTick testClock 0
    clientWithConnection auth $ \conn -> do
      clientWithRPC conn (Proxy @"test1") $ \_call -> do
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
      clientWithRPC conn (Proxy @"test2") $ \call -> do
        waitForTestClockTick testClock 3
        clientSendInput call $ NoMoreElems ()
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

serverLocal1 :: TestClock -> ServerCall -> IO ()
serverLocal1 testClock call = handle showExceptions $ do
    advanceTestClock testClock

    -- Wait for client early termination to become visible
    atomically $ do
      healthy <- serverIsCallHealthy call
      when healthy $ retry

    -- At this point, sending anything should fail
    waitForTestClockTick testClock 4
    serverSendOutput call $ NoMoreElems ()
  where
    showExceptions :: SomeException -> IO ()
    showExceptions err = do
        putStrLn $ "serverLocal1: " ++ show err
        throwIO err

serverLocal2 :: TestClock -> ServerCall -> IO ()
serverLocal2 testClock call = handle showExceptions $ do
    advanceTestClock testClock

    -- Wait for client to tell us they will send no more elements
    _ <- serverRecvInput call
    advanceTestClock testClock

    -- Tell the client we won't send them more elements either
    waitForTestClockTick testClock 5
    serverSendOutput call $ NoMoreElems ()
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
      let serverHandlers :: HandlerMap
          serverHandlers = [
                ("/trivial/test1", serverLocal1 testClock)
              , ("/trivial/test2", serverLocal2 testClock)
              ]

      runServer "50051" $ mkServer serverHandlers

    -- Give the server a chance to start
    threadDelay 500_000

    _client <- async $ clientGlobal testClock (Authority "localhost" 50051)

    -- Wait for the test clock to reach 4 (which it won't)
    -- (This just prevents the test from terminating too soon)
    waitForTestClockTick testClock 4
