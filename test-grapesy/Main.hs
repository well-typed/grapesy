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
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.C8
import Data.Maybe (fromMaybe)
import Data.Proxy
import Debug.Trace
import GHC.Stack
import GHC.TypeLits
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Client qualified as Client
import Network.HTTP2.Client qualified as HTTP2.Client
import Network.HTTP2.Internal qualified as HTTP2
import Network.HTTP2.Server qualified as HTTP2
import Network.HTTP2.Server qualified as Server
import Network.Run.TCP (runTCPServer)
import Network.Run.TCP qualified as Run
import Network.Socket

import Network.GRPC.Util.Concurrency
import Network.GRPC.Util.HTTP2.Stream
import Network.GRPC.Util.Thread

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

      channel <- setupRequestChannel TrivialClient conn

      mb :: Either SomeException () <- unmask $ try $ k channel
      _ <- sessionClose channel (ExitCaseSuccess ()) -- simplification
      either throwIO return mb

clientSendInput :: HasCallStack => ClientCall meth -> IO ()
clientSendInput channel = do
    atomically $ sessionSend channel
    void $ sessionWaitForOutbound channel

clientRecvOutput :: ClientCall meth -> IO ()
clientRecvOutput channel = atomically $ sessionRecv channel

-- ========================================================================== --

type ClientConnection = TVar ConnectionState
type ClientCall meth  = Channel (TrivialClient meth)

clientWithConnection :: HasCallStack => (ClientConnection -> IO a) -> IO a
clientWithConnection k = do
    clientConnState <- newTVarIO ConnectionNotReady

    connCanClose <- newEmptyMVar
    let stayConnectedThread :: IO ()
        stayConnectedThread = clientStayConnected clientConnState connCanClose

    void $ forkIO $ stayConnectedThread
    k clientConnState `finally` putMVar connCanClose ()

data ConnectionState =
    ConnectionNotReady
  | ConnectionReady (TMVar (Maybe SomeException)) ConnectionToServer
  | ConnectionAbandoned SomeException
  | ConnectionClosed

clientStayConnected :: TVar ConnectionState -> MVar () -> IO ()
clientStayConnected connVar connCanClose =
    loop
  where
    loop :: IO ()
    loop = do
        connClosed <- newEmptyTMVarIO

        mRes <- try $
          runTCPClient $ \sock ->
            bracket (HTTP2.Client.allocSimpleConfig sock 4096)
                    HTTP2.Client.freeSimpleConfig $ \conf ->
              HTTP2.Client.run clientConfig conf $ \sendRequest _aux -> do
                let conn = ConnectionToServer sendRequest
                atomically $ writeTVar connVar $ ConnectionReady connClosed conn
                takeMVar connCanClose

        atomically $ putTMVar connClosed $ either Just (\() -> Nothing) mRes

        atomically $ do
          case mRes of
            Right () -> writeTVar connVar $ ConnectionClosed
            Left err -> writeTVar connVar $ ConnectionAbandoned err

    runTCPClient :: (Socket -> IO a) -> IO a
    runTCPClient =
        Run.runTCPClient "localhost" "50051"

    clientConfig :: HTTP2.Client.ClientConfig
    clientConfig = HTTP2.Client.defaultClientConfig {
          HTTP2.Client.scheme    = "http"
        , HTTP2.Client.authority = "localhost"
        }

-- ========================================================================== --

data TrivialClient  (meth :: Symbol) = TrivialClient

instance KnownSymbol meth => InitiateSession (TrivialClient meth) where
  buildRequestInfo _ = RequestInfo {
        requestMethod  = "POST"
      , requestPath    = "/trivial/" <> BS.C8.pack (symbolVal (Proxy @meth))
      , requestHeaders = []
      }

-- ========================================================================== --

data TrivialServer = TrivialServer

instance AcceptSession TrivialServer where
  buildResponseInfo _ = ResponseInfo HTTP.ok200 []

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

type ServerCall = Channel TrivialServer

serverAcceptCall :: ServerConnection -> (ServerCall -> IO ()) -> IO ()
serverAcceptCall (_path, conn) k = do
    let setupResponseChannel' :: IO (Channel TrivialServer)
        setupResponseChannel' = setupResponseChannel callSession conn

        handlerTeardown :: ServerCall -> Either SomeException () -> IO ()
        handlerTeardown channel mRes = void $
            case mRes of
              Right () -> sessionClose channel $ ExitCaseSuccess ()
              Left err -> sessionClose channel $ ExitCaseException err

    let handler ::
             Channel TrivialServer
          -> (forall a. IO a -> IO a)
          -> IO ()
        handler channel unmask = do
            mRes <- try $ unmask $ k channel
            handlerTeardown channel mRes

    runHandler setupResponseChannel' handler
  where
    callSession :: TrivialServer
    callSession = TrivialServer

runHandler ::
     IO (Channel TrivialServer)
  -> (Channel TrivialServer -> (forall a. IO a -> IO a) -> IO ())
  -> IO ()
runHandler setupResponseChannel' handler = do
    mask $ \unmask -> do
      -- This sets up the channel but no response is sent yet
      callChannel   <- setupResponseChannel'
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
                     _mAlreadyClosed <- sessionClose callChannel exitReason
                     loop
                   Nothing -> do
                     cancelWith handlerThread err
                     throwIO err

      loop

serverRecvInput :: ServerCall -> IO ()
serverRecvInput channel = atomically $ sessionRecv channel

serverSendOutput :: ServerCall -> IO ()
serverSendOutput channel = do
    atomically $ sessionSend channel
    void $ sessionWaitForOutbound channel

serverIsCallHealthy :: ServerCall -> STM Bool
serverIsCallHealthy = sessionIsChannelHealthy

-- ========================================================================== --

type ServerConnection = (Strict.ByteString, ConnectionToClient)

withServerConnection ::  (ServerConnection -> IO ()) -> HTTP2.Server
withServerConnection k request _aux respond' =
    k conn
  where
    connectionToClient :: ConnectionToClient
    connectionToClient = ConnectionToClient{request, respond}

    conn :: ServerConnection
    conn = (
          fromMaybe "" $ HTTP2.requestPath request
        , connectionToClient
        )

    respond :: HTTP2.Response -> IO ()
    respond = flip respond' []


-- ========================================================================== --

{-------------------------------------------------------------------------------
  Preliminaries
-------------------------------------------------------------------------------}

data RequestInfo = RequestInfo {
      requestMethod  :: HTTP.Method
    , requestPath    :: HTTP2.Path
    , requestHeaders :: [HTTP.Header]
    }

data ResponseInfo = ResponseInfo {
      responseStatus  :: HTTP.Status
    , responseHeaders :: [HTTP.Header]
    }

{-------------------------------------------------------------------------------
  Main definition
-------------------------------------------------------------------------------}

class InitiateSession sess where
  buildRequestInfo :: sess -> RequestInfo

class AcceptSession sess where
  buildResponseInfo :: sess -> ResponseInfo

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Misbehaving peer
--
-- Although this exception could in principle be caught, there is not much that
-- can be done to rectify the situation: probably this peer should just be
-- avoided (although perhaps one can hope that the problem was transient).
data PeerException =
    -- | Peer sent a malformed message (parser returned an error)
    PeerSentMalformedMessage String

    -- | Peer sent an incomplete message (parser did not consume all data)
  | PeerSentIncompleteMessage

    -- | HTTP request missing @:method@ pseudo-header
  | PeerMissingPseudoHeaderMethod

    -- | HTTP request missing @:path@ pseudo-header
  | PeerMissingPseudoHeaderPath

    -- | HTTP response missing @:status@ pseudo-header
  | PeerMissingPseudoHeaderStatus
  deriving stock (Show)
  deriving anyclass (Exception)

-- ========================================================================== --

{-------------------------------------------------------------------------------
  Definitions

  The fields of 'Channel' are its /implementation/, not its interface. It is
  kept opaque in the top-level @.Peer@ module.

  Implementation note: it is tempting to try and define 'Channel' purely in
  terms of bytestrings, and deal with serialization and deserialization to and
  from messages in a higher layer. However, this does not work:

  - For deserialization, if we make chunks of messages available in the 'TMVar',
    then if multiple threads are reading from that one 'TMVar', one thread might
    get the first chunk of a message and another thread the second.
  - Similarly, for serialization, if multiple threads are trying to write to the
    'TMVar', we might get interleaving of fragments of messages.

  Thus, to ensure thread safety, we work at the level of messages, not bytes.
-------------------------------------------------------------------------------}

-- | Bidirectional open channel on a node to a peer node
--
-- The node might be a client (and its peer a server), or the node might be
-- a server (and its peer a client); the main purpose of this abstraction
-- is precisely to abstract over that difference.
--
-- Each channel is constructed for a /single/ session (request/response).
data Channel sess = Channel {
      -- | Thread state of the thread receiving messages from the peer
      channelInbound :: TVar (ThreadState (TMVar RegularFlowState))

      -- | Thread state of the thread sending messages to the peer
    , channelOutbound :: TVar (ThreadState (TMVar RegularFlowState))

      -- | 'CallStack' of the final call to 'send'
      --
      -- The sole purpose of this 'TVar' is catching user mistakes: if there is
      -- another 'send' after the final message, we can throw an exception,
      -- rather than the message simply being lost or blockng indefinitely.
    , channelSentFinal :: TVar (Maybe CallStack)

      -- | 'CallStack' of the final call to 'recv'
      --
      -- This is just to improve the user experience; see 'channelSentFinal'.
    , channelRecvFinal :: TVar (Maybe CallStack)
    }

-- | Regular (streaming) flow state
data RegularFlowState = RegularFlowState {
      -- | Messages
      --
      -- This TMVar is written to for incoming messages ('recvMessageLoop') and
      -- read from for outgoing messages ('sendMessageLoop'). It acts as a
      -- one-place buffer, providing backpressure in both directions.
      --
      -- TODO: It might make sense to generalize this to an @N@-place buffer,
      -- for configurable @N@. This might result in better latency masking.
      flowMsg :: TMVar ()

      -- | Trailers
      --
      -- Unlike 'flowMsg', which is /written/ to in 'recvMessageLoop' and /read/
      -- from in 'sendMessageLoop', both loops /set/ 'flowTerminated', once,
      -- just before they terminate.
      --
      -- * For 'sendMessageLoop', this means that the last message has been
      --   written (that is, the last call to 'writeChunk' has happened).
      --   This has two consequences:
      --
      --   1. @http2@ can now construct the trailers ('outboundTrailersMaker')
      --   2. Higher layers can wait on 'flowTerminated' to be /sure/ that the
      --      last message has been written.
      --
      -- * For 'recvMessageLoop', this means that the trailers have been
      --   received from the peer. Higher layers can use this to check for, or
      --   block-and-wait, to receive those trailers.
      --
      -- == Relation to 'channelSentFinal'/'channelRecvFinal'
      --
      -- 'flowTerminated' is set at different times than 'channelSentFinal' and
      -- 'channelRecvFinal' are:
      --
      -- * 'channelSentFinal' is set on the last call to 'send', but /before/
      --   the message is processed by 'sendMessageLoop'.
      -- * 'channelRecvFinal', dually, is set on the last call to 'recv, which
      --   must (necessarily) happen /before/ that message is actually made
      --   available by 'recvMessageLoop'.
      --
      -- /Their/ sole purpose is to catch user errors, not capture data flow.
    , flowTerminated :: TMVar ()
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

initChannel :: IO (Channel sess)
initChannel =
    Channel
      <$> newThreadState
      <*> newThreadState
      <*> newTVarIO Nothing
      <*> newTVarIO Nothing

initFlowStateRegular :: IO RegularFlowState
initFlowStateRegular = do
    RegularFlowState
      <$> newEmptyTMVarIO
      <*> newEmptyTMVarIO

{-------------------------------------------------------------------------------
  Status check
-------------------------------------------------------------------------------}

data ChannelStatus sess = ChannelStatus {
      channelStatusInbound   :: FlowStatus
    , channelStatusOutbound  :: FlowStatus
    , channelStatusSentFinal :: Bool
    , channelStatusRecvFinal :: Bool
    }

data FlowStatus =
    FlowNotYetEstablished
  | FlowEstablished RegularFlowState
  | FlowTerminated
  | FlowFailed SomeException

-- | Get channel status
--
-- This is inherently non-deterministic: it is entirely possible that the
-- connection to the peer has already been broken but we haven't noticed yet,
-- or that the connection will be broken immediately after this call and before
-- whatever the next call is.
checkChannelStatus :: Channel sess -> STM (ChannelStatus sess)
checkChannelStatus Channel{ channelInbound
                   , channelOutbound
                   , channelSentFinal
                   , channelRecvFinal
                   } = do
    stInbound  <- readTVar channelInbound
    stOutbound <- readTVar channelOutbound
    channelStatusInbound   <- go stInbound
    channelStatusOutbound  <- go stOutbound
    channelStatusSentFinal <- isFinal <$> readTVar channelSentFinal
    channelStatusRecvFinal <- isFinal <$> readTVar channelRecvFinal
    return ChannelStatus{
        channelStatusInbound
      , channelStatusOutbound
      , channelStatusSentFinal
      , channelStatusRecvFinal
      }
  where
    go :: ThreadState (TMVar RegularFlowState) -> STM FlowStatus
    go ThreadNotStarted       = return FlowNotYetEstablished
    go (ThreadInitializing _) = return FlowNotYetEstablished
    go (ThreadRunning _ a)    = FlowEstablished <$> readTMVar a
    go (ThreadDone _)         = return $ FlowTerminated
    go (ThreadException e)    = return $ FlowFailed e

    isFinal :: Maybe CallStack -> Bool
    isFinal = maybe False (const True)

-- | Check if the channel is healthy
--
-- This is a simplified API on top of 'getChannelStatus, which
--
-- * retries if the flow in either direction has not yet been established
-- * returns 'False' if the flow in either direction has been terminated,
--   or has failed
-- * returns 'True' otherwise.
sessionIsChannelHealthy :: Channel sess -> STM Bool
sessionIsChannelHealthy channel = do
    status <- checkChannelStatus channel
    case (channelStatusInbound status, channelStatusOutbound status) of
      (FlowNotYetEstablished , _                    ) -> retry
      (_                     , FlowNotYetEstablished) -> retry

      (FlowTerminated        , _                    ) -> return False
      (_                     , FlowTerminated       ) -> return False
      (FlowFailed _          , _                    ) -> return False
      (_                     , FlowFailed _         ) -> return False

      (FlowEstablished _     , FlowEstablished _    ) -> return True

{-------------------------------------------------------------------------------
  Working with an open channel
-------------------------------------------------------------------------------}

-- | Send a message to the node's peer
--
-- It is a bug to call 'send' again after the final message (that is, a message
-- which 'StreamElem.definitelyFinal' considers to be final). Doing so will
-- result in a 'SendAfterFinal' exception.
sessionSend ::
     HasCallStack
  => Channel sess
  -> STM ()
sessionSend Channel{channelOutbound, channelSentFinal} = do
    -- By checking that we haven't sent the final message yet, we know that this
    -- call to 'putMVar' will not block indefinitely: the thread that sends
    -- messages to the peer will get to it eventually (unless it dies, in which
    -- case the thread status will change and the call to 'getThreadInterface'
    -- will be retried).
    sentFinal <- readTVar channelSentFinal
    case sentFinal of
      Just cs -> throwSTM $ SendAfterFinal cs
      Nothing -> do
        regular <- readTMVar =<< getThreadInterface channelOutbound
        writeTVar channelSentFinal $ Just callStack
        putTMVar (flowMsg regular) ()

-- | Receive a message from the node's peer
--
-- It is a bug to call 'recv' again after receiving the final message; Doing so
-- will result in a 'RecvAfterFinal' exception.
sessionRecv ::
     HasCallStack
  => Channel sess
  -> STM ()
sessionRecv Channel{channelInbound, channelRecvFinal} = do
    iface <- getThreadInterface channelInbound
    regular <- readTMVar iface
    takeTMVar (flowMsg regular)
    writeTVar channelRecvFinal $ Just callStack

-- | Thrown by 'send'
--
-- The 'CallStack' is the callstack of the final call to 'send'.
--
-- See 'send' for additional discussion.
data SendAfterFinal =
    -- | Call to 'send' after the final message was sent
    SendAfterFinal CallStack

    -- | Call to 'send', but we are in the Trailers-Only case
  | SendButTrailersOnly
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Thrown by 'recv'
--
-- The 'CallStack' is the callstack of the final call to 'recv'.
--
-- See 'recv' for additional discussion.
data RecvAfterFinal =
     -- | Call to 'recv' after the final message was already received
     RecvAfterFinal CallStack
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Closing
-------------------------------------------------------------------------------}

-- | Wait for the outbound thread to terminate
--
-- See 'forceClose' for discussion.
sessionWaitForOutbound :: Channel sess -> IO (RegularFlowState)
sessionWaitForOutbound Channel{channelOutbound} = atomically $
    readTMVar =<< waitForThread channelOutbound

-- | Close the channel
--
-- Before a channel can be closed, you should 'send' the final outbound message
-- and then 'waitForOutbound' until all outbound messages have been processed.
-- Not doing so is considered a bug (it is not possible to do this implicitly,
-- because the final call to 'send' involves a choice of trailers, and calling
-- 'waitForOutbound' /without/ a final close to 'send' will result in deadlock).
-- Typically code will also process all /incoming/ messages, but doing so of
-- course not mandatory.
--
-- Calling 'close' will kill the outbound thread ('sendMessageLoop'), /if/ it is
-- still running. If the thread was terminated with an exception, this could
-- mean one of two things:
--
-- * The connection to the peer was lost
-- * Proper procedure for outbound messages was not followed (see above)
--
-- In this case, 'close' will return a 'ChannelUncleanClose' exception.
--
-- TODO: @http2@ does not offer an API for indicating that we want to ignore
-- further output. We should check that this does not result in memory leak (if
-- the server keeps sending data and we're not listening.)
sessionClose ::
     HasCallStack
  => Channel sess
  -> ExitCase a    -- ^ The reason why the channel is being closed
  -> IO (Maybe ChannelUncleanClose)
sessionClose Channel{channelOutbound} reason = do
    -- We leave the inbound thread running. Although the channel is closed,
    -- there might still be unprocessed messages in the queue. The inbound
    -- thread will terminate once it reaches the end of the queue
    -- (this relies on nhttps://github.com/kazu-yamamoto/http2/pull/83).
     outbound <- cancelThread channelOutbound $ toException channelClosed
     case outbound of
       Right _   -> return $ Nothing
       Left  err -> return $ Just (ChannelUncleanClose err)
  where
    channelClosed :: ChannelClosed
    channelClosed =
        case reason of
          ExitCaseSuccess _   -> ChannelDiscarded callStack
          ExitCaseException e -> ChannelException callStack e
          ExitCaseAbort       -> ChannelAborted   callStack

-- | Thrown by 'close' if not all outbound messages have been processed
--
-- See 'close' for discussion.
data ChannelUncleanClose = ChannelUncleanClose SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Thrown to the inbound and outbound threads by 'close'
data ChannelClosed =
    -- | Channel was closed because it was discarded
    --
    -- This typically corresponds to leaving the scope of 'acceptCall' or
    -- 'withRPC' (without throwing an exception).
    ChannelDiscarded CallStack

    -- | Channel was closed with an exception
  | ChannelException CallStack SomeException

    -- | Channel was closed for an unknown reason
    --
    -- This will only be used in monad stacks that have error mechanisms other
    -- than exceptions.
  | ChannelAborted CallStack
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Constructing channels
-------------------------------------------------------------------------------}

-- | Send all messages to the node's peer
--
-- Should be called with exceptions masked.
sendMessageLoop :: forall sess.
     (forall x. IO x -> IO x) -- ^ Unmask
  -> Proxy sess
  -> RegularFlowState
  -> IO ()
sendMessageLoop unmask _proxy st = do
    unmask $ atomically $ takeTMVar (flowMsg st)
    atomically $ putTMVar (flowTerminated st) ()

-- | Receive all messages sent by the node's peer
--
-- Should be called with exceptions masked.
--
-- TODO: This is wrong, we are never marking the final element as final.
-- (But fixing this requires a patch to http2.)
recvMessageLoop :: forall sess.
     (forall x. IO x -> IO x) -- ^ Unmask
  -> Proxy sess
  -> RegularFlowState
  -> InputStream
  -> IO ()
recvMessageLoop unmask _proxy st stream = do
    go
  where
    go :: IO ()
    go = do
        loop
        atomically $ putTMVar (flowTerminated st) $ ()
        atomically $ putTMVar (flowMsg        st) $ ()
      where
        loop :: IO ()
        loop = do
            mbs <- unmask $ try $ getChunk stream
            case mbs of
              Left (err :: SomeException) -> throwIO err
              Right _ -> error "unexpected chunk"


outboundTrailersMaker :: Channel sess -> HTTP2.TrailersMaker
outboundTrailersMaker channel = go
  where
    go :: HTTP2.TrailersMaker
    go (Just _) = return $ HTTP2.NextTrailersMaker go
    go Nothing  = do
        regular <- sessionWaitForOutbound channel
        ()      <- atomically $ readTMVar $ flowTerminated regular
        return $ HTTP2.Trailers []

-- ========================================================================== --

{-------------------------------------------------------------------------------
  Connection
-------------------------------------------------------------------------------}

-- | Connection to the client, as provided by @http2@
data ConnectionToClient = ConnectionToClient {
      request :: Server.Request
    , respond :: Server.Response -> IO ()
    }

{-------------------------------------------------------------------------------
  Initiate response
-------------------------------------------------------------------------------}

-- | Setup response channel
--
-- The actual response will not immediately be initiated; see below.
setupResponseChannel :: forall sess.
     (AcceptSession sess, HasCallStack)
  => sess
  -> ConnectionToClient
  -> IO (Channel sess)
setupResponseChannel sess conn = do
    channel <- initChannel

    forkThread (channelInbound channel) newEmptyTMVarIO $ \unmask stVar -> do
      regular <- initFlowStateRegular
      stream  <- serverInputStream (request conn)
      atomically $ putTMVar stVar regular
      recvMessageLoop unmask (Proxy @sess) regular stream

    forkThread (channelOutbound channel) newEmptyTMVarIO $ \unmask stVar -> do
      let responseInfo = buildResponseInfo sess
      regular <- initFlowStateRegular
      atomically $ putTMVar stVar regular
      let resp :: Server.Response
          resp = Server.responseStreaming
                       (responseStatus  responseInfo)
                       (responseHeaders responseInfo)
               $ \write' flush' -> do
                    _stream <- serverOutputStream write' flush'
                    sendMessageLoop unmask (Proxy @sess) regular
      respond conn resp

    return channel

-- ========================================================================== --

{-------------------------------------------------------------------------------
  Connection
-------------------------------------------------------------------------------}

-- | Connection to the server, as provided by @http2@
data ConnectionToServer = ConnectionToServer {
      sendRequest ::
           Client.Request
        -> (Client.Response -> IO ())
        -> IO ()
    }

{-------------------------------------------------------------------------------
  Initiate request

  Control flow and exception handling here is a little tricky.

  * 'sendRequest' (coming from @http2@) will itself spawn a separate thread to
    deal with sending inputs to the server (here, that is 'sendMessageLoop').

  * As the last thing it does, 'sendRequest' will then call its continuation
    argument in whatever thread it was called. Here, that continuation argument
    is 'recvMessageLoop' (dealing with outputs sent by the server), which we
    want to run in a separate thread also. We must therefore call 'sendRequest'
    in a newly forked thread.

    Note that 'sendRequest' will /only/ call its continuation once it receives
    a response from the server. Some servers will not send the (start of the)
    response until it has received (part of) the request, so it is important
    that we do not wait on 'sendRequest' before we return control to the caller.

  * We need to decide what to do with any exceptions that might arise in these
    threads. One option might be to rethrow those exceptions to the parent
    thread, but this presupposes a certain level of hygiene in client code: if
    the client code spawns threads of their own, and shares the open RPC call
    between them, then we rely on the client code to propagate the exception
    further.

    We therefore choose a different approach: we do not try to propagate the
    exception at all, but instead ensure that any (current or future) call to
    'read' or 'write' (or 'push') on the open 'Peer' will result in an exception
    when the respective threads have died.

  * This leaves one final problem to deal with: if setting up the request
    /itself/ throws an exception, one or both threads might never get started.
    To avoid client code blocking indefinitely we will therefore catch any
    exceptions that arise during the call setup, and use them to 'cancel' both
    threads (which will kill them or ensure they never get started).
-------------------------------------------------------------------------------}

-- | Setup request channel
--
-- This initiate a new request.
setupRequestChannel :: forall sess.
     InitiateSession sess
  => sess
  -> ConnectionToServer
  -> IO (Channel sess)
setupRequestChannel sess ConnectionToServer{sendRequest} = do
    channel <- initChannel
    let requestInfo = buildRequestInfo sess

    regular <- initFlowStateRegular
    let req :: Client.Request
        req = setRequestTrailers channel
            $ Client.requestStreamingUnmask
                (requestMethod  requestInfo)
                (requestPath    requestInfo)
                (requestHeaders requestInfo)
            $ \unmask write' flush' -> unmask $ do
                 threadBody (channelOutbound channel)
                            (newTMVarIO regular)
                          $ \_stVar -> do
                   _stream <- clientOutputStream write' flush'
                   sendMessageLoop unmask (Proxy @sess) regular
    forkRequest channel req

    return channel
  where
    forkRequest :: Channel sess -> Client.Request -> IO ()
    forkRequest channel req =
        forkThread (channelInbound channel) newEmptyTMVarIO $ \unmask stVar -> do
          setup <- try $ sendRequest req $ \resp -> do
            regular <- initFlowStateRegular
            stream  <- clientInputStream resp
            atomically $ putTMVar stVar regular
            recvMessageLoop unmask (Proxy @sess) regular stream

          case setup of
            Right () -> return ()
            Left  e  -> do
              void $ cancelThread (channelOutbound channel) e
              throwIO e

{-------------------------------------------------------------------------------
   Auxiliary http2
-------------------------------------------------------------------------------}

setRequestTrailers ::
     Channel sess
  -> Client.Request -> Client.Request
setRequestTrailers channel req =
    Client.setRequestTrailersMaker req $
      outboundTrailersMaker channel

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

clientLocal1 :: HasCallStack => TestClock -> IO ()
clientLocal1 testClock = handle showExceptions $ do
    waitForTestClockTick testClock 0
    clientWithConnection $ \conn -> do
      clientWithRPC conn (Proxy @"test1") $ \_call -> do
        waitForTestClockTick testClock 2
        throwIO $ userError "this models some kind of client exception"
  where
    showExceptions :: SomeException -> IO ()
    showExceptions err = do
        putStrLn $ "clientLocal1: " ++ show err
        throwIO err

clientLocal2 :: HasCallStack => TestClock -> IO ()
clientLocal2 testClock = handle showExceptions $ do
    waitForTestClockTick testClock 1
    clientWithConnection $ \conn -> do
      clientWithRPC conn (Proxy @"test2") $ \call -> do
        waitForTestClockTick testClock 3
        clientSendInput call
        _ <- clientRecvOutput call
        advanceTestClock testClock
  where
    showExceptions :: SomeException -> IO ()
    showExceptions err = do
        putStrLn $ "clientLocal2: " ++ show err
        throwIO err

clientGlobal :: TestClock -> IO ()
clientGlobal testClock =
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
    withAsync (clientLocal2 testClock) $ \thread2 ->
    withAsync (clientLocal1 testClock) $ \thread1 ->
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
    serverSendOutput call
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
    serverSendOutput call
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

    _client <- async $ clientGlobal testClock

    -- Wait for the test clock to reach 4 (which it won't)
    -- (This just prevents the test from terminating too soon)
    waitForTestClockTick testClock 4
