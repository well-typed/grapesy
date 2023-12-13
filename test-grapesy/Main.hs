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
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Char8 qualified as BS.C8
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Void
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

import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Util.Concurrency
import Network.GRPC.Util.Parser
import Network.GRPC.Util.HTTP2 (fromHeaderTable)
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

      channel <- setupRequestChannel TrivialClient nullTracer conn flowStart

      mb :: Either SomeException () <- unmask $ try $ k channel
      _ <- sessionClose channel (ExitCaseSuccess ()) -- simplification
      either throwIO return mb
  where
    flowStart :: FlowStart (ClientOutbound rpc)
    flowStart = FlowStartRegular ClientOutboundHeaders

clientSendInput :: HasCallStack => ClientCall meth -> StreamElem () Void -> IO ()
clientSendInput channel msg = do
    atomically $ sessionSend channel msg
    StreamElem.whenDefinitelyFinal msg $ \_ ->
      void $ sessionWaitForOutbound channel

clientRecvOutput :: ClientCall meth -> IO (StreamElem () Void)
clientRecvOutput channel = atomically $
    first (const ()) <$> sessionRecv channel

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
data ClientInbound  (meth :: Symbol)
data ClientOutbound (meth :: Symbol)

instance DataFlow (ClientInbound meth) where
  data Headers    (ClientInbound meth) = ClientInboundHeaders deriving (Show)
  type Message    (ClientInbound meth) = Void
  type Trailers   (ClientInbound meth) = ()
  type NoMessages (ClientInbound meth) = Void

instance DataFlow (ClientOutbound meth) where
  data Headers    (ClientOutbound meth) = ClientOutboundHeaders deriving (Show)
  type Message    (ClientOutbound meth) = Void
  type Trailers   (ClientOutbound meth) = ()
  type NoMessages (ClientOutbound meth) = Void

instance IsSession (TrivialClient meth) where
  type Inbound  (TrivialClient meth) = ClientInbound  meth
  type Outbound (TrivialClient meth) = ClientOutbound meth

  buildOutboundTrailers _ _ = []
  parseInboundTrailers  _ _ = return ()
  buildMsg              _ _ = absurd
  parseMsg              _ _ = absurdParser

instance KnownSymbol meth => InitiateSession (TrivialClient meth) where
  parseResponseRegular    _ _ = return ClientInboundHeaders
  parseResponseNoMessages _ _ = throwIO $ userError "Unexpected Trailers-Only"

  buildRequestInfo _ _ = RequestInfo {
        requestMethod  = "POST"
      , requestPath    = "/trivial/" <> BS.C8.pack (symbolVal (Proxy @meth))
      , requestHeaders = []
      }

-- ========================================================================== --

data TrivialServer = TrivialServer
data ServerInbound
data ServerOutbound

instance DataFlow ServerInbound where
  data Headers    ServerInbound = ServerInboundHeaders deriving (Show)
  type Message    ServerInbound = Void
  type Trailers   ServerInbound = ()
  type NoMessages ServerInbound = Void

instance DataFlow ServerOutbound where
  data Headers    ServerOutbound = ServerOutboundHeaders deriving (Show)
  type Message    ServerOutbound = Void
  type Trailers   ServerOutbound = ()
  type NoMessages ServerOutbound = Void

instance IsSession TrivialServer where
  type Inbound  TrivialServer = ServerInbound
  type Outbound TrivialServer = ServerOutbound

  buildOutboundTrailers _ _ = []
  parseInboundTrailers  _ _ = return ()
  buildMsg              _ _ = absurd
  parseMsg              _ _ = absurdParser

instance AcceptSession TrivialServer where
  parseRequestRegular    _ _ = return ServerInboundHeaders
  parseRequestNoMessages _ _ = throwIO $ userError "Unexpected Trailers-Only"
  buildResponseInfo      _ _ = ResponseInfo HTTP.ok200 []

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
        setupResponseChannel' =
            setupResponseChannel
              callSession
              nullTracer
              conn
              (\_ -> return $ FlowStartRegular ServerOutboundHeaders)

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

serverRecvInput :: ServerCall -> IO (StreamElem () Void)
serverRecvInput channel = atomically $
    first (const ()) <$> sessionRecv channel

serverSendOutput :: ServerCall -> StreamElem () Void -> IO ()
serverSendOutput channel msg = do
    atomically $ sessionSend channel msg
    StreamElem.whenDefinitelyFinal msg $ \_ ->
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

-- | Flow of data in a session
--
-- This describes the flow of data in /one/ direction. The normal flow of data
-- is as follows:
--
-- 1. (Proper) Headers
-- 2. Messages
-- 3. Trailers
--
-- However, in the case that there /are/ no messages, this whole thing collapses
-- and we just have headers (in gRPC this is referred to as the Trailers-Only
-- case, but we avoid that terminology here).
--
-- * It looks different on the wire: in the regular case, we will have /two/
--   HTTP @Headers@ frames, but in the absence of messages we only have one.
-- * Applications may in turn treat this case special, using a different set of
--   headers (specifically, this is the case for gRPC).
class ( Show (Headers    flow)
      , Show (Message    flow)
      , Show (Trailers   flow)
      , Show (NoMessages flow)
      ) => DataFlow flow where
  data Headers    flow :: Type
  type Message    flow :: Type
  type Trailers   flow :: Type
  type NoMessages flow :: Type

-- | Start of data flow
--
-- See 'DataFlow' for discussion.
data FlowStart flow =
    FlowStartRegular    (Headers    flow)
  | FlowStartNoMessages (NoMessages flow)

deriving instance DataFlow flow => Show (FlowStart flow)

-- | Session between two nodes in the network
--
-- The session is described from the point of view of /this/ node, who is
-- talking to a peer node. For example, if this node is a client, then the peer
-- is a server, the outbound headers correspond to a request and the inbound
-- headers correspond to a response (see also 'InitiateSession').
--
-- We avoid referring to \"inputs\" or \"outputs\" here, but instead talk about
-- \"inbound\" or \"outbound\". When we are dealing with gRPC, \"inputs\" are
-- outbound for the client and inbound for the server, and \"outputs\" are
-- inbound for the client and outbound for the server.
class ( DataFlow (Inbound  sess)
      , DataFlow (Outbound sess)
      ) => IsSession sess where
  type Inbound  sess :: Type
  type Outbound sess :: Type

  -- | Parse proper trailers
  parseInboundTrailers ::
       sess
    -> [HTTP.Header]
    -> IO (Trailers (Inbound sess))

  -- | Build proper trailers
  buildOutboundTrailers ::
       sess
    -> Trailers (Outbound sess)
    -> [HTTP.Header]

  -- | Parse message
  parseMsg ::
       sess
    -> Headers (Inbound sess)
    -> Parser (Message (Inbound sess))

  -- | Build message
  buildMsg ::
       sess
    -> Headers (Outbound sess)
    -> Message (Outbound sess)
    -> Builder

-- | Initiate new session
--
-- A client node connects to a server, and initiates the request.
class IsSession sess => InitiateSession sess where
  -- | Build 'RequestInfo' for the server
  buildRequestInfo ::
       sess
    -> FlowStart (Outbound sess) -> RequestInfo

  -- | Parse 'ResponseInfo' from the server, regular case
  --
  -- See 'parseResponseTrailersOnly' for the Trailers-Only case.
  parseResponseRegular ::
       sess
    -> ResponseInfo -> IO (Headers (Inbound sess))

  -- | Parse 'ResponseInfo' from the server, Trailers-Only case
  parseResponseNoMessages ::
       sess
    -> ResponseInfo -> IO (NoMessages (Inbound sess))

-- | Accept session
--
-- A server node listens and accepts incoming requests from client nodes.
class IsSession sess => AcceptSession sess where
  -- | Parse 'RequestInfo' from the client, regular case
  --
  -- See 'parseRequestTrailersOnly' for the Trailers-Only case.
  parseRequestRegular ::
       sess
    -> RequestInfo -> IO (Headers (Inbound sess))

  --  | Parse 'RequestInfo' from the client, Trailers-Only case
  parseRequestNoMessages ::
       sess
    -> RequestInfo -> IO (NoMessages (Inbound sess))

  -- | Build 'ResponseInfo' for the client
  buildResponseInfo ::
       sess
    -> FlowStart (Outbound sess)
    -> ResponseInfo

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
      channelInbound :: TVar (ThreadState (TMVar (FlowState (Inbound sess))))

      -- | Thread state of the thread sending messages to the peer
    , channelOutbound :: TVar (ThreadState (TMVar (FlowState (Outbound sess))))

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

-- | Data flow state
data FlowState flow =
    FlowStateRegular (RegularFlowState flow)
  | FlowStateNoMessages (NoMessages flow)

-- | Regular (streaming) flow state
data RegularFlowState flow = RegularFlowState {
      -- | Headers
      --
      -- On the client side, the outbound headers are specified when the request
      -- is made ('callRequestMetadata'), and the inbound headers are recorded
      -- once the responds starts to come in; clients can block-and-wait for
      -- these headers ('getInboundHeaders').
      --
      -- On the server side, the inbound headers are recorded when the request
      -- comes in, and the outbound headers are specified
      -- ('setResponseMetadata') before the response is initiated
      -- ('initiateResponse'/'sendTrailersOnly').
      flowHeaders :: Headers flow

      -- | Messages
      --
      -- This TMVar is written to for incoming messages ('recvMessageLoop') and
      -- read from for outgoing messages ('sendMessageLoop'). It acts as a
      -- one-place buffer, providing backpressure in both directions.
      --
      -- TODO: It might make sense to generalize this to an @N@-place buffer,
      -- for configurable @N@. This might result in better latency masking.
    , flowMsg :: TMVar (StreamElem (Trailers flow) (Message flow))

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
    , flowTerminated :: TMVar (Trailers flow)
    }

-- | 'Show' instance is useful in combination with @stm-debug@ only
deriving instance (
    Show (Headers flow)
  , Show (TMVar (StreamElem (Trailers flow) (Message flow)))
  , Show (TMVar (Trailers flow))
  ) => Show (RegularFlowState flow)

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

initFlowStateRegular :: Headers flow -> IO (RegularFlowState flow)
initFlowStateRegular headers = do
    RegularFlowState headers
      <$> newEmptyTMVarIO
      <*> newEmptyTMVarIO

{-------------------------------------------------------------------------------
  Status check
-------------------------------------------------------------------------------}

data ChannelStatus sess = ChannelStatus {
      channelStatusInbound   :: FlowStatus (Inbound sess)
    , channelStatusOutbound  :: FlowStatus (Outbound sess)
    , channelStatusSentFinal :: Bool
    , channelStatusRecvFinal :: Bool
    }

data FlowStatus flow =
    FlowNotYetEstablished
  | FlowEstablished (FlowState flow)
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
    go :: ThreadState (TMVar (FlowState flow)) -> STM (FlowStatus flow)
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
  -> StreamElem (Trailers (Outbound sess)) (Message (Outbound sess))
  -> STM ()
sessionSend Channel{channelOutbound, channelSentFinal} msg = do
    -- By checking that we haven't sent the final message yet, we know that this
    -- call to 'putMVar' will not block indefinitely: the thread that sends
    -- messages to the peer will get to it eventually (unless it dies, in which
    -- case the thread status will change and the call to 'getThreadInterface'
    -- will be retried).
    sentFinal <- readTVar channelSentFinal
    case sentFinal of
      Just cs -> throwSTM $ SendAfterFinal cs
      Nothing -> do
        st <- readTMVar =<< getThreadInterface channelOutbound
        case st of
          FlowStateRegular regular -> do
            StreamElem.whenDefinitelyFinal msg $ \_trailers ->
              writeTVar channelSentFinal $ Just callStack
            putTMVar (flowMsg regular) msg
          FlowStateNoMessages _ ->
            -- For outgoing messages, the caller decides to use Trailers-Only,
            -- so if they then subsequently call 'send', we throw an exception.
            -- This is different for /inbound/ messages; see 'recv', below.
            throwSTM $ SendButTrailersOnly

-- | Receive a message from the node's peer
--
-- It is a bug to call 'recv' again after receiving the final message; Doing so
-- will result in a 'RecvAfterFinal' exception.
sessionRecv ::
     HasCallStack
  => Channel sess
  -> STM ( StreamElem
             (Either (NoMessages (Inbound sess)) (Trailers (Inbound sess)))
             (Message (Inbound sess))
         )
sessionRecv Channel{channelInbound, channelRecvFinal} = do
    -- By checking that we haven't received the final message yet, we know that
    -- this call to 'takeTMVar' will not block indefinitely: the thread that
    -- receives messages from the peer will get to it eventually (unless it
    -- dies, in which case the thread status will change and the call to
    -- 'getThreadInterface' will be retried).
    traceM $ "recv 1 " ++ " at " ++ prettyCallStack callStack
--    readFinal <- readTVar channelRecvFinal
--    traceM $ "recv 2 " ++ show readFinal
--    case readFinal of
--      Just cs -> throwSTM $ RecvAfterFinal cs
--      Nothing -> do
    do
        -- We get the TMVar in the same transaction as reading from it (below).
        -- This means that /if/ the thread running 'recvMessageLoop' throws an
        -- exception and is killed, the 'takeTMVar' below cannot block
        -- indefinitely.
        traceM $ "recv 3a"
        iface <- getThreadInterface channelInbound
        traceM $ "recv 3b"
        st  <- readTMVar iface
        case st of
          FlowStateRegular regular -> do
            traceM $ "recv 4.1"
            msg <- takeTMVar (flowMsg regular)
            traceM "recv 4.2"
             -- We update 'channelRecvFinal' in the same tx as the read, to
            -- atomically change from "there is a value" to "all values read".
            StreamElem.whenDefinitelyFinal msg $ \_trailers ->
              writeTVar channelRecvFinal $ Just callStack
            traceM "recv 4.3"
            return $ first Right msg
          FlowStateNoMessages trailers -> do
            traceM "recv 5.1"
            writeTVar channelRecvFinal $ Just callStack
            traceM "recv 5.2"
            return $ NoMoreElems (Left trailers)

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
sessionWaitForOutbound :: HasCallStack => Channel sess -> IO (FlowState (Outbound sess))
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
     IsSession sess
  => sess
  -> (forall x. IO x -> IO x) -- ^ Unmask
  -> Tracer IO (DebugMsg sess)
  -> RegularFlowState (Outbound sess)
  -> OutputStream
  -> IO ()
sendMessageLoop sess unmask tracer st stream =
    go $ buildMsg sess (flowHeaders st)
  where
    go :: (Message (Outbound sess) -> Builder) -> IO ()
    go build = do
        trailers <- loop
        atomically $ putTMVar (flowTerminated st) trailers
      where
        loop :: IO (Trailers (Outbound sess))
        loop = do
            traceWith tracer $ NodeSendAwaitMsg
            -- Technically the call to unmask is necessary here, as takeTMVar
            -- is interuptible.
            msg <- unmask $ atomically $ takeTMVar (flowMsg st)
            traceWith tracer $ NodeSendMsg msg

            case msg of
              StreamElem x -> do
                writeChunk stream $ build x
                flush stream
                loop
              FinalElem x trailers -> do
                -- We don't flush the last message, so that http2 can mark the
                -- stream as END_STREAM (rather than having to send a separate
                -- empty data frame).
                writeChunk stream $ build x
                return trailers
              NoMoreElems trailers -> do
                return trailers

-- | Receive all messages sent by the node's peer
--
-- Should be called with exceptions masked.
--
-- TODO: This is wrong, we are never marking the final element as final.
-- (But fixing this requires a patch to http2.)
recvMessageLoop :: forall sess.
     IsSession sess
  => sess
  -> (forall x. IO x -> IO x) -- ^ Unmask
  -> Tracer IO (DebugMsg sess)
  -> RegularFlowState (Inbound sess)
  -> InputStream
  -> IO ()
recvMessageLoop sess unmask tracer st stream = do
    go $ parseMsg sess (flowHeaders st)
  where
    go :: Parser (Message (Inbound sess)) -> IO ()
    go = \parser -> do
        trailers <- loop parser >>= parseInboundTrailers sess
        traceWith tracer $ NodeRecvFinal trailers
        atomically $ putTMVar (flowTerminated st) $ trailers
        atomically $ putTMVar (flowMsg        st) $ NoMoreElems trailers
      where
        loop :: Parser (Message (Inbound sess)) -> IO [HTTP.Header]
        loop (ParserError err) = do
            throwIO $ PeerSentMalformedMessage err
        loop (ParserDone x p') = do
            traceWith tracer $ NodeRecvMsg (StreamElem x)
            atomically $ putTMVar (flowMsg st) $ StreamElem x
            loop p'
        loop (ParserNeedsData acc p') = do
            traceWith tracer $ NodeNeedsData acc
            mbs <- unmask $ try $ getChunk stream
            bs <- case mbs of
                    Left (err :: SomeException) -> do
                      throwIO err
                    Right bs ->
                      return bs


            if | not (BS.Strict.null bs) -> do
                   loop $ p' bs

               | not (BS.Lazy.null acc) -> do
                   throwIO PeerSentIncompleteMessage

               | otherwise -> do
                   getTrailers stream

outboundTrailersMaker :: forall sess.
     IsSession sess
  => sess
  -> Channel sess
  -> HTTP2.TrailersMaker
outboundTrailersMaker sess channel = go
  where
    go :: HTTP2.TrailersMaker
    go (Just _) = return $ HTTP2.NextTrailersMaker go
    go Nothing  = do
        -- Wait for the thread to terminate
        --
        -- If the thread was killed, this will throw an exception (which will
        -- then result in @http2@ cancelling the corresponding stream).
        flowState <- sessionWaitForOutbound channel
        trailers  <- case flowState of
                       FlowStateRegular regular ->
                         atomically $ readTMVar $ flowTerminated regular
                       FlowStateNoMessages _ ->
                         error "unexpected FlowStateNoMessages"
        return $ HTTP2.Trailers $ buildOutboundTrailers sess trailers

data DebugMsg sess =
    -- | Thread sending messages is awaiting a message
    NodeSendAwaitMsg

    -- | Thread sending message will send a message
  | NodeSendMsg (
        StreamElem
          (Trailers (Outbound sess))
          (Message  (Outbound sess))
      )

    -- | Receive thread requires data
    --
    -- We also record the data already received
  | NodeNeedsData BS.Lazy.ByteString

    -- | Receive thread received a message
  | NodeRecvMsg (
        StreamElem
          (Either (NoMessages (Inbound sess)) (Trailers (Inbound sess)))
          (Message (Inbound sess))
      )

    -- | Receive thread received the trailers
  | NodeRecvFinal (Trailers (Inbound sess))

deriving instance IsSession sess => Show (DebugMsg sess)

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
  -> Tracer IO (DebugMsg sess)
  -> ConnectionToClient
  -> (FlowStart (Inbound sess) -> IO (FlowStart (Outbound sess)))
  -- ^ Construct headers for the initial response
  --
  -- This function has quite a bit of control over how the outbound part of
  -- the channel gets established:
  --
  -- * If it blocks, the response will not be initiated until it returns.
  -- * If it throws an exception, no response will be attempted at all. This
  --   allows the function to send a response of its own (typically, some kind
  --   of error response).
  -> IO (Channel sess)
setupResponseChannel sess tracer conn startOutbound = do
    channel <- initChannel

    let requestHeaders = fromHeaderTable $ Server.requestHeaders (request conn)
    requestMethod <- case Server.requestMethod (request conn) of
                       Just x  -> return x
                       Nothing -> throwIO PeerMissingPseudoHeaderMethod
    requestPath   <- case Server.requestPath (request conn) of
                       Just x  -> return x
                       Nothing -> throwIO PeerMissingPseudoHeaderPath
    let requestInfo = RequestInfo {requestHeaders, requestMethod, requestPath}

    inboundStart <-
      if Server.requestBodySize (request conn) == Just 0 then
        FlowStartNoMessages <$> parseRequestNoMessages sess requestInfo
      else
        FlowStartRegular <$> parseRequestRegular sess requestInfo

    forkThread (channelInbound channel) newEmptyTMVarIO $ \unmask stVar -> do
      case inboundStart of
        FlowStartRegular headers -> do
          regular <- initFlowStateRegular headers
          stream  <- serverInputStream (request conn)
          atomically $ putTMVar stVar $ FlowStateRegular regular
          recvMessageLoop sess unmask tracer regular stream
        FlowStartNoMessages trailers -> do
          atomically $ putTMVar stVar $ FlowStateNoMessages trailers

    forkThread (channelOutbound channel) newEmptyTMVarIO $ \unmask stVar -> do
      outboundStart <- startOutbound inboundStart
      let responseInfo = buildResponseInfo sess outboundStart
      case outboundStart of
        FlowStartRegular headers -> do
          regular <- initFlowStateRegular headers
          atomically $ putTMVar stVar $ FlowStateRegular regular
          let resp :: Server.Response
              resp = setResponseTrailers sess channel
                   $ Server.responseStreaming
                           (responseStatus  responseInfo)
                           (responseHeaders responseInfo)
                   $ \write' flush' -> do
                        stream <- serverOutputStream write' flush'
                        sendMessageLoop sess unmask tracer regular stream
          respond conn resp
        FlowStartNoMessages trailers -> do
          atomically $ putTMVar stVar $ FlowStateNoMessages trailers
          let resp :: Server.Response
              resp = Server.responseNoBody
                       (responseStatus  responseInfo)
                       (responseHeaders responseInfo)
          respond conn $ resp

    return channel

{-------------------------------------------------------------------------------
  Auxiliary http2
-------------------------------------------------------------------------------}

setResponseTrailers ::
     IsSession sess
  => sess
  -> Channel sess
  -> Server.Response -> Server.Response
setResponseTrailers sess channel resp =
    Server.setResponseTrailersMaker resp $
      outboundTrailersMaker sess channel

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
  -> Tracer IO (DebugMsg sess)
  -> ConnectionToServer
  -> FlowStart (Outbound sess)
  -> IO (Channel sess)
setupRequestChannel sess tracer ConnectionToServer{sendRequest} outboundStart = do
    channel <- initChannel
    let requestInfo = buildRequestInfo sess outboundStart

    case outboundStart of
      FlowStartRegular headers -> do
        regular <- initFlowStateRegular headers
        let req :: Client.Request
            req = setRequestTrailers sess channel
                $ Client.requestStreamingUnmask
                    (requestMethod  requestInfo)
                    (requestPath    requestInfo)
                    (requestHeaders requestInfo)
                $ \unmask write' flush' -> unmask $ do
                     threadBody (channelOutbound channel)
                                (newTMVarIO (FlowStateRegular regular))
                              $ \_stVar -> do
                       stream <- clientOutputStream write' flush'
                       sendMessageLoop sess unmask tracer regular stream
        forkRequest channel req
      FlowStartNoMessages trailers -> do
        stVar <- newTMVarIO $ FlowStateNoMessages trailers
        atomically $ writeTVar (channelOutbound channel) $ ThreadDone stVar
        let req :: Client.Request
            req = Client.requestNoBody
                    (requestMethod  requestInfo)
                    (requestPath    requestInfo)
                    (requestHeaders requestInfo)
        forkRequest channel req

    return channel
  where
    forkRequest :: Channel sess -> Client.Request -> IO ()
    forkRequest channel req =
        forkThread (channelInbound channel) newEmptyTMVarIO $ \unmask stVar -> do
          setup <- try $ sendRequest req $ \resp -> do
            responseStatus <- case Client.responseStatus resp of
                                Just x  -> return x
                                Nothing -> throwIO PeerMissingPseudoHeaderStatus
            let responseHeaders = fromHeaderTable $ Client.responseHeaders resp
                responseInfo    = ResponseInfo {responseHeaders, responseStatus}

            inboundStart <-
              if Client.responseBodySize resp == Just 0
                then FlowStartNoMessages <$>
                       parseResponseNoMessages sess responseInfo
                else FlowStartRegular <$>
                       parseResponseRegular sess responseInfo

            case inboundStart of
              FlowStartRegular headers -> do
                regular <- initFlowStateRegular headers
                stream  <- clientInputStream resp
                atomically $ putTMVar stVar $ FlowStateRegular regular
                recvMessageLoop sess unmask tracer regular stream
              FlowStartNoMessages trailers -> do
                atomically $ putTMVar stVar $ FlowStateNoMessages trailers

          case setup of
            Right () -> return ()
            Left  e  -> do
              void $ cancelThread (channelOutbound channel) e
              throwIO e

{-------------------------------------------------------------------------------
   Auxiliary http2
-------------------------------------------------------------------------------}

setRequestTrailers ::
     IsSession sess
  => sess
  -> Channel sess
  -> Client.Request -> Client.Request
setRequestTrailers sess channel req =
    Client.setRequestTrailersMaker req $
      outboundTrailersMaker sess channel

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
        clientSendInput call $ NoMoreElems ()
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

    _client <- async $ clientGlobal testClock

    -- Wait for the test clock to reach 4 (which it won't)
    -- (This just prevents the test from terminating too soon)
    waitForTestClockTick testClock 4
