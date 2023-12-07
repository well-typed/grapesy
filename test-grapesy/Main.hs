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
import Data.ByteString.Char8 qualified as BS.C8
import Data.Default
import Data.Proxy
import Data.Text qualified as Text
import Data.Void
import Debug.Trace
import GHC.Stack
import GHC.TypeLits
import Network.HTTP2.Client qualified as HTTP2.Client
import Network.HTTP2.Server qualified as HTTP2
import Network.Run.TCP (runTCPServer)
import Network.Run.TCP qualified as Run
import Network.Socket
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Internal qualified as HTTP2

import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server.Connection (Connection(..), withConnection)
import Network.GRPC.Server.Connection qualified as Connection
import Network.GRPC.Server.Context (ServerParams)
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Server.Session
import Network.GRPC.Spec
import Network.GRPC.Util.Concurrency
import Network.GRPC.Util.HTTP2.Stream (ClientDisconnected(..))
import Network.GRPC.Util.Parser
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Util.Session.Server qualified as Server

-- ========================================================================== --

_traceLabelled :: Show a => String -> a -> a
_traceLabelled lbl x = trace (lbl ++ ": " ++ show x) x

-- ========================================================================== --

clientWithRPC ::
     (HasCallStack, KnownSymbol meth)
  => ClientConnection
  -> Proxy (TrivialRpc meth)
  -> (ClientCall meth -> IO ())
  -> IO ()
clientWithRPC ClientConnection{clientConnState} _proxy k =
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
          TrivialClient
          nullTracer
          conn
          flowStart

      mb :: Either SomeException () <- unmask $ try $ k (ClientCall channel)
      _ <- Session.close channel (ExitCaseSuccess ()) -- simplification
      either throwIO return mb
  where
    flowStart :: Session.FlowStart (ClientOutbound rpc)
    flowStart = Session.FlowStartRegular ClientOutboundHeaders

clientSendInput :: HasCallStack => ClientCall meth -> StreamElem () Void -> IO ()
clientSendInput call msg = do
    atomically $ Session.send (clientCallChannel call) msg
    StreamElem.whenDefinitelyFinal msg $ \_ ->
      void $ Session.waitForOutbound (clientCallChannel call)

clientRecvOutput :: ClientCall meth -> IO (StreamElem () Void)
clientRecvOutput call = atomically $
    first (const ()) <$> Session.recv (clientCallChannel call)

-- ========================================================================== --

data ClientConnection = ClientConnection {
      clientConnState :: TVar ConnectionState
    }

data ClientCall meth =  ClientCall {
      clientCallChannel :: Session.Channel (TrivialClient meth)
    }

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

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data TrivialClient (meth :: Symbol) = TrivialClient

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

data ClientInbound  (meth :: Symbol)
data ClientOutbound (meth :: Symbol)

instance Session.DataFlow (ClientInbound meth) where
  data Headers    (ClientInbound meth) = ClientInboundHeaders deriving (Show)
  type Message    (ClientInbound meth) = Void
  type Trailers   (ClientInbound meth) = ()
  type NoMessages (ClientInbound meth) = ()

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
  parseMsg              _ _ = ParserError "Unexpected message of type Void"
  buildMsg              _ _ = absurd

instance KnownSymbol meth => Session.InitiateSession (TrivialClient meth) where
  parseResponseRegular    _ _ = return ClientInboundHeaders
  parseResponseNoMessages _ _ = return ()

  buildRequestInfo _ _ = Session.RequestInfo {
        requestMethod  = "POST"
      , requestPath    = "/trivial/" <> BS.C8.pack (symbolVal (Proxy @meth))
      , requestHeaders = []
      }

-- ========================================================================== --

data ServerConfig = ServerConfig ServiceName

runServer :: ServerConfig -> HTTP2.Server -> IO ()
runServer (ServerConfig port) server =
    runTCPServer Nothing port $ \sock -> do
      bracket (HTTP2.allocSimpleConfig sock 4096)
              HTTP2.freeSimpleConfig $ \config ->
        HTTP2.run HTTP2.defaultServerConfig config server

-- ========================================================================== --

data RpcHandler = forall rpc. IsRPC rpc => RpcHandler {
      -- | Handler proper
      runRpcHandler :: ServerCall rpc -> IO ()
    }

mkRpcHandler :: IsRPC rpc => Proxy rpc -> (ServerCall rpc -> IO ()) -> RpcHandler
mkRpcHandler _ = RpcHandler

type HandlerMap = [(Path, RpcHandler)]

constructHandlerMap :: [RpcHandler] -> HandlerMap
constructHandlerMap = map (\h -> (handlerPath h, h))

handlerPath :: RpcHandler -> Path
handlerPath RpcHandler{runRpcHandler} = aux runRpcHandler
  where
    aux :: forall rpc. IsRPC rpc => (ServerCall rpc -> IO ()) -> Path
    aux _ = rpcPath (Proxy @rpc)

-- ========================================================================== --

-- | Construct server
--
-- The server can be run using the standard infrastructure offered by the
-- @http2@ package, but "Network.GRPC.Server.Run" provides some convenience
-- functions.
withServer :: ServerParams -> [RpcHandler] -> (HTTP2.Server -> IO a) -> IO a
withServer params handlers k =
    Context.withContext params $
      k . server (constructHandlerMap handlers)
  where
    server :: HandlerMap -> Context.ServerContext -> HTTP2.Server
    server handlerMap ctxt =
        withConnection ctxt $
          handleRequest params handlerMap

handleRequest ::
     HasCallStack
  => ServerParams
  -> HandlerMap
  -> Connection -> IO ()
handleRequest params handlers conn = do
    -- TODO: Proper "Apache style" logging (in addition to the debug logging)
    traceWith tracer $ Context.ServerDebugRequest path
    let handler = case lookup path handlers of
                    Just h  -> h
                    Nothing -> error "withHandler: unknown path"
    case handler of
      RpcHandler h -> serverAcceptCall params conn h
  where
    path :: Path
    path = Connection.path conn

    tracer :: Tracer IO Context.ServerDebugMsg
    tracer =
          Context.serverDebugTracer
        $ Context.params
        $ connectionContext conn

-- ========================================================================== --

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ServerCall rpc = IsRPC rpc => ServerCall {
      -- | Server state (across all calls)
      callSession :: ServerSession rpc

      -- | Bidirectional channel to the client
    , callChannel :: Session.Channel (ServerSession rpc)

      -- | Request metadata
      --
      -- This is filled once we get the request headers
    , callRequestMetadata :: TMVar [CustomMetadata]

      -- | Response metadata
      --
      -- Metadata to be included when we send the initial response headers.
      -- Can be updated until the first message (see 'callFirstMessage').
      --
      -- Defaults to the empty list.
    , callResponseMetadata :: TVar [CustomMetadata]

      -- | What kicked off the response?
      --
      -- This is empty until the response /has/ in fact been kicked off.
    , callResponseKickoff :: TMVar Kickoff
    }

-- | What kicked off the response?
--
-- When the server handler starts, we do not immediately initate the response
-- to the client, because the server might not have decided the initial response
-- metadata yet (indeed, it might need wait for some incoming messages from the
-- client before it can compute that metadata). We therefore wait until we
-- send the initial response headers, until the handler..
--
-- 1. explicitly calls 'initiateResponse'
-- 2. sends the first message using 'sendOutput'
-- 3. initiates and immediately terminates the response using 'sendTrailersOnly'
--
-- We only need distinguish between (1 or 2) versus (3), corresponding precisely
-- to the two constructors of 'FlowStart'.
data Kickoff =
    KickoffRegular
  | KickoffTrailersOnly TrailersOnly
  deriving (Show)

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

-- | Accept incoming call
--
-- If an exception is thrown during call setup, we will send an error response
-- to the client (and then rethrow the exception).
serverAcceptCall :: forall rpc.
     (IsRPC rpc, HasCallStack)
  => Context.ServerParams
  -> Connection
  -> (ServerCall rpc -> IO ())
  -> IO ()
serverAcceptCall params conn k = do
    callRequestMetadata  <- newEmptyTMVarIO
    callResponseMetadata <- newTVarIO []
    callResponseKickoff  <- newEmptyTMVarIO

    let setupResponseChannel :: IO (Session.Channel (ServerSession rpc))
        setupResponseChannel =
            Session.setupResponseChannel
              callSession
              (contramap (Context.ServerDebugMsg @rpc) tracer)
              (Connection.connectionClient conn)
              (   handle sendErrorResponse
                . mkOutboundHeaders
                    callRequestMetadata
                    callResponseMetadata
                    callResponseKickoff
              )

        handlerTeardown ::
             ServerCall rpc
          -> Either SomeException ()
          -> IO ()
        handlerTeardown call@ServerCall{callChannel} mRes = do
            mUnclean <-
              case mRes of
                Right () ->
                  -- Handler terminated successfully
                  -- (but it might not have sent the final message, see below)
                  Session.close callChannel $ ExitCaseSuccess ()
                Left err -> do
                  -- The handler threw an exception. /Try/ to tell the client
                  -- (but see discussion of 'forwardException')
                  traceWith (Context.serverExceptionTracer params) err
                  forwardException call err
                  Session.close callChannel $ ExitCaseException err

            case mUnclean of
              Nothing  -> return () -- Channel was closed cleanly
              Just err ->
                -- An unclean shutdown can have 3 causes:
                --
                -- 1. We failed to set up the call.
                --
                --    If the failure is due to network comms, we definitely have
                --    no way of communicating with the client. If the failure
                --    was due to a setup failure in 'mkOutboundHeaders', then
                --    the client has /already/ been notified of the problem.
                --
                -- 2. We lost communication during the call.
                --
                --    In this case we also have no way of telling the client
                --    that something went wrong.
                --
                -- 3. The handler failed to properly terminate the communication
                --    (send the final message and call 'waitForOutbound').
                --
                --    This is the trickiest case. We don't really know what
                --    state the handler left the channel in; for example, we
                --    might have killed the thread halfway through sending a
                --    message.
                --
                -- So the only thing we really can do here is log the error,
                -- and nothing else. (We should not rethrow it, as doing so will
                -- cause http2 to reset the stream, which is not always the
                -- right thing to do (for example, in case (1)).
                traceWith (Context.serverExceptionTracer params) $
                  toException err

    let handler ::
             Session.Channel (ServerSession rpc)
          -> (forall a. IO a -> IO a)
          -> IO ()
        handler callChannel unmask = do
            mRes <- try $ unmask $ k call
            handlerTeardown call mRes
          where
            call :: ServerCall rpc
            call = ServerCall{
                callSession
              , callChannel
              , callRequestMetadata
              , callResponseMetadata
              , callResponseKickoff
              }

    runHandler setupResponseChannel handler
  where
    callSession :: ServerSession rpc
    callSession = ServerSession

    tracer :: Tracer IO Context.ServerDebugMsg
    tracer = Context.serverDebugTracer $ Context.params $ connectionContext conn

    mkOutboundHeaders ::
         TMVar [CustomMetadata]
      -> TVar [CustomMetadata]
      -> TMVar Kickoff
      ->     Session.FlowStart (ServerInbound  rpc)
      -> IO (Session.FlowStart (ServerOutbound rpc))
    mkOutboundHeaders requestMetadataVar
                      responseMetadataVar
                      responseKickoffVar
                      requestStart
                    = do
        traceIO "mkOutboundHeaders 1"
        -- Make request metadata available (see 'getRequestMetadata')
        atomically $ putTMVar requestMetadataVar $
                       requestMetadata inboundHeaders
        traceIO "mkOutboundHeaders 2"

        -- Wait for kickoff (see 'Kickoff' for discussion)
        kickoff <- atomically $ readTMVar responseKickoffVar
        traceIO "mkOutboundHeaders 3"

        -- Get response metadata (see 'setResponseMetadata')
        responseMetadata <- atomically $ readTVar responseMetadataVar
        traceIO "mkOutboundHeaders 4"

        -- Session start
        case kickoff of
          KickoffRegular -> do
            traceIO $ "mkOutboundHeaders 5"
            return $ Session.FlowStartRegular $ OutboundHeaders {
                outHeaders = ResponseHeaders {
                    responseMetadata = responseMetadata
                  }
              }
          KickoffTrailersOnly trailers -> do
            traceIO $ "mkOutboundHeaders 6"
            return $ Session.FlowStartNoMessages trailers
      where
        inboundHeaders :: RequestHeaders
        inboundHeaders =
            case requestStart of
              Session.FlowStartRegular    headers  -> inbHeaders headers
              Session.FlowStartNoMessages trailers ->            trailers

    -- | Send response when 'mkOutboundHeaders' fails
    --
    -- TODO: Some duplication with 'forwardException' (and 'withHandler').
    sendErrorResponse :: forall x. SomeException -> IO x
    sendErrorResponse err = do
        traceWith tracer $ Context.ServerDebugAcceptFailed err
        Server.respond (connectionClient conn) $
          HTTP2.responseNoBody
            HTTP.ok200 -- gRPC uses HTTP 200 even when there are gRPC errors
            (buildTrailersOnly $ TrailersOnly $ ProperTrailers {
                trailerGrpcStatus  = GrpcError GrpcUnknown
                -- TODO: Potential security concern here
                -- (showing the exception)?
              , trailerGrpcMessage = Just $ Text.pack (show err)
              , trailerMetadata    = []
              })
        throwIO err

-- | Run the handler
--
-- http2 will kill the handler when the client disappears, but we don't want
-- that behaviour: we want the handler to be able to terminate cleanly. We
-- therefore run the handler in a separate thread, and wait for that thread to
-- terminate. If we are interrupted while we wait, it depends on the
-- interruption:
--
-- * If we are interrupted by 'HTTP2.KilledByHttp2ThreadManager', it means
--   we got disconnected from the client. In this case, we shut down the channel
--   (if it's not already shut down); /if/ the handler at this tries to
--   communicate with the client, an exception will be raised. However, the
--   handler /can/ terminate cleanly and, of course, typically will.
--   Importantly, this avoids race conditions: even if the server and the client
--   agree that no further communication takes place (the client sent their
--   final message, the server sent the trailers), without the indireciton of
--   this additional thread it can still happen that http2 kills the handler
--   (after the client disconnects) before the handler has a chance to
--   terminate.
-- * If we are interrupted by another kind of asynchronous exception, we /do/
--   kill the handler (this might for example be a timeout).
--
-- This is in line with the overall design philosophy of communication in this
-- library: exceptions will only be raised synchronously when communication is
-- attempted, not asynchronously when we notice a problem.
runHandler ::
     IO (Session.Channel (ServerSession rpc))
  -> (Session.Channel (ServerSession rpc) -> (forall a. IO a -> IO a) -> IO ())
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

-- | Process exception thrown by a handler
--
-- Trace the exception and forward it to the client.
--
-- The attempt to forward it to the client is a best-effort only:
--
-- * The nature of the exception might mean that we we cannot send anything to
--   the client at all.
-- * It is possible the exception was thrown /after/ the handler already send
--   the trailers to the client.
--
-- We therefore catch and suppress all exceptions here.
forwardException :: ServerCall rpc -> SomeException -> IO ()
forwardException call err = do
    handle ignoreExceptions $
      sendProperTrailers call trailers
  where
    trailers :: ProperTrailers
    trailers
      | Just (err' :: GrpcException) <- fromException err
      = grpcExceptionToTrailers err'

      -- TODO: There might be a security concern here (server-side exceptions
      -- could potentially leak some sensitive data).
      | otherwise
      = ProperTrailers {
            trailerGrpcStatus  = GrpcError GrpcUnknown
          , trailerGrpcMessage = Just $ Text.pack (show err)
          , trailerMetadata    = []
          }

    -- See discussion above.
    ignoreExceptions :: SomeException -> IO ()
    ignoreExceptions _ = return ()

{-------------------------------------------------------------------------------
  Open (ongoing) call
-------------------------------------------------------------------------------}

-- | Receive RPC input from the client
--
-- We do not return trailers, since gRPC does not support sending trailers from
-- the client to the server (only from the server to the client).
serverRecvInput :: forall rpc. HasCallStack => ServerCall rpc -> IO (StreamElem NoMetadata (Input rpc))
serverRecvInput call =
    catch go $ \e@BlockedIndefinitelyOnSTM{} -> do
      traceIO $ "Uhoh.. got the dreaded " ++ show e
      throwIO e
      --threadDelay 1_000_000
      --traceIO $ "Trying again!"
      --go
  where
    go :: IO (StreamElem NoMetadata (Input rpc))
    go = atomically $ recvInputSTM call

-- | Send RPC output to the client
--
-- This will send a @grpc-status@ of @0@ to the client; for anything else (i.e.,
-- to indicate something went wrong), the server handler should throw a
-- 'GrpcException' (the @grapesy@ client API treats this the same way: a
-- @grpc-status@ other than @0@ will be raised as a 'GrpcException').
--
-- This is a blocking call if this is the final message (i.e., the call will
-- not return until the message has been written to the HTTP2 stream).
serverSendOutput :: ServerCall rpc -> StreamElem [CustomMetadata] (Output rpc) -> IO ()
serverSendOutput call msg = do
    _updated <- initiateResponse call
    atomically $ sendOutputSTM call msg
    StreamElem.whenDefinitelyFinal msg $ \_ -> waitForOutbound call

{-------------------------------------------------------------------------------
  Low-level API

  'initiateResponse' and 'sendOutputSTM' /MUST NOT/ live in the same
  transaction: in order to send a message, we need to know if the call is in
  "regular" flow state or "trailers only" flow state, which we only know after
  'initiateResponse'; so if these live in the same transaction, we will
  deadlock.
-------------------------------------------------------------------------------}

-- | STM version of 'recvInput'
--
-- This is a low-level API; most users can use 'recvInput' instead.
--
-- Most server handlers will deal with single clients, but in principle
-- 'recvInputSTM' could be used to wait for the first message from any number of
-- clients.
recvInputSTM :: forall rpc.
     HasCallStack
  => ServerCall rpc
  -> STM (StreamElem NoMetadata (Input rpc))
recvInputSTM ServerCall{callChannel} =
    first ignoreTrailersOnly <$> Session.recv callChannel
  where
    ignoreTrailersOnly ::
         Either RequestHeaders NoMetadata
      -> NoMetadata
    ignoreTrailersOnly _ = NoMetadata

-- | STM version of 'sendOutput'
--
-- This is a low-level API; most users can use 'sendOutput' instead.
--
-- If you choose to use 'sendOutputSTM' instead, you have two responsibilities:
--
-- * You must call 'initiateResponse' before calling 'sendOutputSTM';
--   'sendOutputSTM' will throw 'ResponseNotInitiated' otherwise.
-- * You must call 'waitForOutbound' after sending the final message (and before
--   exiting the scope of 'acceptCall'); if you don't, you risk that the HTTP2
--   stream is cancelled.
--
-- Implementation note: we cannot call 'waitForOutbound' /in/ 'acceptCall': if
-- the handler for whatever reason never writes the final message, then such a
-- call would block indefinitely.
sendOutputSTM :: ServerCall rpc -> StreamElem [CustomMetadata] (Output rpc) -> STM ()
sendOutputSTM ServerCall{callChannel, callResponseKickoff} msg = do
    mKickoff <- tryReadTMVar callResponseKickoff
    case mKickoff of
      Just _  -> Session.send callChannel (first mkTrailers msg)
      Nothing -> throwSTM $ ResponseNotInitiated
  where
    mkTrailers :: [CustomMetadata] -> ProperTrailers
    mkTrailers metadata = ProperTrailers {
          trailerGrpcStatus  = GrpcOk
        , trailerGrpcMessage = Nothing
        , trailerMetadata    = metadata
        }

-- | Initiate the response
--
-- See 'sendOutputSTM' for discusison.
--
-- Returns 'False' if the response was already initiated.
initiateResponse :: ServerCall rpc -> IO Bool
initiateResponse ServerCall{callResponseKickoff} =
    atomically $ tryPutTMVar callResponseKickoff KickoffRegular

-- | Wait for all outbound messages to have been processed
--
-- This function /must/ be called before leaving the scope of 'acceptCall'.
-- However, 'sendOutput' will call 'waitForOutbound' when the last message is
-- sent, so you only need to worry about this if you are using 'sendOutputSTM'.
waitForOutbound :: ServerCall rpc -> IO ()
waitForOutbound = void . Session.waitForOutbound . callChannel

data ResponseKickoffException =
    ResponseAlreadyInitiated
  | ResponseNotInitiated
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Check if the connection to the client is healthy
--
-- See 'Network.GRPC.Client.isCallHealthy' for considerations regarding the
-- non-deterministic result of this function.
serverIsCallHealthy :: ServerCall rpc -> STM Bool
serverIsCallHealthy = Session.isChannelHealthy . callChannel

{-------------------------------------------------------------------------------
  Internal API
-------------------------------------------------------------------------------}

-- | Send 'ProperTrailers'
--
-- This function is not part of the public API: we use it the top-level
-- exception handler in "Network.GRPC.Server" to forward exceptions in server
-- handlers to the client.
--
-- If no messages have been sent yet, we make use of the @Trailers-Only@ case.
sendProperTrailers :: ServerCall rpc -> ProperTrailers -> IO ()
sendProperTrailers ServerCall{callResponseKickoff, callChannel} trailers = do
    updated <- atomically $ tryPutTMVar callResponseKickoff $
                 KickoffTrailersOnly (TrailersOnly trailers)
    unless updated $
      -- If we didn't update, then the response has already been initiated and
      -- we cannot make use of the Trailers-Only case.
      atomically $ Session.send callChannel (NoMoreElems trailers)


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

  Once we have inlined and simplified the server stuff also, this can go.
-------------------------------------------------------------------------------}

-- RPC calls that exchange no data
data TrivialRpc (meth :: Symbol)

instance KnownSymbol meth => IsRPC (TrivialRpc meth) where
  type Input  (TrivialRpc meth) = Void
  type Output (TrivialRpc meth) = Void

  serializationFormat _ = "trivial"
  serviceName         _ = "trivial"
  methodName          _ = Text.pack $ symbolVal (Proxy @meth)
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
      clientWithRPC conn (Proxy @TestRpc1) $ \_call -> do
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
      clientWithRPC conn (Proxy @TestRpc2) $ \call -> do
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

{-------------------------------------------------------------------------------
  Server-side interpretation

  The server-side is slightly different, since the infrastructure spawns
  threads on our behalf (one for each incoming RPC).
-------------------------------------------------------------------------------}

serverLocal1 ::
     TestClock
  -> ServerCall (TrivialRpc meth)
  -> IO ()
serverLocal1 testClock call = handle showExceptions $ do
    advanceTestClock testClock

    -- Wait for client early termination to become visible
    atomically $ do
      healthy <- serverIsCallHealthy call
      when healthy $ retry

    -- At this point, sending anything should fail
    waitForTestClockTick testClock 4
    serverSendOutput call $ NoMoreElems []
  where
    showExceptions :: SomeException -> IO ()
    showExceptions err = do
        putStrLn $ "serverLocal1: " ++ show err
        throwIO err

serverLocal2 ::
     TestClock
  -> ServerCall (TrivialRpc meth)
  -> IO ()
serverLocal2 testClock call = handle showExceptions $ do
    advanceTestClock testClock

    -- Wait for client to tell us they will send no more elements
    _ <- serverRecvInput call
    advanceTestClock testClock

    -- Tell the client we won't send them more elements either
    waitForTestClockTick testClock 5
    serverSendOutput call $ NoMoreElems []
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
      let serverHandlers :: [RpcHandler]
          serverHandlers = [
                mkRpcHandler (Proxy @TestRpc1) $ serverLocal1 testClock
              , mkRpcHandler (Proxy @TestRpc2) $ serverLocal2 testClock
              ]

          serverParams :: ServerParams
          serverParams = def {
                -- Don't print exceptions (to avoid confusing test output)
                Context.serverExceptionTracer = nullTracer
              }

      withServer serverParams serverHandlers $
        runServer (ServerConfig "50051")

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
