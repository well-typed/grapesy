{-# LANGUAGE OverloadedStrings #-}

-- | Open (ongoing) RPC call
--
-- Intended for uqnqualified import.
module Network.GRPC.Server.Call (
    Call -- opaque

    -- * Construction
  , setupCall
  , runHandler

    -- * Open (ongoing) call
  , recvInput
  , sendOutput
  , getRequestMetadata
  , setResponseMetadata

    -- ** Protocol specific wrappers
  , recvFinalInput
  , sendFinalOutput
  , sendNextOutput
  , sendTrailers

    -- ** Low-level\/specialized API
  , initiateResponse
  , sendTrailersOnly
  , isCallHealthy
  , recvInputWithEnvelope
  , sendOutputWithEnvelope
  , getRequestTraceContext

    -- ** Internal API
  , sendProperTrailers
  , serverExceptionToClientError
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Catch (ExitCase(..))
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Tracer
import Data.Bifunctor
import Data.Default
import Data.Text qualified as Text
import GHC.Stack
import Network.HTTP2.Internal qualified as HTTP2
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server.Context (ServerContext(..))
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Server.Session
import Network.GRPC.Spec
import Network.GRPC.Util.HTTP2.Stream (ClientDisconnected(..))
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Util.Session.Server qualified as Server

import Debug.Concurrent

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Call rpc = IsRPC rpc => Call {
      -- | Server state (across all calls)
      callSession :: ServerSession rpc

      -- | Bidirectional channel to the client
    , callChannel :: Session.Channel (ServerSession rpc)

      -- | Request headers
      --
      -- This is filled once we get the request headers
    , callRequestHeaders :: TMVar RequestHeaders

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

-- | Setup call
--
-- No response is sent to the caller.
--
-- Does not throw any exceptions.
setupCall :: forall rpc.
     IsRPC rpc
  => Server.ConnectionToClient
  -> ServerContext
  -> IO (Either SomeException (Call rpc))
setupCall conn ServerContext{params} = runExceptT $ do
    callRequestHeaders   <- liftIO $ newEmptyTMVarIO
    callResponseMetadata <- liftIO $ newTVarIO []
    callResponseKickoff  <- liftIO $ newEmptyTMVarIO

    flowStart :: Session.FlowStart (ServerInbound rpc) <-
      case Session.determineFlowStart callSession req of
        Left  err   -> throwError $ toException err
        Right start -> return start

    let inboundHeaders :: RequestHeaders
        inboundHeaders =
            case flowStart of
              Session.FlowStartRegular    headers  -> inbHeaders headers
              Session.FlowStartNoMessages trailers ->            trailers

    -- Make request headers available (e.g., see 'getRequestMetadata')
    liftIO $ atomically $ putTMVar callRequestHeaders inboundHeaders

    -- Technically compression is only relevant in the 'KickoffRegular' case
    -- (i.e., in the case that the /server/ responds with messages, rather than
    -- make use the Trailers-Only case). However, the kickoff might not be known
    -- until the server has received various messages from the client. To
    -- simplify control flow, therefore, we do compression negotation early,
    -- to avoid having to deal with compression negotation failure later.
    cOut :: Compression <-
      case requestAcceptCompression inboundHeaders of
         Nothing   -> return noCompression
         Just cids ->
           -- If the requests explicitly lists compression algorithms, and
           -- that list does /not/ include @identity@, then we should not
           -- default to 'noCompression', even if all other algorithms are
           -- unsupported. This gives the client the option to /insist/ on
           -- compression.
           case Compr.choose compr cids of
             Right c   -> return c
             Left  err -> throwError $ toException err

    callChannel :: Session.Channel (ServerSession rpc) <- liftIO $
      Session.setupResponseChannel
        callSession
        (contramap (Context.ServerDebugMsg @rpc) tracer)
        conn
        flowStart
        (mkOutboundHeaders callResponseMetadata callResponseKickoff cOut)

    return Call{
        callSession
      , callRequestHeaders
      , callResponseMetadata
      , callResponseKickoff
      , callChannel
      }
  where
    callSession :: ServerSession rpc
    callSession = ServerSession {
          serverCompression = compr
        }

    compr :: Compr.Negotation
    compr = Context.serverCompression params

    req :: HTTP2.Request
    req = Server.request conn

    tracer :: Tracer IO Context.ServerDebugMsg
    tracer = Context.serverDebugTracer params

    mkOutboundHeaders ::
         TVar [CustomMetadata]
      -> TMVar Kickoff
      -> Compression
      -> IO (Session.FlowStart (ServerOutbound rpc))
    mkOutboundHeaders metadataVar kickoffVar cOut = do
        -- Wait for kickoff (see 'Kickoff' for discussion)
        kickoff <- atomically $ readTMVar kickoffVar

        -- Get response metadata (see 'setResponseMetadata')
        -- It is important we read this only /after/ the kickoff.
        responseMetadata <- atomically $ readTVar metadataVar

        -- Session start
        case kickoff of
          KickoffRegular ->
            return $ Session.FlowStartRegular $ OutboundHeaders {
                outHeaders = ResponseHeaders {
                    responseCompression =
                      Just $ Compr.compressionId cOut
                  , responseAcceptCompression =
                      Just $ Compr.offer compr
                  , responseMetadata =
                      responseMetadata
                  , responseOverrideContentType =
                      Context.serverContentType params
                  }
              , outCompression = cOut
              }
          KickoffTrailersOnly trailers ->
            return $ Session.FlowStartNoMessages trailers

-- | Accept incoming call
--
-- Can return an exception in one of two cases:
--
-- * The handler threw an exception. If this happens, we will /attempt/ to
--   inform the client of what happened; see 'forwardException'.
-- * We were interrupted by an asynchronous exception while we were waiting for
--   the handler. However, unless the call to 'runHandler' is wrapped in a
--   timeout (or similar), no asynchronous exceptions are expected.
runHandler :: forall rpc.
     HasCallStack
  => (forall x. IO x -> IO x)
  -> Call rpc
  -> (Call rpc -> IO ())
  -> IO (Either SomeException ())
runHandler unmaskTopLevel call@Call{callChannel} k = do
    -- http2 will kill the handler when the client disappears, but we want the
    -- handler to be able to terminate cleanly. We therefore run the handler in
    -- a separate thread, and wait for that thread to terminate.
    waitForHandler =<< asyncWithUnmask handler
  where
    -- The handler itself will run in a separate thread
    handler :: (forall a. IO a -> IO a) -> IO (Either SomeException ())
    handler unmaskInThread = do
        mRes <- try $ unmaskInThread $ k call
        handlerTeardown mRes
        return mRes

    -- Deal with any exceptions thrown in the handler
    handlerTeardown :: Either SomeException () -> IO ()
    handlerTeardown (Right ()) = ignoreUncleanClose $ do
        -- Handler terminated successfully, but may not have sent final message.
        -- /If/ the final message was sent, 'forwardException' does nothing.
        forwardException call $ toException HandlerTerminated
        Session.close callChannel $ ExitCaseSuccess ()
    handlerTeardown (Left err) = ignoreUncleanClose $ do
        -- The handler threw an exception. Attempt to tell the client.
        forwardException call err
        Session.close callChannel $ ExitCaseException err

    -- An unclean shutdown can have 2 causes:
    --
    -- 1. We lost communication during the call.
    --
    --    We have no way of telling the client that something went wrong.
    --
    -- 2. The handler failed to properly terminate the communication
    --    (send the final message and call 'waitForOutbound').
    --
    --    This is a bug in the handler, and is trickier to deal with. We don't
    --    really know what state the handler left the channel in; for example,
    --    we might have killed the thread halfway through sending a message.
    --
    --    So there not really anything we can do here (except perhaps show
    --    the exception in 'serverTopLevel').
    ignoreUncleanClose :: IO (Maybe SomeException) -> IO ()
    ignoreUncleanClose = void

    -- Wait for the handler to terminate
    --
    -- If we are interrupted while we wait, it depends on the
    -- interruption:
    --
    -- * If we are interrupted by 'HTTP2.KilledByHttp2ThreadManager', it means
    --   we got disconnected from the client. In this case, we shut down the
    --   channel (if it's not already shut down); /if/ the handler at this tries
    --   to communicate with the client, an exception will be raised. However,
    --   the handler /can/ terminate cleanly and, of course, typically will.
    --   Importantly, this avoids race conditions: even if the server and the
    --   client agree that no further communication takes place (the client sent
    --   their final message, the server sent the trailers), without the
    --   indireciton of this additional thread it can still happen that http2
    --   kills the handler (after the client disconnects) before the handler has
    --   a chance to terminate.
    -- * If we are interrupted by another kind of asynchronous exception, we
    --   /do/ kill the handler (this might for example be a timeout).
    --
    -- This is in line with the overall design philosophy of communication in
    -- this library: exceptions will only be raised synchronously when
    -- communication is attempted, not asynchronously when we notice a problem.
    waitForHandler ::
         Async (Either SomeException ())
      -> IO (Either SomeException ())
    waitForHandler handlerThread = loop
      where
        loop :: IO (Either SomeException ())
        loop = do
           -- We do not need to use 'waitCatch': the handler installs its
           -- own exception handler and will not throw any exceptions.
           mRes <- try $ unmaskTopLevel $ wait handlerThread
           case mRes of
             Right res -> return res
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
                   return $ Left err

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
forwardException :: Call rpc -> SomeException -> IO ()
forwardException call =
      handle ignoreExceptions
    . sendProperTrailers call
    . serverExceptionToClientError
  where
    ignoreExceptions :: SomeException -> IO ()
    ignoreExceptions _ = return ()

-- | Turn exception raised in server handler to error to be sent to the client
--
-- TODO: There might be a security concern here (server-side exceptions could
-- potentially leak some sensitive data).
serverExceptionToClientError :: SomeException -> ProperTrailers
serverExceptionToClientError err
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

-- | Sent to the client when the handler terminates early
data HandlerTerminated = HandlerTerminated
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Open (ongoing) call
-------------------------------------------------------------------------------}

-- | Receive RPC input from the client
--
-- We do not return trailers, since gRPC does not support sending trailers from
-- the client to the server (only from the server to the client).
recvInput :: HasCallStack => Call rpc -> IO (StreamElem NoMetadata (Input rpc))
recvInput = fmap (fmap snd) . recvInputWithEnvelope

-- | Generalization of 'recvInput', providing additional meta-information
--
-- This can be used to get some information about how the message was sent, such
-- as its compressed and uncompressed size.
--
-- Most applications will never need to use this function.
recvInputWithEnvelope ::
     HasCallStack
  => Call rpc
  -> IO (StreamElem NoMetadata (InboundEnvelope, Input rpc))
recvInputWithEnvelope Call{callChannel} =
    first ignoreTrailersOnly <$> Session.recv callChannel
  where
    ignoreTrailersOnly :: Either RequestHeaders NoMetadata -> NoMetadata
    ignoreTrailersOnly _ = NoMetadata

-- | Send RPC output to the client
--
-- This will send a @grpc-status@ of @0@ to the client; for anything else (i.e.,
-- to indicate something went wrong), the server handler should throw a
-- 'GrpcException' (the @grapesy@ client API treats this the same way: a
-- @grpc-status@ other than @0@ will be raised as a 'GrpcException').
--
-- This is a blocking call if this is the final message (i.e., the call will
-- not return until the message has been written to the HTTP2 stream).
sendOutput :: Call rpc -> StreamElem [CustomMetadata] (Output rpc) -> IO ()
sendOutput call = sendOutputWithEnvelope call . fmap (def,)

-- | Generalization of 'sendOutput' with additional control
--
-- This can be used for example to enable or disable compression for individual
-- messages.
--
-- Most applications will never need to use this function.
sendOutputWithEnvelope ::
     Call rpc
  -> StreamElem [CustomMetadata] (OutboundEnvelope, Output rpc)
  -> IO ()
sendOutputWithEnvelope call@Call{callChannel} msg = do
    _updated <- initiateResponse call
    Session.send callChannel (first mkTrailers msg)

    -- This /must/ be called before leaving the scope of 'acceptCall' (or we
    -- risk that the HTTP2 stream is cancelled). We can't call 'waitForOutbound'
    -- /in/ 'acceptCall', because if the handler for whatever reason never
    -- writes the final message, such a call would block indefinitely.
    StreamElem.whenDefinitelyFinal msg $ \_ ->
      void $ Session.waitForOutbound callChannel
  where
    mkTrailers :: [CustomMetadata] -> ProperTrailers
    mkTrailers metadata = ProperTrailers {
          trailerGrpcStatus  = GrpcOk
        , trailerGrpcMessage = Nothing
        , trailerMetadata    = metadata
        }

-- | Get request metadata
--
-- This will block until we have received the initial request /headers/ (but may
-- well return before we receive the first /message/ from the client).
getRequestMetadata :: Call rpc -> IO [CustomMetadata]
getRequestMetadata Call{callRequestHeaders} =
    atomically $ requestMetadata <$> readTMVar callRequestHeaders

-- | Set the initial response metadata
--
-- This can be set at any time before the response is initiated (either
-- implicitly by calling 'sendOutput', or explicitly by calling
-- 'initiateResponse' or 'sendTrailersOnly'). If the response has already
-- been initiated (and therefore the initial response metadata already sent),
-- will throw 'ResponseAlreadyInitiated'.
--
-- Note that this is about the /initial/ metadata; additional metadata can be
-- sent after the final message; see 'sendOutput'.
setResponseMetadata :: Call rpc -> [CustomMetadata] -> IO ()
setResponseMetadata Call{ callResponseMetadata
                        , callResponseKickoff
                        }
                    md = atomically $ do
    mKickoff <- tryReadTMVar callResponseKickoff
    case mKickoff of
      Nothing -> writeTVar callResponseMetadata md
      Just _  -> throwSTM ResponseAlreadyInitiated

{-------------------------------------------------------------------------------
  Low-level API
-------------------------------------------------------------------------------}

-- | Initiate the response
--
-- This will cause the initial response metadata to be sent
-- (see also 'setResponseMetadata').
--
-- Returns 'False' if the response was already initiated.
initiateResponse :: Call rpc -> IO Bool
initiateResponse Call{callResponseKickoff} =
    atomically $ tryPutTMVar callResponseKickoff KickoffRegular

-- | Use the gRPC @Trailers-Only@ case for non-error responses
--
-- Under normal circumstances a gRPC server will respond to the client with
-- an initial set of headers, than zero or more messages, and finally a set of
-- trailers. When there /are/ no messages, this /can/ be collapsed into a single
-- set of trailers (or headers, depending on your point of view); the gRPC
-- specification refers to this as the @Trailers-Only@ case. It mandates:
--
-- > Most responses are expected to have both headers and trailers but
-- > Trailers-Only is permitted for calls that produce an immediate error.
--
-- In @grapesy@, if a server handler throws a 'GrpcException', we will make use
-- of this @Trailers-Only@ case if applicable, as per the specification.
--
-- /However/, some servers make use of @Trailers-Only@ also in non-error cases.
-- For example, the @listFeatures@ handler in the official Python route guide
-- example server will use @Trailers-Only@ if there are no features to report.
-- Since this is not conform the gRPC specification, we do not do this in
-- @grapesy@ by default, but we make the option available through
-- 'sendTrailersOnly'.
--
-- Throws 'ResponseAlreadyInitiated' if the response has already been initiated.
sendTrailersOnly :: Call rpc -> [CustomMetadata] -> IO ()
sendTrailersOnly Call{callResponseKickoff} metadata = do
    updated <- atomically $ tryPutTMVar callResponseKickoff $
                 KickoffTrailersOnly trailers
    unless updated $ throwIO ResponseAlreadyInitiated
  where
    trailers :: TrailersOnly
    trailers = TrailersOnly $ ProperTrailers {
          trailerGrpcStatus  = GrpcOk
        , trailerGrpcMessage = Nothing
        , trailerMetadata    = metadata
        }

data ResponseAlreadyInitiated = ResponseAlreadyInitiated
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Check if the connection to the client is healthy
--
-- See 'Network.GRPC.Client.isCallHealthy' for considerations regarding the
-- non-deterministic result of this function.
isCallHealthy :: Call rpc -> STM Bool
isCallHealthy = Session.isChannelHealthy . callChannel

-- | Get trace context for the request (if any)
--
-- This provides (minimal) support for OpenTelemetry.
getRequestTraceContext :: Call rpc -> IO (Maybe TraceContext)
getRequestTraceContext Call{callRequestHeaders} =
    atomically $ requestTraceContext <$> readTMVar callRequestHeaders

{-------------------------------------------------------------------------------
  Protocol specific wrappers
-------------------------------------------------------------------------------}

-- | Receive input, which we expect to be the /final/ input
--
-- Throws 'ProtocolException' if the input we receive is not final.
--
-- NOTE: If the first input we receive from the client is not marked as final,
-- we will block until we receive the end-of-stream indication.
recvFinalInput :: forall rpc. HasCallStack => Call rpc -> IO (Input rpc)
recvFinalInput call@Call{} = do
    inp1 <- recvInput call
    case inp1 of
      NoMoreElems    NoMetadata -> err $ TooFewInputs @rpc
      FinalElem  inp NoMetadata -> return inp
      StreamElem inp            -> do
        inp2 <- recvInput call
        case inp2 of
          NoMoreElems     NoMetadata -> return inp
          FinalElem  inp' NoMetadata -> err $ TooManyInputs @rpc inp'
          StreamElem inp'            -> err $ TooManyInputs @rpc inp'
  where
    err :: ProtocolException rpc -> IO a
    err = throwIO . ProtocolException

-- | Send final output
--
-- See also 'sendTrailers'.
sendFinalOutput :: Call rpc -> (Output rpc, [CustomMetadata]) -> IO ()
sendFinalOutput call = sendOutput call . uncurry FinalElem

-- | Send the next output
--
-- If this is the last output, you should call 'sendTrailers' after.
sendNextOutput :: Call rpc -> Output rpc -> IO ()
sendNextOutput call = sendOutput call . StreamElem

-- | Send trailers
--
-- This tells the client that there will be no more outputs. You should call
-- this (or 'sendFinalOutput') even when there is no special information to be
-- included in the trailers.
sendTrailers :: Call rpc -> [CustomMetadata] -> IO ()
sendTrailers call = sendOutput call . NoMoreElems

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
sendProperTrailers :: Call rpc -> ProperTrailers -> IO ()
sendProperTrailers Call{callResponseKickoff, callChannel} trailers = do
    updated <- atomically $ tryPutTMVar callResponseKickoff $
                 KickoffTrailersOnly (TrailersOnly trailers)
    unless updated $
      -- If we didn't update, then the response has already been initiated and
      -- we cannot make use of the Trailers-Only case.
      Session.send callChannel (NoMoreElems trailers)
    void $ Session.waitForOutbound callChannel
