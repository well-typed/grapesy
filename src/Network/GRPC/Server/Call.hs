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
  , sendGrpcException
  , getRequestMetadata
  , setResponseInitialMetadata

    -- ** Protocol specific wrappers
  , sendNextOutput
  , sendFinalOutput
  , sendTrailers
  , recvNextInput
  , recvFinalInput
  , recvEndOfInput

    -- ** Low-level\/specialized API
  , initiateResponse
  , sendTrailersOnly
  , recvInputWithEnvelope
  , sendOutputWithEnvelope
  , getRequestTraceContext

    -- ** Internal API
  , sendProperTrailers
  , serverExceptionToClientError
  , getRequestTimeout

    -- * Exceptions
  , HandlerTerminated(..)
  , ResponseAlreadyInitiated(..)
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.XIO (XIO, XIO', NeverThrows)
import Control.Monad.XIO qualified as XIO
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

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Open connection to a client
data Call rpc = SupportsServerRpc rpc => Call {
      -- | Server context (across all calls)
      callContext :: ServerContext

      -- | Server session (for this call)
    , callSession :: ServerSession rpc

      -- | Bidirectional channel to the client
    , callChannel :: Session.Channel (ServerSession rpc)

      -- | Request headers
    , callRequestHeaders :: RequestHeaders

      -- | Response metadata
      --
      -- Metadata to be included when we send the initial response headers.
      --
      -- Can be updated until the first message (see 'callFirstMessage'), at
      -- which point it /must/ have been set (if not, an exception is thrown).
    , callResponseMetadata :: TVar (Maybe (ResponseInitialMetadata rpc))

      -- | What kicked off the response?
      --
      -- This is empty until the response /has/ in fact been kicked off.
    , callResponseKickoff :: TMVar Kickoff
    }

-- | What kicked off the response?
--
-- When the server handler starts, we do not immediately initiate the response
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
--
-- We record the 'CallStack' of the call that initiated the response.
data Kickoff =
    KickoffRegular      CallStack
  | KickoffTrailersOnly CallStack TrailersOnly
  deriving (Show)

kickoffCallStack :: Kickoff -> CallStack
kickoffCallStack (KickoffRegular      cs  ) = cs
kickoffCallStack (KickoffTrailersOnly cs _) = cs

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

-- | Setup call
--
-- No response is sent to the caller.
setupCall :: forall rpc.
     SupportsServerRpc rpc
  => Server.ConnectionToClient
  -> ServerContext
  -> XIO' CallSetupFailure (Call rpc)
setupCall conn callContext@ServerContext{params} = do
    callResponseMetadata <- XIO.unsafeTrustMe $ newTVarIO Nothing
    callResponseKickoff  <- XIO.unsafeTrustMe $ newEmptyTMVarIO

    flowStart :: Session.FlowStart (ServerInbound rpc) <-
      XIO.unsafeTrustMe $ Session.determineFlowStart callSession req

    let callRequestHeaders :: RequestHeaders
        callRequestHeaders =
            case flowStart of
              Session.FlowStartRegular    headers  -> inbHeaders headers
              Session.FlowStartNoMessages trailers ->            trailers

    -- Technically compression is only relevant in the 'KickoffRegular' case
    -- (i.e., in the case that the /server/ responds with messages, rather than
    -- make use the Trailers-Only case). However, the kickoff might not be known
    -- until the server has received various messages from the client. To
    -- simplify control flow, therefore, we do compression negotation early,
    -- to avoid having to deal with compression negotation failure later.
    cOut :: Compression <-
      case requestAcceptCompression callRequestHeaders of
         Nothing   -> return noCompression
         Just cids -> return $ Compr.choose compr cids

    callChannel :: Session.Channel (ServerSession rpc) <-
      XIO.neverThrows $ Session.setupResponseChannel
        callSession
        conn
        flowStart
        (mkOutboundHeaders callResponseMetadata callResponseKickoff cOut)

    return Call{
        callContext
      , callSession
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

    mkOutboundHeaders ::
         TVar (Maybe (ResponseInitialMetadata rpc))
      -> TMVar Kickoff
      -> Compression
      -> IO (Session.FlowStart (ServerOutbound rpc))
    mkOutboundHeaders metadataVar kickoffVar cOut = do
        -- Wait for kickoff (see 'Kickoff' for discussion)
        kickoff <- atomically $ readTMVar kickoffVar

        -- Get response metadata (see 'setResponseMetadata')
        -- It is important we read this only /after/ the kickoff.
        responseMetadata <- do
          mMetadata <- atomically $ readTVar metadataVar
          case mMetadata of
            Just md -> return $ buildMetadata md
            Nothing -> throwIO $ ResponseInitialMetadataNotSet

        -- Session start
        case kickoff of
          KickoffRegular _cs ->
            return $ Session.FlowStartRegular $ OutboundHeaders {
                outHeaders = ResponseHeaders {
                    responseCompression       = Just $ Compr.compressionId cOut
                  , responseAcceptCompression = Just $ Compr.offer compr
                  , responseMetadata          = customMetadataMapFromList
                                                  responseMetadata
                  , responseContentType       = Context.serverContentType params
                  }
              , outCompression = cOut
              }
          KickoffTrailersOnly _cs trailers ->
            return $ Session.FlowStartNoMessages trailers

-- | Accept incoming call
--
-- If the handler throws an exception, we will /attempt/ to inform the client
-- of what happened (see 'forwardException') before re-throwing the exception.
runHandler :: forall rpc.
     HasCallStack
  => Call rpc
  -> (Call rpc -> IO ())
  -> XIO ()
runHandler call@Call{callChannel} k = do
    -- http2 will kill the handler when the client disappears, but we want the
    -- handler to be able to terminate cleanly. We therefore run the handler in
    -- a separate thread, and wait for that thread to terminate.
    handlerThread <- liftIO $ async $ XIO.runThrow handler
    waitForHandler handlerThread
  where
    -- The handler itself will run in a separate thread
    handler :: XIO ()
    handler = do
        result <- liftIO $ try $ k call
        handlerTeardown result

    -- Deal with any exceptions thrown in the handler
    handlerTeardown :: Either SomeException () -> XIO ()
    handlerTeardown (Right ()) = do
        -- Handler terminated successfully, but may not have sent final message.
        -- /If/ the final message was sent, 'forwardException' does nothing.
        forwarded <- XIO.neverThrows $ do
          forwarded <- forwardException call $ toException HandlerTerminated
          ignoreUncleanClose $ ExitCaseSuccess ()
          return forwarded
        when forwarded $
          -- The handler terminated before it sent the final message.
          throwM HandlerTerminated
    handlerTeardown (Left err) = do
        -- The handler threw an exception. Attempt to tell the client.
        XIO.neverThrows $ do
          void $ forwardException call err
          ignoreUncleanClose $ ExitCaseException err
        throwM err

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
    ignoreUncleanClose :: ExitCase a -> XIO' NeverThrows ()
    ignoreUncleanClose reason =
        XIO.swallowIO $ void $ Session.close callChannel reason

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
    waitForHandler :: Async () -> XIO ()
    waitForHandler handlerThread = loop
      where
        loop :: XIO ()
        loop = do
            (liftIO $ wait handlerThread) `catch` \err ->
             case fromException err of
               Just (HTTP2.KilledByHttp2ThreadManager mErr) -> do
                 let exitReason :: ExitCase ()
                     exitReason =
                       case mErr of
                         Nothing -> ExitCaseSuccess ()
                         Just exitWithException ->
                           ExitCaseException . toException $
                             ClientDisconnected exitWithException callStack
                 XIO.neverThrows $ ignoreUncleanClose exitReason
                 loop
               Nothing -> do
                 -- If we get an exception while waiting on the handler, there
                 -- are two possibilities:
                 --
                 -- 1. The exception was an asynchronous exception, thrown to us
                 --    externally. In this case @cancalWith@ will throw the
                 --    exception to the handler (and wait for it to terminate).
                 -- 2. The exception was thrown by the handler itself. In this
                 --    case @cancelWith@ is a no-op.
                 liftIO $ cancelWith handlerThread err
                 throwM err

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
-- We therefore catch and suppress all exceptions here. Returns @True@ if the
-- forwarding was successful, @False@ if it raised an exception.
forwardException :: Call rpc -> SomeException -> XIO' NeverThrows Bool
forwardException call =
      XIO.swallowIO'
    . sendProperTrailers call
    . serverExceptionToClientError

-- | Turn exception raised in server handler to error to be sent to the client
--
-- TODO: There might be a security concern here (server-side exceptions could
-- potentially leak some sensitive data).
serverExceptionToClientError :: SomeException -> ProperTrailers
serverExceptionToClientError err
    | Just (err' :: GrpcException) <- fromException err
    = grpcExceptionToTrailers err'

    | otherwise
    = ProperTrailers {
          properTrailersGrpcStatus     = GrpcError GrpcUnknown
        , properTrailersGrpcMessage    = Just $ Text.pack (show err)
        , properTrailersMetadata       = mempty
        , properTrailersPushback       = Nothing
        , properTrailersOrcaLoadReport = Nothing
        }

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
-- This will send a 'GrpcStatus' of 'GrpcOk' to the client; for anything else
-- (i.e., to indicate something went wrong), the server handler should call
-- 'sendGrpcException'.
--
-- This is a blocking call if this is the final message (i.e., the call will not
-- return until the message has been written to the HTTP2 stream).
sendOutput ::
     HasCallStack
  => Call rpc
  -> StreamElem (ResponseTrailingMetadata rpc) (Output rpc) -> IO ()
sendOutput call = sendOutputWithEnvelope call . fmap (def,)

-- | Generalization of 'sendOutput' with additional control
--
-- This can be used for example to enable or disable compression for individual
-- messages.
--
-- Most applications will never need to use this function.
sendOutputWithEnvelope :: forall rpc.
     HasCallStack
  => Call rpc
  -> StreamElem (ResponseTrailingMetadata rpc) (OutboundEnvelope, Output rpc)
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
    mkTrailers :: ResponseTrailingMetadata rpc -> ProperTrailers
    mkTrailers metadata = ProperTrailers {
          properTrailersGrpcStatus     = GrpcOk
        , properTrailersGrpcMessage    = Nothing
        , properTrailersMetadata       = customMetadataMapFromList $
                                           buildMetadata metadata
        , properTrailersPushback       = Nothing
        , properTrailersOrcaLoadReport = Nothing
        }

-- | Send 'GrpcException' to the client
--
-- This closes the connection to the client; sending further messages will
-- result in an exception being thrown.
--
-- Instead of calling 'sendGrpcException' handlers can also simply /throw/ the
-- gRPC exception (the @grapesy@ /client/ API treats this the same way: a
-- 'GrpcStatus' other than 'GrpcOk' will be raised as a 'GrpcException'). The
-- difference is primarily one of preference/convenience, but the two are not
-- /completely/ the same: when the 'GrpcException' is thrown,
-- 'Context.serverTopLevel' will see the handler throw an exception (and, by
-- default, log that exception); when using 'sendGrpcException', the handler is
-- considered to have terminated normally. For handlers defined using
-- "Network.GRPC.Server.StreamType" throwing the exception is the only option.
--
-- Technical note: if the response to the client has not yet been initiated when
-- 'sendGrpcException' is called, this will make use of the [gRPC
-- Trailers-Only](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
-- case.
sendGrpcException :: Call rpc -> GrpcException -> IO ()
sendGrpcException call = sendProperTrailers call . grpcExceptionToTrailers

-- | Get request metadata
--
-- The request metadata is included in the client's request headers when they
-- first make the request, and is therefore available immediately to the handler
-- (even if the first /message/ from the client may not yet have been sent).
getRequestMetadata :: Call rpc -> IO (RequestMetadata rpc)
getRequestMetadata Call{callRequestHeaders} =
    parseMetadata . customMetadataMapToList $
      requestMetadata callRequestHeaders

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
setResponseInitialMetadata ::
     HasCallStack
  => Call rpc -> ResponseInitialMetadata rpc -> IO ()
setResponseInitialMetadata Call{ callResponseMetadata
                               , callResponseKickoff
                               }
                           md = atomically $ do
    mKickoff <- fmap kickoffCallStack <$> tryReadTMVar callResponseKickoff
    case mKickoff of
      Nothing -> writeTVar callResponseMetadata (Just md)
      Just cs -> throwSTM $ ResponseAlreadyInitiated cs callStack

{-------------------------------------------------------------------------------
  Low-level API
-------------------------------------------------------------------------------}

-- | Initiate the response
--
-- This will cause the initial response metadata to be sent
-- (see also 'setResponseMetadata').
--
-- Returns 'False' if the response was already initiated.
initiateResponse :: HasCallStack => Call rpc -> IO Bool
initiateResponse Call{callResponseKickoff} =
    atomically $ tryPutTMVar callResponseKickoff $ KickoffRegular callStack

-- | Use the gRPC @Trailers-Only@ case for non-error responses
--
-- Under normal circumstances a gRPC server will respond to the client with
-- an initial set of headers, then zero or more messages, and finally a set of
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
sendTrailersOnly :: HasCallStack => Call rpc -> [CustomMetadata] -> IO ()
sendTrailersOnly Call{ callContext
                     , callResponseKickoff
                     }
                 metadata
               = atomically $ do
    previously <- fmap kickoffCallStack <$> tryReadTMVar callResponseKickoff
    case previously of
      Nothing -> putTMVar callResponseKickoff $
                   KickoffTrailersOnly callStack trailers
      Just cs -> throwSTM $ ResponseAlreadyInitiated cs callStack
  where
    ServerContext{params} = callContext

    trailers :: TrailersOnly
    trailers = TrailersOnly {
          trailersOnlyContentType = Context.serverContentType params
        , trailersOnlyProper      = ProperTrailers {
              properTrailersGrpcStatus     = GrpcOk
            , properTrailersGrpcMessage    = Nothing
            , properTrailersMetadata       = customMetadataMapFromList metadata
            , properTrailersPushback       = Nothing
            , properTrailersOrcaLoadReport = Nothing
            }
        }

-- | Get trace context for the request (if any)
--
-- This provides (minimal) support for OpenTelemetry.
getRequestTraceContext :: Call rpc -> IO (Maybe TraceContext)
getRequestTraceContext Call{callRequestHeaders} =
    return $ requestTraceContext callRequestHeaders

{-------------------------------------------------------------------------------
  Protocol specific wrappers
-------------------------------------------------------------------------------}

-- | Send the next output
--
-- If this is the last output, you should call 'sendTrailers' after
-- (or use 'sendFinalOutput').
sendNextOutput ::
     HasCallStack
  => Call rpc -> Output rpc -> IO ()
sendNextOutput call = sendOutput call . StreamElem

-- | Send final output
--
-- See also 'sendTrailers'.
sendFinalOutput ::
     HasCallStack
  => Call rpc -> (Output rpc, ResponseTrailingMetadata rpc) -> IO ()
sendFinalOutput call = sendOutput call . uncurry FinalElem

-- | Send trailers
--
-- This tells the client that there will be no more outputs. You should call
-- this (or 'sendFinalOutput') even when there is no special information to be
-- included in the trailers.
sendTrailers ::
     HasCallStack
  => Call rpc -> ResponseTrailingMetadata rpc -> IO ()
sendTrailers call = sendOutput call . NoMoreElems

-- | Receive next input
--
-- Throws 'ProtocolException' if there are no more inputs.
recvNextInput :: forall rpc. HasCallStack => Call rpc -> IO (Input rpc)
recvNextInput call@Call{} = do
    mInp <- recvInput call
    case mInp of
      NoMoreElems    NoMetadata -> err $ TooFewInputs @rpc
      FinalElem  out NoMetadata -> return out
      StreamElem out            -> return out
  where
    err :: ProtocolException rpc -> IO a
    err = throwIO . ProtocolException

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

-- | Wait for the client to indicate that there are no more inputs
--
-- Throws 'ProtocolException' if we received an input.
recvEndOfInput :: forall rpc. HasCallStack => Call rpc -> IO ()
recvEndOfInput call@Call{} = do
    mInp <- recvInput call
    case mInp of
      NoMoreElems    NoMetadata -> return ()
      FinalElem  inp NoMetadata -> err $ TooManyInputs @rpc inp
      StreamElem inp            -> err $ TooManyInputs @rpc inp
  where
    err :: ProtocolException rpc -> IO a
    err = throwIO . ProtocolException

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
sendProperTrailers Call{callContext, callResponseKickoff, callChannel}
                   trailers = do
    updated <-
      atomically $
        tryPutTMVar callResponseKickoff $
          KickoffTrailersOnly
            callStack
            ( properTrailersToTrailersOnly (
                trailers
              , Context.serverContentType params
              )
            )
    unless updated $
      -- If we didn't update, then the response has already been initiated and
      -- we cannot make use of the Trailers-Only case.
      Session.send callChannel (NoMoreElems trailers)
    void $ Session.waitForOutbound callChannel
  where
    ServerContext{params} = callContext

-- | Get the timeout
--
-- This is internal API: the timeout is automatically imposed on the handler.
getRequestTimeout :: Call rpc -> Maybe Timeout
getRequestTimeout call = requestTimeout $ callRequestHeaders call

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Handler terminated early
--
-- This gets thrown in the handler, and sent to the client, when the handler
-- terminates before sending the final output and trailers.
data HandlerTerminated = HandlerTerminated
  deriving stock (Show)
  deriving anyclass (Exception)

data ResponseAlreadyInitiated = ResponseAlreadyInitiated {
      -- | When was the response first initiated?
      responseInitiatedFirst :: CallStack

      -- | Where did we attempt to initiate the response a second time?
    , responseInitiatedAgain :: CallStack
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | The response initial metadata must be set before the first message is sent
data ResponseInitialMetadataNotSet = ResponseInitialMetadataNotSet
  deriving stock (Show)
  deriving anyclass (Exception)