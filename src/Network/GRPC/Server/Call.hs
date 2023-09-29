{-# LANGUAGE OverloadedStrings #-}

-- | Open (ongoing) RPC call
--
-- Intended for uqnqualified import.
module Network.GRPC.Server.Call (
    Call -- opaque

    -- * Construction
  , acceptCall

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

    -- ** Low-level API
  , recvInputSTM
  , sendOutputSTM
  , initiateResponse
  , sendTrailersOnly
  , waitForOutbound
  , isCallHealthy

    -- ** Internal API
  , sendProperTrailers
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Tracer
import Data.Bifunctor
import Data.Text qualified as Text
import GHC.Stack
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Internal qualified as HTTP2
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server.Connection (Connection(..))
import Network.GRPC.Server.Connection qualified as Connection
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Server.Session
import Network.GRPC.Spec
import Network.GRPC.Util.Concurrency
import Network.GRPC.Util.HTTP2.Stream (ClientDisconnected(..))
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Util.Session.Server qualified as Server

import Debug.Trace
import GHC.IO.Exception

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Call rpc = IsRPC rpc => Call {
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
acceptCall :: forall rpc.
     (IsRPC rpc, HasCallStack)
  => Context.ServerParams
  -> Connection
  -> (Call rpc -> IO ())
  -> IO ()
acceptCall params conn k = do
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
             Call rpc
          -> Either SomeException ()
          -> IO ()
        handlerTeardown call@Call{callChannel} mRes = do
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
            call :: Call rpc
            call = Call{
                callSession
              , callChannel
              , callRequestMetadata
              , callResponseMetadata
              , callResponseKickoff
              }

    runHandler setupResponseChannel handler
  where
    callSession :: ServerSession rpc
    callSession = ServerSession {
          serverCompression = compr
        }

    compr :: Compr.Negotation
    compr = Context.serverCompression $ Context.params $ connectionContext conn

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
            cOut :: Compression <-
              case requestAcceptCompression inboundHeaders of
                 Nothing   -> return noCompression
                 Just cids ->
                   -- If the requests explicitly lists compression algorithms,
                   -- and that list does /not/ include @identity@, then we
                   -- should not default to 'noCompression', even if all other
                   -- algorithms are unsupported. This gives the client the
                   -- option to /insist/ on compression.
                   case Compr.choose compr cids of
                     Right c   -> return c
                     Left  err -> throwM err
            traceIO $ "mkOutboundHeaders 5" ++ show cOut
            return $ Session.FlowStartRegular $ OutboundHeaders {
                outHeaders = ResponseHeaders {
                    responseCompression       = Just $ Compr.compressionId cOut
                  , responseAcceptCompression = Just $ Compr.offer compr
                  , responseMetadata          = responseMetadata
                  }
              , outCompression = cOut
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
        throwM err

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
                 -- Relies on https://github.com/kazu-yamamoto/http2/pull/82
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
                     throwM err

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
forwardException :: Call rpc -> SomeException -> IO ()
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
recvInput :: forall rpc. HasCallStack => Call rpc -> IO (StreamElem NoMetadata (Input rpc))
recvInput call =
    catch go $ \e@BlockedIndefinitelyOnSTM{} -> do
      traceIO $ "Uhoh.. got the dreaded " ++ show e
      throwM e
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
sendOutput :: Call rpc -> StreamElem [CustomMetadata] (Output rpc) -> IO ()
sendOutput call msg = do
    _updated <- initiateResponse call
    atomically $ sendOutputSTM call msg
    StreamElem.whenDefinitelyFinal msg $ \_ -> waitForOutbound call

-- | Get request metadata
--
-- This will block until we have received the initial request /headers/ (but may
-- well return before we receive the first /message/ from the client).
getRequestMetadata :: Call rpc -> IO [CustomMetadata]
getRequestMetadata Call{callRequestMetadata} =
    atomically $ readTMVar callRequestMetadata

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
  => Call rpc
  -> STM (StreamElem NoMetadata (Input rpc))
recvInputSTM Call{callChannel} =
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
sendOutputSTM :: Call rpc -> StreamElem [CustomMetadata] (Output rpc) -> STM ()
sendOutputSTM Call{callChannel, callResponseKickoff} msg = do
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
    unless updated $ throwM ResponseAlreadyInitiated
  where
    trailers :: TrailersOnly
    trailers = TrailersOnly $ ProperTrailers {
          trailerGrpcStatus  = GrpcOk
        , trailerGrpcMessage = Nothing
        , trailerMetadata    = metadata
        }

-- | Wait for all outbound messages to have been processed
--
-- This function /must/ be called before leaving the scope of 'acceptCall'.
-- However, 'sendOutput' will call 'waitForOutbound' when the last message is
-- sent, so you only need to worry about this if you are using 'sendOutputSTM'.
waitForOutbound :: Call rpc -> IO ()
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
isCallHealthy :: Call rpc -> STM Bool
isCallHealthy = Session.isChannelHealthy . callChannel

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
      NoMoreElems    NoMetadata -> throwM $ TooFewInputs @rpc
      FinalElem  inp NoMetadata -> return inp
      StreamElem inp            -> do
        inp2 <- recvInput call
        case inp2 of
          NoMoreElems     NoMetadata -> return inp
          FinalElem  inp' NoMetadata -> throwM $ TooManyInputs @rpc inp'
          StreamElem inp'            -> throwM $ TooManyInputs @rpc inp'

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
      atomically $ Session.send callChannel (NoMoreElems trailers)
