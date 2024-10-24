{-# LANGUAGE OverloadedStrings #-}

-- | Open (ongoing) RPC call
--
-- Intended for unqualified import.
module Network.GRPC.Client.Call (
    -- * Construction
    Call -- opaque
  , withRPC

    -- * Open (ongoing) call
  , sendInput
  , recvOutput
  , recvResponseMetadata

    -- ** Protocol specific wrappers
  , sendNextInput
  , sendFinalInput
  , sendEndOfInput
  , recvResponseInitialMetadata
  , recvNextOutput
  , recvFinalOutput
  , recvTrailers

    -- ** Low-level\/specialized API
  , sendInputWithMeta
  , recvNextOutputElem
  , recvOutputWithMeta
  , recvInitialResponse
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.Foldable (asum)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text qualified as Text
import Data.Version
import GHC.Stack

import Network.GRPC.Client.Connection (Connection, ConnParams(..))
import Network.GRPC.Client.Connection qualified as Connection
import Network.GRPC.Client.Session
import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Spec
import Network.GRPC.Util.GHC
import Network.GRPC.Util.HKD qualified as HKD
import Network.GRPC.Util.HTTP2.Stream (ServerDisconnected(..))
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Util.Thread qualified as Thread

import Paths_grapesy qualified as Grapesy

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

-- | State of the call
--
-- This type is kept abstract (opaque) in the public facing API.
data Call rpc = SupportsClientRpc rpc => Call {
      callChannel :: Session.Channel (ClientSession rpc)
    }

-- | Scoped RPC call
--
-- This is the low-level API for making RPC calls, providing full flexibility.
-- You may wish to consider using the infrastructure from
-- "Network.GRPC.Client.StreamType.IO" instead.
--
-- Typical usage:
--
-- > withRPC conn def (Proxy @ListFeatures) $ \call -> do
-- >   .. use 'call' to send and receive messages
--
-- for some previously established connection 'conn'
-- (see 'Network.GRPC.Client.withConnection') and where @ListFeatures@ is some
-- kind of RPC.
--
-- The call is setup in the background, and might not yet have been established
-- when the body is run. If you want to be sure that the call has been setup,
-- you can call 'recvResponseMetadata'.
--
-- Leaving the scope of 'withRPC' before the client informs the server that they
-- have sent their last message (using 'sendInput' or 'sendEndOfInput') is
-- considered a cancellation, and accordingly throws a 'GrpcException' with
-- 'GrpcCancelled' (see also <https://grpc.io/docs/guides/cancellation/>).
--
-- There is one exception to this rule: if the server unilaterally closes the
-- RPC (that is, the server already sent the trailers), then the call is
-- considered closed and the cancellation exception is not raised. Under normal
-- circumstances (with well-behaved server handlers) this should not arise.
-- (The gRPC specification itself is not very specific about this case; see
-- discussion at <https://stackoverflow.com/questions/55511528/should-grpc-server-side-half-closing-implicitly-terminate-the-client>.)
--
-- If there are still /inbound/ messages upon leaving the scope of 'withRPC' no
-- exception is raised (but the call is nonetheless still closed, and the server
-- handler will be informed that the client has disappeared).
withRPC :: forall rpc m a.
     (MonadMask m, MonadIO m, SupportsClientRpc rpc, HasCallStack)
  => Connection -> CallParams rpc -> Proxy rpc -> (Call rpc -> m a) -> m a
withRPC conn callParams proxy k = fmap fst $
      generalBracket
        (liftIO $ startRPC conn proxy callParams)
        closeRPC
        (k . fst)
  where
    closeRPC :: (Call rpc, Session.CancelRequest) -> ExitCase a -> m ()
    closeRPC (call, cancelRequest) exitCase = liftIO $ do
        -- /Before/ we do anything else (see below), check if we have evidence
        -- that we can discard the connection.
        canDiscard <- checkCanDiscard call

        -- Send the RST_STREAM frame /before/ closing the outbound thread.
        --
        -- When we call 'Session.close', we will terminate the
        -- 'sendMessageLoop', @http2@ will interpret this as a clean termination
        -- of the stream. We must therefore cancel this stream before calling
        -- 'Session.close'. /If/ the final message has already been sent,
        -- @http2@ guarantees (as a postcondition of @outBodyPushFinal@) that
        -- cancellation will be a no-op.
        sendResetFrame cancelRequest exitCase

        -- Now close the /outbound/ thread, see docs of 'Session.close' for
        -- details.
        mException <- liftIO $ Session.close (callChannel call) exitCase
        case mException of
          Nothing ->
            -- The outbound thread had already terminated
            return ()
          Just ex ->
            case fromException ex of
              Nothing ->
                -- We are leaving the scope of 'withRPC' because of an exception
                -- in the client, just rethrow that exception.
                throwM ex
              Just discarded ->
                -- We are leaving the scope of 'withRPC' without having sent the
                -- final message.
                --
                -- If the server was closed before we cancelled the stream, this
                -- means that the server unilaterally closed the connection.
                -- This should be regarded as normal termination of the RPC (see
                -- the docs for 'withRPC')
                --
                -- Otherwise, the client left the scope of 'withRPC' before the
                -- RPC was complete, which the gRPC spec mandates to result in a
                -- 'GrpcCancelled' exception. See docs of 'throwCancelled'.
                unless canDiscard $
                  throwCancelled discarded

    -- Send a @RST_STREAM@ frame if necessary
    sendResetFrame :: Session.CancelRequest -> ExitCase a -> IO ()
    sendResetFrame cancelRequest exitCase =
        cancelRequest $
          case exitCase of
            ExitCaseSuccess _ ->
              -- Error code will be CANCEL
              Nothing
            ExitCaseAbort ->
              -- Error code will be INTERNAL_ERROR. The client aborted with an
              -- error that we don't have access to. We want to tell the server
              -- that something has gone wrong (i.e. INTERNAL_ERROR), so we must
              -- pass an exception, however the exact nature of the exception is
              -- not particularly important as it is only recorded locally.
              Just . toException $ Session.ChannelAborted callStack
            ExitCaseException e ->
              -- Error code will be INTERNAL_ERROR
              Just e

    -- The spec mandates that when a client cancels a request (which in grapesy
    -- means exiting the scope of withRPC), the client receives a CANCELLED
    -- exception. We need to deal with the edge case mentioned above, however:
    -- the server might have already closed the connection. The client must have
    -- evidence that this is the case, which could mean one of two things:
    --
    -- o The client received the final message from the server
    -- o The server threw an exception (and the client saw this)
    --
    -- We can check for the former using 'channelRecvFinal', and the latter
    -- using 'hasThreadTerminated'. By checking both, we avoid race conditions:
    --
    -- o If the client received the final message, 'channelRecvFinal' /will/
    --   have been updated (we update this in the same transaction that returns
    --   the actual element; see 'Network.GRPC.Util.Session.Channel.recv').
    -- o If the server threw an exception, and the client observed this, then
    --   the inbound thread state /must/ have changed to 'ThreadException'.
    --
    -- Note that it is /not/ sufficient to check if the inbound thread has
    -- terminated: we might have received the final message, but the thread
    -- might still be /about/ to terminate, but not /actually/ have terminated.
    --
    -- See also:
    --
    -- o <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#cancel_after_begin>
    -- o <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#cancel_after_first_response>
    throwCancelled :: ChannelDiscarded -> IO ()
    throwCancelled (ChannelDiscarded cs) = do
        throwM $ GrpcException {
            grpcError         = GrpcCancelled
          , grpcErrorMessage  = Just $ mconcat [
                                    "Channel discarded by client at "
                                  , Text.pack $ prettyCallStack cs
                                  ]
          , grpcErrorMetadata = []
          }

    checkCanDiscard :: Call rpc -> IO Bool
    checkCanDiscard Call{callChannel} = do
        mRecvFinal  <- atomically $
          readTVar $ Session.channelRecvFinal callChannel
        let onNotRunning :: STM ()
            onNotRunning = return ()
        mTerminated <- atomically $
          Thread.getThreadState_
            (Session.channelInbound callChannel)
            onNotRunning
        return $
          or [
              case mRecvFinal of
                Session.RecvNotFinal          -> False
                Session.RecvWithoutTrailers _ -> True
                Session.RecvFinal _           -> True

              -- We are checking if we have evidence that we can discard the
              -- channel. If the inbound thread is not yet running, this implies
              -- that the server has not yet initiated their response to us,
              -- which means we have no evidence to believe we can discard the
              -- channel.
            , case mTerminated of
                Thread.ThreadNotYetRunning_ () -> False
                Thread.ThreadRunning_          -> False
                Thread.ThreadDone_             -> True
                Thread.ThreadException_ _      -> True
            ]

-- | Open new channel to the server
--
-- This is a non-blocking call; the connection will be set up in a
-- background thread; if this takes time, then the first call to
-- 'sendInput' or 'recvOutput' will block, but the call to 'startRPC'
-- itself will not block. This non-blocking nature makes this safe to use
-- in 'bracket' patterns.
startRPC :: forall rpc.
     (SupportsClientRpc rpc, HasCallStack)
  => Connection
  -> Proxy rpc
  -> CallParams rpc
  -> IO (Call rpc, Session.CancelRequest)
startRPC conn _ callParams = do
    (connClosed, connToServer) <- Connection.getConnectionToServer conn
    cOut     <- Connection.getOutboundCompression conn
    metadata <- buildMetadataIO $ callRequestMetadata callParams
    let flowStart :: Session.FlowStart (ClientOutbound rpc)
        flowStart = Session.FlowStartRegular $ OutboundHeaders {
            outHeaders     = requestHeaders cOut metadata
          , outCompression = fromMaybe noCompression cOut
          }

    let serverClosedConnection ::
             Either (TrailersOnly' HandledSynthesized) ProperTrailers'
          -> SomeException
        serverClosedConnection =
              either toException toException
            . grpcClassifyTermination
            . either trailersOnlyToProperTrailers' id

    (channel, cancelRequest) <-
      Session.setupRequestChannel
        session
        connToServer
        serverClosedConnection
        flowStart

    -- Spawn a thread to monitor the connection, and close the new channel when
    -- the connection is closed. To prevent a memory leak by hanging on to the
    -- channel for the lifetime of the connection, the thread also terminates in
    -- the (normal) case that the channel is closed before the connection is.
    _ <- forkLabelled "grapesy:monitorConnection" $ do
      status <- atomically $ do
          (Left <$> Thread.waitForNormalOrAbnormalThreadTermination
                      (Session.channelOutbound channel))
        `orElse`
          (Right <$> readTMVar connClosed)
      case status of
        Left _ -> return () -- Channel closed before the connection
        Right mErr -> do
          let exitReason :: ExitCase ()
              exitReason =
                case mErr of
                  Nothing -> ExitCaseSuccess ()
                  Just exitWithException ->
                    ExitCaseException . toException $
                      ServerDisconnected exitWithException callStack
          _mAlreadyClosed <- Session.close channel exitReason
          return ()

    return (Call channel, cancelRequest)
  where
    connParams :: ConnParams
    connParams = Connection.connParams conn

    requestHeaders :: Maybe Compression -> [CustomMetadata] -> RequestHeaders
    requestHeaders cOut metadata = RequestHeaders{
          requestTimeout =
            asum [
                callTimeout callParams
              , connDefaultTimeout connParams
              ]
        , requestMetadata =
            customMetadataMapFromList metadata
        , requestCompression =
            compressionId <$> cOut
        , requestAcceptCompression = Just $
            Compression.offer $ connCompression connParams
        , requestContentType =
            connContentType connParams
        , requestMessageType =
            Just MessageTypeDefault
        , requestUserAgent = Just $
            mconcat [
                "grpc-haskell-grapesy/"
              , mconcat . intersperse "." $
                  map (BS.Strict.C8.pack . show) $
                    versionBranch Grapesy.version
              ]
        , requestIncludeTE =
            True
        , requestTraceContext =
            Nothing
        , requestPreviousRpcAttempts =
            Nothing
        , requestUnrecognized =
            ()
        }

    session :: ClientSession rpc
    session = ClientSession {
          clientConnection = conn
        }

{-------------------------------------------------------------------------------
  Open (ongoing) call
-------------------------------------------------------------------------------}

-- | Send an input to the peer
--
-- Calling 'sendInput' again after sending the final message is a bug.
sendInput ::
     (HasCallStack, MonadIO m)
  => Call rpc
  -> StreamElem NoMetadata (Input rpc)
  -> m ()
sendInput call = sendInputWithMeta call . fmap (def,)

-- | Generalization of 'sendInput', providing additional control
--
-- See also 'Network.GRPC.Server.sendOutputWithMeta'.
--
-- Most applications will never need to use this function.
sendInputWithMeta ::
     (HasCallStack, MonadIO m)
  => Call rpc
  -> StreamElem NoMetadata (OutboundMeta, Input rpc)
  -> m ()
sendInputWithMeta Call{callChannel} msg = liftIO $ do
    Session.send callChannel msg

    -- This should be called before exiting the scope of 'withRPC'.
    StreamElem.whenDefinitelyFinal msg $ \_ ->
      Session.waitForOutbound callChannel

-- | Receive an output from the peer
--
-- After the final 'Output', you will receive any custom metadata (application
-- defined trailers) that the server returns. We do /NOT/ include the
-- 'GrpcStatus' here: a status of 'GrpcOk' carries no information, and any other
-- status will result in a 'GrpcException'. Calling 'recvOutput' again after
-- receiving the trailers is a bug and results in a 'RecvAfterFinal' exception.
recvOutput :: forall rpc m.
     (MonadIO m, HasCallStack)
  => Call rpc
  -> m (StreamElem (ResponseTrailingMetadata rpc) (Output rpc))
recvOutput call@Call{} = liftIO $ do
    streamElem <- recvOutputWithMeta call
    bitraverse (responseTrailingMetadata call) (return . snd) streamElem

-- | Receive an output from the peer, if one exists
--
-- If this is the final output, the /next/ call to 'recvNextOutputElem' will
-- return 'NoNextElem'; see also 'Network.GRPC.Server.recvNextInputElem' for
-- detailed discussion.
recvNextOutputElem ::
     (MonadIO m, HasCallStack)
  => Call rpc -> m (NextElem (Output rpc))
recvNextOutputElem =
      fmap (either (const NoNextElem) (NextElem . snd))
    . recvEither

-- | Generalization of 'recvOutput', providing additional meta-information
--
-- This returns the full set of trailers, /even if those trailers indicate a
-- gRPC failure, or if any trailers fail to parse/. Put another way, gRPC
-- failures are returned as values here, rather than throwing an exception.
--
-- Most applications will never need to use this function.
--
-- See also 'Network.GRPC.Server.recvInputWithMeta'.
recvOutputWithMeta :: forall rpc m.
     (MonadIO m, HasCallStack)
  => Call rpc
  -> m (StreamElem ProperTrailers' (InboundMeta, Output rpc))
recvOutputWithMeta = recvBoth

-- | The initial metadata that was included in the response headers
--
-- The server can send two sets of metadata: an initial set of type
-- 'ResponseInitialMetadata' when it first initiates the response, and then a
-- final set of type 'ResponseTrailingMetadata' after the final message (see
-- 'recvOutput').
--
-- It is however possible for the server to send only a /single/ set; this is
-- the gRPC \"Trailers-Only\" case. The server can choose to do so when it knows
-- it will not send any messages; in this case, the initial response metadata is
-- fact of type 'ResponseTrailingMetadata' instead. The 'ResponseMetadata' type
-- distinguishes between these two cases.
--
-- If the \"Trailers-Only\" case can be ruled out (that is, if it would amount
-- to a protocol error), you can use 'recvResponseInitialMetadata' instead.
--
-- This can block: we need to wait until we receive the metadata. The precise
-- communication pattern will depend on the specifics of each server:
--
-- * It might be necessary to send one or more inputs to the server before it
--   returns any replies.
-- * The response metadata /will/ be available before the first output from the
--   server, and may indeed be available /well/ before.
recvResponseMetadata :: forall rpc m.
     MonadIO m
  => Call rpc -> m (ResponseMetadata rpc)
recvResponseMetadata call@Call{} = liftIO $
    recvInitialResponse call >>= aux
  where
    aux ::
         Either (TrailersOnly'    HandledSynthesized)
                (ResponseHeaders' HandledSynthesized)
      -> IO (ResponseMetadata rpc)
    aux (Left trailers) =
        case grpcClassifyTermination properTrailers of
          Left exception ->
            throwM exception
          Right terminatedNormally -> do
            ResponseTrailingMetadata <$>
              parseMetadata (grpcTerminatedMetadata terminatedNormally)
      where
        properTrailers = trailersOnlyToProperTrailers' trailers
    aux (Right headers) =
        ResponseInitialMetadata <$>
          parseMetadata (customMetadataMapToList $ responseMetadata headers)

-- | Return the initial response from the server
--
-- This is a low-level function, and generalizes 'recvResponseInitialMetadata'.
-- If the server returns a gRPC error, that will be returned as a value here
-- rather than thrown as an exception.
--
-- Most applications will never need to use this function.
recvInitialResponse :: forall rpc m.
     MonadIO m
  => Call rpc
  -> m ( Either (TrailersOnly'    HandledSynthesized)
                 (ResponseHeaders' HandledSynthesized)
        )
recvInitialResponse Call{callChannel} = liftIO $
    fmap inbHeaders <$> Session.getInboundHeaders callChannel

{-------------------------------------------------------------------------------
  Protocol specific wrappers
-------------------------------------------------------------------------------}

-- | Send the next input
--
-- If this is the last input, you should call 'sendFinalInput' instead.
sendNextInput :: MonadIO m => Call rpc -> Input rpc -> m ()
sendNextInput call = sendInput call . StreamElem

-- | Send final input
--
-- For some servers it is important that the client marks the final input /when
-- it is sent/. If you really want to send the final input and separately tell
-- the server that no more inputs will be provided, use 'sendEndOfInput' (or
-- 'sendInput').
sendFinalInput ::
     MonadIO m
  => Call rpc
  -> Input rpc
  -> m ()
sendFinalInput call input =
    sendInput call (FinalElem input NoMetadata)

-- | Indicate that there are no more inputs
--
-- See 'sendFinalInput' for additional discussion.
sendEndOfInput :: MonadIO m => Call rpc -> m ()
sendEndOfInput call = sendInput call $ NoMoreElems NoMetadata

-- | Receive /initial/ metadata
--
-- This is a specialization of 'recvResponseMetadata' which can be used if a use
-- of \"Trailers-Only\" amounts to a protocol error; if the server /does/ use
-- \"Trailers-Only\", this throws a 'ProtoclException'
-- ('UnexpectedTrailersOnly').
recvResponseInitialMetadata :: forall rpc m.
     MonadIO m
  => Call rpc
  -> m (ResponseInitialMetadata rpc)
recvResponseInitialMetadata call@Call{} = liftIO $ do
    md <- recvResponseMetadata call
    case md of
      ResponseInitialMetadata md' ->
        return md'
      ResponseTrailingMetadata md' ->
        err $ UnexpectedTrailersOnly md'
  where
    err :: ProtocolException rpc -> IO a
    err = throwM . ProtocolException

-- | Receive the next output
--
-- Throws 'ProtocolException' if there are no more outputs.
recvNextOutput :: forall rpc m.
     (MonadIO m, HasCallStack)
  => Call rpc -> m (Output rpc)
recvNextOutput call@Call{} = liftIO $ do
    mOut <- recvEither call
    case mOut of
      Left trailers -> do
        trailingMetadata <- responseTrailingMetadata call trailers
        err $ TooFewOutputs @rpc trailingMetadata
      Right (_env, out) ->
        return out
  where
    err :: ProtocolException rpc -> IO a
    err = throwM . ProtocolException

-- | Receive output, which we expect to be the /final/ output
--
-- Throws 'ProtocolException' if the output we receive is not final.
--
-- NOTE: If the first output we receive from the server is not marked as final,
-- we will block until we receive the end-of-stream indication.
recvFinalOutput :: forall rpc m.
     (MonadIO m, HasCallStack)
  => Call rpc
  -> m (Output rpc, ResponseTrailingMetadata rpc)
recvFinalOutput call@Call{} = liftIO $ do
    out1 <- recvOutput call
    case out1 of
      NoMoreElems    ts -> err $ TooFewOutputs @rpc ts
      FinalElem  out ts -> return (out, ts)
      StreamElem out    -> do
        out2 <- recvOutput call
        case out2 of
          NoMoreElems ts    -> return (out, ts)
          FinalElem  out' _ -> err $ TooManyOutputs @rpc out'
          StreamElem out'   -> err $ TooManyOutputs @rpc out'
  where
    err :: ProtocolException rpc -> IO a
    err = throwM . ProtocolException

-- | Receive trailers
--
-- Throws 'ProtocolException' if we received an output.
recvTrailers :: forall rpc m.
     (MonadIO m, HasCallStack)
  => Call rpc -> m (ResponseTrailingMetadata rpc)
recvTrailers call@Call{} = liftIO $ do
    mOut <- recvOutput call
    case mOut of
      NoMoreElems     ts -> return ts
      FinalElem  out _ts -> err $ TooManyOutputs @rpc out
      StreamElem out     -> err $ TooManyOutputs @rpc out
  where
    err :: ProtocolException rpc -> IO a
    err = throwM . ProtocolException

{-------------------------------------------------------------------------------
  Internal auxiliary: deal with final message
-------------------------------------------------------------------------------}

recvBoth :: forall rpc m.
     (HasCallStack, MonadIO m)
  => Call rpc
  -> m (StreamElem ProperTrailers' (InboundMeta, Output rpc))
recvBoth Call{callChannel} = liftIO $
    flatten <$> Session.recvBoth callChannel
  where
    -- We lose type information here: Trailers-Only is no longer visible
    flatten ::
         Either
           (TrailersOnly' HandledSynthesized)
           (StreamElem ProperTrailers' (InboundMeta, Output rpc))
      -> StreamElem ProperTrailers' (InboundMeta, Output rpc)
    flatten (Left trailersOnly) =
        NoMoreElems $ trailersOnlyToProperTrailers' trailersOnly
    flatten (Right streamElem) =
        streamElem

recvEither :: forall rpc m.
     (HasCallStack, MonadIO m)
  => Call rpc
  -> m (Either ProperTrailers' (InboundMeta, Output rpc))
recvEither Call{callChannel} = liftIO $
    flatten <$> Session.recvEither callChannel
  where
    flatten ::
         Either
           (TrailersOnly' HandledSynthesized)
           (Either ProperTrailers' (InboundMeta, Output rpc))
      -> Either ProperTrailers' (InboundMeta, Output rpc)
    flatten (Left trailersOnly) =
        Left $ trailersOnlyToProperTrailers' trailersOnly
    flatten (Right (Left properTrailers)) =
        Left $ properTrailers
    flatten (Right (Right msg)) =
        Right $ msg

responseTrailingMetadata ::
     MonadIO m
  => Call rpc
  -> ProperTrailers' -> m (ResponseTrailingMetadata rpc)
responseTrailingMetadata Call{} trailers = liftIO $
    case grpcClassifyTermination trailers of
      Right terminatedNormally -> do
        parseMetadata $ grpcTerminatedMetadata terminatedNormally
      Left exception ->
        throwM exception


-- | Forget that we are in the Trailers-Only case
--
-- Error handling is a bit subtle here. If we are in the Trailers-Only case:
--
-- * Any synthesized errors have already been dealt with
--   (the type @TrailersOnly' Void@ tell us this)
-- * If 'connVerifyHeaders' is enabled, /all/ trailers have been verified
--   (unfortunately this we cannot see from type).
--
-- This means that we might only have a (non-synthesized) error for the
-- content-type if 'connVerifyHeaders' is /not/ enabled; since we are not
-- actually interested in the content-type here, we can therefore just ignore
-- these errors.
trailersOnlyToProperTrailers' ::
     TrailersOnly' HandledSynthesized
  -> ProperTrailers'
trailersOnlyToProperTrailers' =
      fst                                   -- justified by the comment above
    . trailersOnlyToProperTrailers
    . HKD.map (first $ mapSynthesized handledSynthesized) -- simple injection
