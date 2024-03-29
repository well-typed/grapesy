{-# LANGUAGE OverloadedStrings #-}

-- | Open (ongoing) RPC call
--
-- Intended for unqualified import.
module Network.GRPC.Client.Call (
    -- * Construction
    withRPC

    -- * Open (ongoing) call
  , sendInput
  , recvOutput
  , recvResponseInitialMetadata

    -- ** Protocol specific wrappers
  , sendNextInput
  , sendFinalInput
  , sendEndOfInput
  , recvNextOutput
  , recvFinalOutput
  , recvTrailers

    -- ** Repeated send/recv
  , sendAllInputs
  , recvAllOutputs

    -- ** Low-level\/specialized API
  , sendInputWithEnvelope
  , recvOutputWithEnvelope
  , recvInitialResponse
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bitraversable
import Data.Default
import Data.Maybe (isJust)
import Data.Proxy
import Data.Text qualified as Text
import GHC.Stack

import Network.GRPC.Client.Connection (Connection, Call(..))
import Network.GRPC.Client.Connection qualified as Connection
import Network.GRPC.Client.Session
import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Spec
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Util.Thread qualified as Thread

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

-- | Scoped RPC call
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
--
-- If there are still /inbound/ messages upon leaving the scope of 'withRPC' no
-- exception is raised (but the call is nonetheless still closed, and the server
-- handler will be informed that the client has disappeared).
withRPC :: forall m rpc a.
     (MonadMask m, MonadIO m, SupportsClientRpc rpc, HasCallStack)
  => Connection -> CallParams rpc -> Proxy rpc -> (Call rpc -> m a) -> m a
withRPC conn callParams proxy k = fmap fst $
      generalBracket
        (liftIO $ Connection.startRPC conn proxy callParams)
        closeRPC
        k
  where
    closeRPC :: Call rpc -> ExitCase a -> m ()
    closeRPC call exitCase = liftIO $ do
        mException <- liftIO $ Session.close (callChannel call) exitCase
        case mException of
          Nothing -> return ()
          Just ex ->
            case fromException ex of
              Nothing        -> throwM ex
              Just discarded -> throwCancelled call discarded

    -- The spec mandates that when a client cancels a request (which in grapesy
    -- means exiting the scope of withRPC), the client receives a CANCELLED
    -- exception. We need to deal with the edge case mentioned above, however:
    -- the server might have already closed the connection. The client must have
    -- evidence that this is the case, which could mean one of two things:
    --
    -- o The received the final message from the server
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
    -- Note that it /not/ sufficient to check if the inbound thread has
    -- terminated: we might have received the final message, but the thread
    -- might still be /about/ to terminate, but not /actually/ have terminated.
    --
    -- See also:
    --
    -- o <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#cancel_after_begin>
    -- o <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#cancel_after_first_response>
    throwCancelled :: Call rpc -> ChannelDiscarded -> IO ()
    throwCancelled Call{callChannel} (ChannelDiscarded cs) = do
        mRecvFinal  <- atomically $
          readTVar $ Session.channelRecvFinal callChannel
        mTerminated <- atomically $
          Thread.hasThreadTerminated $ Session.channelInbound callChannel
        let serverClosed = isJust mRecvFinal || isJust mTerminated

        unless serverClosed $
          throwM $ GrpcException {
              grpcError         = GrpcCancelled
            , grpcErrorMessage  = Just $ mconcat [
                                      "Channel discarded by client at "
                                    , Text.pack $ prettyCallStack cs
                                    ]
            , grpcErrorMetadata = []
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
sendInput call = sendInputWithEnvelope call . fmap (def,)

-- | Generalization of 'sendInput', providing additional control
--
-- See also 'Network.GRPC.Server.sendOutputWithEnvelope'.
--
-- Most applications will never need to use this function.
sendInputWithEnvelope ::
     (HasCallStack, MonadIO m)
  => Call rpc
  -> StreamElem NoMetadata (OutboundEnvelope, Input rpc)
  -> m ()
sendInputWithEnvelope Call{callChannel} msg = liftIO $ do
    Session.send callChannel msg

    -- This should be called before exiting the scope of 'withRPC'.
    StreamElem.whenDefinitelyFinal msg $ \_ ->
      void $ Session.waitForOutbound callChannel

-- | Receive an output from the peer
--
-- After the final 'Output', you will receive any custom metadata (application
-- defined trailers) that the server returns. We do /NOT/ include the
-- 'GrpcStatus' here: a status of 'GrpcOk' carries no information, and any other
-- status will result in a 'GrpcException'. Calling 'recvOutput' again after
-- receiving the trailers is a bug and results in a 'RecvAfterFinal' exception.
recvOutput :: forall m rpc.
     (MonadIO m, HasCallStack)
  => Call rpc
  -> m (StreamElem (ResponseTrailingMetadata rpc) (Output rpc))
recvOutput call@Call{} = liftIO $
    recvOutputWithEnvelope call >>= bitraverse aux (return . snd)
  where
    aux :: ProperTrailers -> IO (ResponseTrailingMetadata rpc)
    aux trailers =
        case grpcClassifyTermination trailers of
          Right terminatedNormally -> do
            parseMetadata $ grpcTerminatedMetadata terminatedNormally
          Left exception ->
            throwM exception

-- | Generalization of 'recvOutput', providing additional meta-information
--
-- This returns the full set of trailers, /even if those trailers indicate
-- a gRPC failure/. Put another way, gRPC failures are returned as values here,
-- rather than throwing an exception.
--
-- Most applications will never need to use this function.
--
-- See also 'Network.GRPC.Server.recvInputWithEnvelope'.
recvOutputWithEnvelope ::
     (MonadIO m, HasCallStack)
  => Call rpc
  -> m (StreamElem ProperTrailers (InboundEnvelope, Output rpc))
recvOutputWithEnvelope Call{callChannel} = liftIO $
    first (either (fst . trailersOnlyToProperTrailers) id) <$>
      Session.recv callChannel

-- | The initial metadata that was included in the response headers
--
-- The server can send two sets of metadata: an initial set of type
-- @ResponseInitialMetadata@ when it first initiates the response, and then a
-- final set of type @ResponseTrailingMetadata@ after the final message (see
-- 'recvOutput'). See also 'HasCustomMetadata'.
--
-- It is however possible for the server to send only a /single/ set; this is
-- the gRPC \"Trailers-Only\" case. The server can choose to do so when it knows
-- it will not send any messages; in this case, the initial response metadata is
-- fact of type @ResponseTrailingMetadata@ instead. The 'ResponseMetadata' type
-- distinguishes between these two cases.
--
-- This can block: we need to wait until we receive the metadata. The precise
-- communication pattern will depend on the specifics of each server:
--
-- * It might be necessary to send one or more inputs to the server before it
--   returns any replies.
-- * The response metadata /will/ be available before the first output from the
--   server, and may indeed be available /well/ before.
recvResponseInitialMetadata :: forall rpc. Call rpc -> IO (ResponseMetadata rpc)
recvResponseInitialMetadata call@Call{} =
    recvInitialResponse call >>= aux
  where
    aux :: Either TrailersOnly ResponseHeaders -> IO (ResponseMetadata rpc)
    aux (Left trailers) =
        case grpcClassifyTermination properTrailers of
          Left exception ->
            throwM exception
          Right terminatedNormally -> do
            ResponseTrailingMetadata <$>
              parseMetadata (grpcTerminatedMetadata terminatedNormally)
      where
        (properTrailers, _contentType) = trailersOnlyToProperTrailers trailers
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
recvInitialResponse ::
     Call rpc
  -> IO (Either TrailersOnly ResponseHeaders)
recvInitialResponse Call{callChannel} =
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

-- | Receive the next output
--
-- Throws 'ProtocolException' if there are no more outputs. Discards metadata.
recvNextOutput :: forall m rpc.
     (MonadIO m, HasCallStack)
  => Call rpc -> m (Output rpc)
recvNextOutput call@Call{} = liftIO $ do
    mOut <- recvOutput call
    case mOut of
      NoMoreElems     ts -> err $ TooFewOutputs @rpc ts
      StreamElem out     -> return out
      FinalElem  out _ts -> return out
  where
    err :: ProtocolException rpc -> IO a
    err = throwM . ProtocolException

-- | Receive output, which we expect to be the /final/ output
--
-- Throws 'ProtocolException' if the output we receive is not final.
--
-- NOTE: If the first output we receive from the server is not marked as final,
-- we will block until we receive the end-of-stream indication.
recvFinalOutput :: forall m rpc.
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
recvTrailers :: forall m rpc.
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
  Repeated send/recv

  These are primarily useful for implementing streaming clients.
-------------------------------------------------------------------------------}

-- | Send all inputs returned by the specified action
--
-- Terminates after the action returns 'FinalElem' or 'NoMoreElems'
sendAllInputs :: forall m rpc.
     MonadIO m
  => Call rpc
  -> m (StreamElem NoMetadata (Input rpc))
  -> m ()
sendAllInputs call produceInput = loop
  where
    loop :: m ()
    loop = do
        inp <- produceInput
        sendInput call inp
        case inp of
          StreamElem{}  -> loop
          FinalElem{}   -> return ()
          NoMoreElems{} -> return ()

recvAllOutputs :: forall m rpc.
     MonadIO m
  => Call rpc
  -> (Output rpc -> m ())
  -> m (ResponseTrailingMetadata rpc)
recvAllOutputs call processOutput = loop
  where
    loop :: m (ResponseTrailingMetadata rpc)
    loop = do
        mOut <- recvOutput call
        case mOut of
          StreamElem out -> do
            processOutput out
            loop
          NoMoreElems trailers ->
            return trailers
          FinalElem out trailers -> do
            processOutput out
            return trailers
