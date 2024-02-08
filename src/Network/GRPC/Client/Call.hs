-- | Open (ongoing) RPC call
--
-- Intended for unqualified import.
module Network.GRPC.Client.Call (
    -- * Construction
    withRPC

    -- * Open (ongoing) call
  , sendInput
  , recvOutput
  , recvResponseMetadata

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
  , isCallHealthy
  , sendInputWithEnvelope
  , recvOutputWithEnvelope
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Default
import Data.Proxy
import GHC.Stack

import Network.GRPC.Client.Connection (Connection, Call(..))
import Network.GRPC.Client.Connection qualified as Connection
import Network.GRPC.Client.Session
import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Spec
import Network.GRPC.Util.Session qualified as Session

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

-- | Scoped RPC call
--
-- The call is setup in the background, and might not yet have been established
-- when the body is run. If you want to be sure that the call has been setup,
-- you can call 'recvResponseMetadata'.
--
-- Will throw 'ChannelDiscarded' if the RPC call was not terminated cleanly
-- at the time of exiting the scope of 'withRPC'.
withRPC :: forall m rpc a.
     (MonadMask m, MonadIO m, IsRPC rpc, HasCallStack)
  => Connection -> CallParams -> Proxy rpc -> (Call rpc -> m a) -> m a
withRPC conn callParams proxy k =
    (throwUnclean =<<) $
      generalBracket
        (liftIO $ Connection.startRPC conn proxy callParams)
        (\call -> liftIO . closeRPC call)
        k
  where
    throwUnclean :: (a, Maybe SomeException) -> m a
    throwUnclean (_, Just err) = throwM err
    throwUnclean (x, Nothing)  = return x

-- | Close an open RPC call
--
-- This is a low-level API; most users should use 'withRPC' instead.
--
-- See 'Session.close' for detailed discussion.
closeRPC ::
     HasCallStack
  => Call rpc -> ExitCase a -> IO (Maybe SomeException)
closeRPC = Session.close . callChannel

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
-- After the final 'Output', you will receive any 'CustomMetadata' (application
-- defined trailers) that the server returns. We do /NOT/ include the
-- 'GrpcStatus' here: a status of 'GrpcOk' carries no information, and any other
-- status will result in a 'GrpcException'. Calling 'recvOutput' again after
-- receiving the trailers is a bug and results in a 'RecvAfterFinal' exception.
recvOutput ::
     (MonadIO m, HasCallStack)
  => Call rpc
  -> m (StreamElem [CustomMetadata] (Output rpc))
recvOutput = fmap (fmap snd) . recvOutputWithEnvelope

-- | Generalization of 'recvOutput', providing additional meta-information
--
-- See also 'Network.GRPC.Server.recvInputWithEnvelope'.
--
-- Most applications will never need to use this function.
recvOutputWithEnvelope ::
     (MonadIO m, HasCallStack)
  => Call rpc
  -> m (StreamElem [CustomMetadata] (InboundEnvelope, Output rpc))
recvOutputWithEnvelope Call{callChannel} = liftIO $
    first collapseTrailers <$> Session.recv callChannel
  where
    -- No difference between 'ProperTrailers' and 'TrailersOnly'
    collapseTrailers ::
         Either [CustomMetadata] [CustomMetadata]
      -> [CustomMetadata]
    collapseTrailers = either id id

-- | The initial metadata that was included in the response headers
--
-- The server might send additional metadata after the final output; see
-- 'recvOutput'.
--
-- This can block: we need to wait until we receive the metadata. The precise
-- communication pattern will depend on the specifics of each server:
--
-- * It might be necessary to send one or more inputs to the server before it
--   returns any replies.
-- * The response metadata /will/ be available before the first output from the
--   server, and may indeed be available /well/ before.
recvResponseMetadata :: Call rpc -> IO [CustomMetadata]
recvResponseMetadata Call{callChannel} =
    aux <$> Session.getInboundHeaders callChannel
  where
    aux ::
         Either [CustomMetadata] (Headers (ClientInbound rpc))
      -> [CustomMetadata]
    aux (Left trailersOnly) = trailersOnly
    aux (Right headers)     = responseMetadata $ inbHeaders headers

{-------------------------------------------------------------------------------
  Low-level API
-------------------------------------------------------------------------------}

-- | Check if the connection is still OK
--
-- This is inherently non-deterministic: the connection to the server could have
-- been lost and we might not yet realize, or the connnection could be lost
-- straight after 'isCallHealthy' returns. Use with caution.
isCallHealthy :: Call rpc -> STM Bool
isCallHealthy = Session.isChannelHealthy . callChannel

{-------------------------------------------------------------------------------
  Protocol specific wrappers
-------------------------------------------------------------------------------}

-- | Send the next input
--
-- If this is the last input, you should call 'sendFinalInput' instead.
sendNextInput :: Call rpc -> Input rpc -> IO ()
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
  -> m (Output rpc, [CustomMetadata])
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
  => Call rpc -> m [CustomMetadata]
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
  -> m [CustomMetadata]
recvAllOutputs call processOutput = loop
  where
    loop :: m [CustomMetadata]
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
