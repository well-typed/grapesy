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
  , sendFinalInput
  , sendAllInputs
  , recvFinalOutput
  , recvAllOutputs

    -- ** Low-level API
  , sendInputSTM
  , recvOutputSTM
  , waitForOutbound
  , isCallHealthy
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Proxy
import GHC.Stack

import Network.GRPC.Client.Connection (Connection, Call(..))
import Network.GRPC.Client.Connection qualified as Connection
import Network.GRPC.Client.Session
import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Spec
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Util.STM

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

-- | Scoped RPC call
--
-- The call is setup in the background, and might not yet have been established
-- when the body is run. If you want to be sure that the call has been setup,
-- you can call 'recvResponseMetadata'.
withRPC :: forall m rpc a.
     (MonadMask m, MonadIO m, IsRPC rpc)
  => Connection -> CallParams -> Proxy rpc -> (Call rpc -> m a) -> m a
withRPC conn callParams proxy k =
    (throwUnclean =<<) $
      generalBracket
        (liftIO $ Connection.startRPC conn proxy callParams)
        (\call -> liftIO . closeRPC call)
        k
  where
    throwUnclean :: (a, Maybe ChannelUncleanClose) -> m a
    throwUnclean (_, Just err) = throwM err
    throwUnclean (x, Nothing)  = return x

-- | Close an open RPC call
--
-- This is a low-level API; most users should use 'withRPC' instead.
--
-- See 'Session.close' for detailed discussion.
closeRPC ::
     HasCallStack
  => Call rpc -> ExitCase a -> IO (Maybe ChannelUncleanClose)
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
sendInput call msg = liftIO $ do
    atomically $ sendInputSTM call msg
    StreamElem.whenDefinitelyFinal msg $ \_ -> waitForOutbound call

-- | Receive an output from the peer
--
-- After the final 'Output', you will receive any 'CustomMetadata' (application
-- defined trailers) that the server returns. We do /NOT/ include the
-- 'GrpcStatus' here: a status of 'GrpcOk' carries no information, and any other
-- status will result in a 'GrpcException'. Calling 'recvOutput' again after
-- receiving the trailers is a bug and results in a 'RecvAfterFinal' exception.
recvOutput ::
     MonadIO m
  => Call rpc
  -> m (StreamElem [CustomMetadata] (Output rpc))
recvOutput call =
    liftIO $ atomically $ recvOutputSTM call

-- | The initial metadata that was included in the response headers
--
-- The server might send additional metadata after the final output; see
-- 'recvOutput'.
--
-- This lives in STM because it might block: we need to wait until we receive
-- the metadata. The precise communication pattern will depend on the specifics
-- of each server:
--
-- * It might be necessary to send one or more inputs to the server before it
--   returns any replies.
-- * The response metadata /will/ be available before the first output from the
--   server, and may indeed be available /well/ before.
recvResponseMetadata :: Call rpc -> STM [CustomMetadata]
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

-- | Send input
--
-- This is a low-level API; most users should use 'sendInput' instead.
--
-- The advantage of 'sendInputSTM' over 'sendInput' is the improved
-- compositionality of @STM@. For example, if the peer is currently busy then
-- 'sendInput' will block, but you can then use 'orElse' to provide an
-- alternative codepath.
--
-- If you choose to use 'sendInputSTM' over 'sendInput', you have some
-- responsibilities:
--
-- * You must call 'waitForOutbound' after sending the final message (and before
--   exiting the scope of 'withRPC').
-- * You should not enqueue multiple messages on the same call within the same
--   STM transaction; doing so will deadlock.
sendInputSTM ::
     HasCallStack
  => Call rpc
  -> StreamElem NoMetadata (Input rpc)
  -> STM ()
sendInputSTM = Session.send . callChannel

-- | Receive output
--
-- This is a low-level API; most users should use 'recvOutput' instead.
--
-- The improved compositionality of STM can for example be used to wait on
-- multiple clients and see which one responds first.
recvOutputSTM :: Call rpc -> STM (StreamElem [CustomMetadata] (Output rpc))
recvOutputSTM = fmap (first collapseTrailers) . Session.recv . callChannel
  where
    -- No difference between 'ProperTrailers' and 'TrailersOnly'
    collapseTrailers ::
         Either [CustomMetadata] [CustomMetadata]
      -> [CustomMetadata]
    collapseTrailers = either id id

-- | Wait for all outbound messages to have been processed
--
-- This should be called before exiting the scope of 'withRPC'. However,
-- 'sendOutput' will call this function when sending the final messages, so
-- you only need to worry about this if using 'sendOutputSTM'.
waitForOutbound :: Call rpc -> IO ()
waitForOutbound call = do
    void $ Session.waitForOutbound (callChannel call)

-- | Check if the connection is still OK
--
-- This is inherently non-deterministic: the connection to the server could have
-- been lost and we might not yet realize, it the connnection could be lost
-- straight after 'isCallHealthy' returns. Use with caution.
isCallHealthy :: Call rpc -> STM Bool
isCallHealthy = Session.isChannelHealthy . callChannel

{-------------------------------------------------------------------------------
  Protocol specific wrappers
-------------------------------------------------------------------------------}

sendFinalInput ::
     MonadIO m
  => Call rpc
  -> Input rpc
  -> m ()
sendFinalInput call input =
    sendInput call (FinalElem input NoMetadata)

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

-- | Receive output, which we expect to be the /final/ output
--
-- Throws 'ProtocolException' if the output we receive is not final.
--
-- NOTE: If the first output we receive from the server is not marked as final,
-- we will block until we receive the end-of-stream indication.
recvFinalOutput :: forall m rpc.
     MonadIO m
  => Call rpc
  -> m (Output rpc, [CustomMetadata])
recvFinalOutput call@Call{} = liftIO $ do
    out1 <- recvOutput call
    case out1 of
      NoMoreElems    ts -> throwM $ TooFewOutputs @rpc ts
      FinalElem  out ts -> return (out, ts)
      StreamElem out    -> do
        out2 <- recvOutput call
        case out2 of
          NoMoreElems ts    -> return (out, ts)
          FinalElem  out' _ -> throwIO $ TooManyOutputs @rpc out'
          StreamElem out'   -> throwIO $ TooManyOutputs @rpc out'

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
