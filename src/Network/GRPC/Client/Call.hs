-- | Open (ongoing) RPC call
--
-- Intended for unqualified import.
module Network.GRPC.Client.Call (
    -- * Definition
    Call -- opaque

    -- * Construction
  , initiateCall
  , abortCall

    -- * Open (ongoing) call
  , sendInput
  , recvOutput
  , recvResponseMetadata

    -- ** Protocol specific wrappers
  , sendFinalInput
  , sendAllInputs
  , recvFinalOutput
  , recvAllOutputs
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class
import Control.Tracer
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import GHC.Stack

import Network.GRPC.Client.Connection (Connection, ConnParams (..))
import Network.GRPC.Client.Connection qualified as Connection
import Network.GRPC.Client.Meta qualified as Meta
import Network.GRPC.Client.Session
import Network.GRPC.Common.Compression (Compression(..))
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Common.Exceptions
import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Spec
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Session qualified as Session

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | State of the call
--
-- This type is kept abstract (opaque) in the public facing API.
data Call rpc = IsRPC rpc => Call {
      callSession :: ClientSession rpc
    , callChannel :: Session.Channel (ClientSession rpc)
    }

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

initiateCall :: forall rpc.
     IsRPC rpc
  => Connection -> CallParams -> IO (Call rpc)
initiateCall conn callParams = do
    cOut <-
      Meta.outboundCompression <$> Connection.currentMeta conn
    callChannel <-
      Session.initiateRequest
        callSession
        tracer
        (Connection.connectionToServer conn)
        OutboundHeaders {
            outHeaders     = requestHeaders cOut
          , outCompression = fromMaybe Compression.identity cOut
          }
    return Call{callSession, callChannel}
  where
    callSession :: ClientSession rpc
    callSession = ClientSession {
          clientCompression = connCompression $ Connection.params conn
        , clientUpdateMeta  = Connection.updateMeta conn
        }

    requestHeaders :: Maybe Compression -> RequestHeaders
    requestHeaders cOut = RequestHeaders{
          requestTimeout =
            asum [
                callTimeout callParams
              , connDefaultTimeout $ Connection.params conn
              ]
        , requestMetadata =
            callRequestMetadata callParams
        , requestCompression =
            compressionId <$> cOut
        , requestAcceptCompression = Just $
            Compression.offer $
              connCompression $ Connection.params conn
        }

    tracer :: Tracer IO (Session.DebugMsg (ClientSession rpc))
    tracer =
        contramap (Connection.PeerDebugMsg @rpc) $
          connDebugTracer (Connection.params conn)

{-------------------------------------------------------------------------------
  Closing an open RPC
-------------------------------------------------------------------------------}

-- | Abort an open RPC call
--
-- TODO: Docs.
abortCall :: Exception e => Call rpc -> HasCallStack => e -> IO ()
abortCall = Session.close . callChannel

{-------------------------------------------------------------------------------
  Open (ongoing) call
-------------------------------------------------------------------------------}

-- | Send an input to the peer
--
-- This lives in @STM@ for improved composability. For example, if the peer is
-- currently busy then 'sendInput' will block, but you can then use 'orElse' to
-- provide an alternative codepath.
--
-- Calling 'sendInput' again after sending the final message is a bug.
--
-- WARNING: Sending multiple messages on the same call within the same STM
-- transaction will deadlock; you should enqueue them separately.
--
-- TODO: We should have a way to detect this and throw an exception.
sendInput :: HasCallStack => Call rpc -> StreamElem () (Input rpc) -> STM ()
sendInput = Session.send . callChannel

-- | Receive an output from the peer
--
-- This lives in @STM@ for improved compositionality. For example, you can wait
-- on multiple clients and see which one responds first.
--
-- After the final 'Output', you will receive any 'CustomMetadata' (application
-- defined trailers) that the server returns. We do /NOT/ include the
-- 'GrpcStatus' here: a status of 'GrpcOk' carries no information, and any other
-- status will result in a 'GrpcException'. Calling 'recvOutput' again after
-- receiving the trailers is a bug and results in a 'RecvAfterFinal' exception.
recvOutput :: Call rpc -> STM (StreamElem [CustomMetadata] (Output rpc))
recvOutput = Session.recv . callChannel

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
    responseMetadata . inbHeaders <$> Session.getInboundHeaders callChannel

{-------------------------------------------------------------------------------
  Protocol specific wrappers
-------------------------------------------------------------------------------}

sendFinalInput ::
     MonadIO m
  => Call rpc
  -> Input rpc
  -> m ()
sendFinalInput call input = liftIO $
    atomically $ sendInput call (FinalElem input ())

sendAllInputs :: forall m rpc.
     MonadIO m
  => Call rpc
  -> m (StreamElem () (Input rpc))
  -> m ()
sendAllInputs call produceInput = loop
  where
    loop :: m ()
    loop = do
        inp <- produceInput
        liftIO $ atomically $ sendInput call inp
        case StreamElem.definitelyFinal inp of
          Nothing -> loop
          Just _  -> return ()

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
    out1 <- atomically $ recvOutput call
    case out1 of
      NoMoreElems    ts -> throwIO $ TooFewOutputs @rpc ts
      FinalElem  out ts -> return (out, ts)
      StreamElem out    -> do
        out2 <- atomically $ recvOutput call
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
        mOut <- liftIO $ atomically $ recvOutput call
        case mOut of
          StreamElem out -> do
            processOutput out
            loop
          NoMoreElems trailers ->
            return trailers
          FinalElem out trailers -> do
            processOutput out
            return trailers
