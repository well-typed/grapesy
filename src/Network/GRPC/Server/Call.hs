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

    -- ** Protocol specific wrappers
  , recvFinalInput
  , recvNextInput
  , sendFinalOutput
  , sendNextOutput
  , sendTrailers
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Tracer
import Data.Bifunctor

import Network.GRPC.Common.Compression (Compression)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Exceptions
import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Server.Connection (Connection)
import Network.GRPC.Server.Connection qualified as Connection
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Server.Session
import Network.GRPC.Spec
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Common.Compression qualified as Compression

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Call rpc = IsRPC rpc => Call {
      callSession :: ServerSession rpc
    , callChannel :: Session.Channel (ServerSession rpc)
    }

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

-- | Accept incoming call
acceptCall :: forall rpc.
     IsRPC rpc
  => Connection
  -> ([CustomMetadata] -> IO [CustomMetadata])
  -> IO (Call rpc)
acceptCall conn mkResponseMetadata = do
    callChannel <-
      Session.initiateResponse
        callSession
        tracer
        (Connection.connectionToClient conn)
        mkOutboundHeaders
    return Call{callSession, callChannel}
  where
    callSession :: ServerSession rpc
    callSession = ServerSession {
          serverCompression = compr
        }

    compr :: Compr.Negotation
    compr = Context.serverCompression $ Context.params $ Connection.context conn

    tracer :: Tracer IO (Session.DebugMsg (ServerSession rpc))
    tracer =
        contramap (Context.PeerDebugMsg @rpc) $
          Context.serverDebugTracer $ Context.params (Connection.context conn)

    mkOutboundHeaders ::
             Session.FlowStart (ServerInbound  rpc)
      -> IO (Session.FlowStart (ServerOutbound rpc))
    mkOutboundHeaders start = do
        responseMetadata <- mkResponseMetadata $ requestMetadata inboundHeaders

        cOut :: Compression <-
          case requestAcceptCompression inboundHeaders of
             Nothing   -> return Compression.identity
             Just cids ->
               -- If the requests explicitly lists compression algorithms, and
               -- that list does /not/ include @identity@, then we should not
               -- default to 'Compression.identity', even if all other
               -- algorithms are unsupported. This gives the client the option
               -- to /insist/ on compression.
               case Compression.choose compr cids of
                 Right c   -> return c
                 Left  err -> throwIO err

        return $ Session.FlowStartRegular $ OutboundHeaders {
            outHeaders = ResponseHeaders {
                responseCompression       = Just $ Compr.compressionId cOut
              , responseAcceptCompression = Just $ Compr.offer compr
              , responseMetadata          = responseMetadata
              }
          , outCompression = cOut
          }
      where
        inboundHeaders :: RequestHeaders
        inboundHeaders =
            case start of
              Session.FlowStartRegular      headers -> inbHeaders headers
              Session.FlowStartTrailersOnly headers ->            headers

{-------------------------------------------------------------------------------
  Open (ongoing) call
-------------------------------------------------------------------------------}

-- | Receive RPC input
--
-- We do not return trailers, since gRPC does not support sending trailers from
-- the client to the server (only from the server to the client).
recvInput :: forall rpc. Call rpc -> STM (StreamElem () (Input rpc))
recvInput Call{callChannel} =
    first ignoreTrailersOnly <$> Session.recv callChannel
  where
    ignoreTrailersOnly :: Either RequestHeaders () -> ()
    ignoreTrailersOnly _ = ()

sendOutput :: Call rpc -> StreamElem [CustomMetadata] (Output rpc) -> STM ()
sendOutput Call{callChannel} = Session.send callChannel . first mkTrailers
  where
    mkTrailers :: [CustomMetadata] -> ProperTrailers
    mkTrailers metadata = ProperTrailers{
          trailerGrpcStatus  = GrpcOk
        , trailerGrpcMessage = Nothing
        , trailerMetadata    = metadata
        }

{-------------------------------------------------------------------------------
  Protocol specific wrappers
-------------------------------------------------------------------------------}

-- | Receive input, which we expect to be the /final/ input
--
-- Throws 'ProtocolException' if the input we receive is not final.
--
-- NOTE: If the first input we receive from the client is not marked as final,
-- we will block until we receive the end-of-stream indication.
recvFinalInput :: forall rpc. Call rpc -> IO (Input rpc)
recvFinalInput call@Call{} = do
    inp1 <- atomically $ recvInput call
    case inp1 of
      NoMoreElems    () -> throwIO $ TooFewInputs @rpc
      FinalElem  inp () -> return inp
      StreamElem inp    -> do
        inp2 <- atomically $ recvInput call
        case inp2 of
          NoMoreElems ()    -> return inp
          FinalElem  inp' _ -> throwIO $ TooManyInputs @rpc inp'
          StreamElem inp'   -> throwIO $ TooManyInputs @rpc inp'

recvNextInput :: Call rpc -> IO (StreamElem () (Input rpc))
recvNextInput call = atomically $ recvInput call

sendFinalOutput :: Call rpc -> (Output rpc, [CustomMetadata]) -> IO ()
sendFinalOutput call = atomically . sendOutput call . uncurry FinalElem

-- | Send the next output
--
-- If this is the last output, you should call 'sendTrailers' after.
sendNextOutput :: Call rpc -> Output rpc -> IO ()
sendNextOutput call = atomically . sendOutput call . StreamElem

-- | Send trailers
--
-- This tells the client that there will be no more outputs. You should call
-- this even when there /are/ no trailers (just supply the empty list).
sendTrailers :: Call rpc -> [CustomMetadata] -> IO ()
sendTrailers call = atomically . sendOutput call . NoMoreElems
