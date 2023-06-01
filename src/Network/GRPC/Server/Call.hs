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
  , recvOnlyInput
  , recvNextInput
  , sendOnlyOutput
  , sendNextOutput
  , sendTrailers
  ) where

import Control.Concurrent.STM
import Control.Tracer
import Data.Bifunctor
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Common.Compression (Compression)
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Common.Exceptions
import Network.GRPC.Server.Connection (Connection)
import Network.GRPC.Server.Connection qualified as Connection
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Spec
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.LengthPrefixed qualified as LengthPrefixed
import Network.GRPC.Spec.Response
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Peer (Peer)
import Network.GRPC.Util.Peer qualified as Peer
import Network.GRPC.Util.StreamElem

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Call rpc = IsRPC rpc => Call {
      callRPC  :: rpc
    , callPeer :: Peer (Output rpc) (Input rpc)
    }

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

-- | Accept incoming call
--
acceptCall :: forall rpc.
     IsRPC rpc
  => Connection
  -> rpc
  -> ([CustomMetadata] -> IO [CustomMetadata])
  -> IO (Call rpc)
acceptCall conn rpc getResponseMetadata = do

    -- TODO: Parse and deal with the non-pseudo response header
    --
    -- For content types that aren't application/grpc (with or without a
    -- subtype), we should throw HTTP 415 Unsupported Media Type response
    let requestMetadata = []

    -- TODO: compression
    let cIn, cOut :: Compression
        cIn  = Compression.identity
        cOut = Compression.identity

    -- Response headers
    responseMetadata <- getResponseMetadata requestMetadata
    let responseHeaders = ResponseHeaders {
            responseCompression       = Nothing -- TODO
          , responseAcceptCompression = Nothing -- TODO
          , responseMetadata
          }

    let peerConfig :: Peer.Config () (Output rpc) (Input rpc)
        peerConfig = Peer.Config {
              serializeOutbound =        LengthPrefixed.buildOutput rpc cOut
            , parseInbound      = \() -> LengthPrefixed.parseInput  rpc cIn
            }

    callPeer <-
      Peer.acceptClient
        tracer
        (Connection.request conn)
        (Connection.respond conn)
        peerConfig
        Peer.Client {
            responseStatus  = HTTP.ok200
          , responseHeaders = buildHeaders rpc responseHeaders
          }

    return Call{callRPC = rpc, callPeer}
  where
    tracer :: Tracer IO (Peer.DebugMsg (Output rpc) (Input rpc))
    tracer =
        contramap (SomeRPC . Context.PeerDebugMsg @rpc) $
          Context.serverTracer $ Context.params (Connection.context conn)

{-------------------------------------------------------------------------------
  Open (ongoing) call
-------------------------------------------------------------------------------}

-- | Receive RPC input
--
-- We do not return trailers, since gRPC does not support sending trailers from
-- the client to the server (only from the server to the client).
recvInput :: forall rpc. Call rpc -> STM (StreamElem () (Input rpc))
recvInput Call{callPeer} = first (const ()) <$> Peer.recv callPeer

sendOutput :: Call rpc -> StreamElem [CustomMetadata] (Output rpc) -> STM ()
sendOutput Call{callPeer} = Peer.send callPeer . first mkTrailers
  where
    mkTrailers :: [CustomMetadata] -> [HTTP.Header]
    mkTrailers metadata = buildTrailers $ Trailers {
          trailerGrpcStatus  = GrpcOk
        , trailerGrpcMessage = Nothing
        , trailerMetadata    = metadata
        }

{-------------------------------------------------------------------------------
  Protocol specific wrappers
-------------------------------------------------------------------------------}

-- | Receive input, which we expect to be the /only/ input
--
-- Throws 'ProtocolException' if the client does not send precisely one input.
recvOnlyInput :: Call rpc -> IO (Input rpc)
recvOnlyInput call@Call{callRPC} = do
    inp1 <- atomically $ recvInput call
    case inp1 of
      NoMoreElems    () -> protocolException callRPC TooFewInputs
      FinalElem  inp () -> return inp
      StreamElem inp    -> do
        inp2 <- atomically $ recvInput call
        case inp2 of
          NoMoreElems ()    -> return inp
          FinalElem  inp' _ -> protocolException callRPC $ TooManyInputs inp'
          StreamElem inp'   -> protocolException callRPC $ TooManyInputs inp'

recvNextInput :: Call rpc -> IO (StreamElem () (Input rpc))
recvNextInput call = atomically $ recvInput call

sendOnlyOutput :: Call rpc -> (Output rpc, [CustomMetadata]) -> IO ()
sendOnlyOutput call = atomically . sendOutput call . uncurry FinalElem

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
