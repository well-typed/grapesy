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
  -> rpc
  -> ([CustomMetadata] -> IO [CustomMetadata])
  -> IO (Call rpc)
acceptCall conn rpc mkResponseMetadata =
    Call callSession <$>
      Session.initiateResponse
        callSession
        tracer
        (Connection.connectionToClient conn)
        mkOutboundHeaders
  where
    callSession :: ServerSession rpc
    callSession = ServerSession {
        serverRPC         = rpc
      , serverCompression = compr
      }

    compr :: Compr.Negotation
    compr = Context.serverCompression $ Context.params $ Connection.context conn

    tracer :: Tracer IO (Session.DebugMsg (ServerSession rpc))
    tracer =
        contramap (SomeRPC . Context.PeerDebugMsg @rpc) $
          Context.serverTracer $ Context.params (Connection.context conn)

    mkOutboundHeaders ::
             Session.InboundHeaders  (ServerSession rpc)
      -> IO (Session.OutboundHeaders (ServerSession rpc))
    mkOutboundHeaders InboundHeaders{inbHeaders} = do
        responseMetadata <- mkResponseMetadata $ requestMetadata inbHeaders

        cOut :: Compression <-
          case requestAcceptCompression inbHeaders of
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

        return OutboundHeaders {
            outHeaders = ResponseHeaders {
                responseCompression       = Just $ Compr.compressionId cOut
              , responseAcceptCompression = Just $ Compr.offer compr
              , responseMetadata          = responseMetadata
              }
          , outCompression = cOut
          }

{-------------------------------------------------------------------------------
  Open (ongoing) call
-------------------------------------------------------------------------------}

-- | Receive RPC input
--
-- We do not return trailers, since gRPC does not support sending trailers from
-- the client to the server (only from the server to the client).
recvInput :: forall rpc. Call rpc -> STM (StreamElem () (Input rpc))
recvInput Call{callChannel} = Session.recv callChannel

sendOutput :: Call rpc -> StreamElem [CustomMetadata] (Output rpc) -> STM ()
sendOutput Call{callChannel} = Session.send callChannel . first mkTrailers
  where
    mkTrailers :: [CustomMetadata] -> Trailers
    mkTrailers metadata = Trailers {
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
recvOnlyInput call@Call{callSession = ServerSession{serverRPC}} = do
    inp1 <- atomically $ recvInput call
    case inp1 of
      NoMoreElems    () -> protocolException serverRPC TooFewInputs
      FinalElem  inp () -> return inp
      StreamElem inp    -> do
        inp2 <- atomically $ recvInput call
        case inp2 of
          NoMoreElems ()    -> return inp
          FinalElem  inp' _ -> protocolException serverRPC $ TooManyInputs inp'
          StreamElem inp'   -> protocolException serverRPC $ TooManyInputs inp'

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
