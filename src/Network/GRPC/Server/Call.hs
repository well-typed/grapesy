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

    -- ** Internal API
  , sendProperTrailers
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Tracer
import Data.Bifunctor
import Data.Text qualified as Text
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Common.Compression (Compression)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Common.Exceptions
import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Server.Connection (Connection)
import Network.GRPC.Server.Connection qualified as Connection
import Network.GRPC.Server.Context qualified as Context
import Network.GRPC.Server.Session
import Network.GRPC.Spec
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.Response qualified as Resp
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Session qualified as Session
import Network.GRPC.Util.Session.Server qualified as Server

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
acceptCall :: forall rpc. IsRPC rpc => Connection -> IO (Call rpc)
acceptCall conn = do
    callRequestMetadata  <- newEmptyTMVarIO
    callResponseMetadata <- newTVarIO []
    callResponseKickoff  <- newEmptyTMVarIO
    callChannel <-
      Session.initiateResponse
        callSession
        (contramap (Context.PeerDebugMsg @rpc) tracer)
        (Connection.connectionToClient conn)
        (    handle sendErrorResponse
           . mkOutboundHeaders
               callRequestMetadata
               callResponseMetadata
               callResponseKickoff
        )
    return Call{
        callSession
      , callChannel
      , callRequestMetadata
      , callResponseMetadata
      , callResponseKickoff
      }
  where
    callSession :: ServerSession rpc
    callSession = ServerSession {
          serverCompression = compr
        }

    compr :: Compr.Negotation
    compr = Context.serverCompression $ Context.params $ Connection.context conn

    tracer :: Tracer IO Context.ServerDebugMsg
    tracer = Context.serverDebugTracer $ Context.params (Connection.context conn)

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
        -- Make request metadata available (see 'getRequestMetadata')
        atomically $ putTMVar requestMetadataVar $
                       requestMetadata inboundHeaders

        -- Wait for kickoff (see 'Kickoff' for discussion)
        kickoff <- atomically $ readTMVar responseKickoffVar

        -- Get response metadata (see 'setResponseMetadata')
        responseMetadata <- atomically $ readTVar responseMetadataVar

        -- Session start
        case kickoff of
          KickoffRegular -> do
            cOut :: Compression <-
              case requestAcceptCompression inboundHeaders of
                 Nothing   -> return Compression.identity
                 Just cids ->
                   -- If the requests explicitly lists compression algorithms,
                   -- and that list does /not/ include @identity@, then we
                   -- should not default to 'Compression.identity', even if all
                   -- other algorithms are unsupported. This gives the client
                   -- the option to /insist/ on compression.
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
          KickoffTrailersOnly trailers ->
            return $ Session.FlowStartTrailersOnly trailers
      where
        inboundHeaders :: RequestHeaders
        inboundHeaders =
            case requestStart of
              Session.FlowStartRegular      headers -> inbHeaders headers
              Session.FlowStartTrailersOnly headers ->            headers

    -- | Send response when 'mkOutboundHeaders' fails
    sendErrorResponse :: SomeException -> IO a
    sendErrorResponse err = do
        traceWith tracer $ Context.AcceptCallFailed err
        Server.respond (Connection.connectionToClient conn) $
          HTTP2.responseNoBody
            HTTP.ok200 -- gRPC uses HTTP 200 even when there are gRPC errors
            (Resp.buildTrailersOnly $ TrailersOnly $ ProperTrailers {
                trailerGrpcStatus  = GrpcError GrpcUnknown
                -- TODO: Potential security concern here
                -- (showing the exception)?
              , trailerGrpcMessage = Just $ Text.pack $ show err
              , trailerMetadata    = []
              })
        throwIO err

{-------------------------------------------------------------------------------
  Open (ongoing) call
-------------------------------------------------------------------------------}

-- | Receive RPC input from the client
--
-- We do not return trailers, since gRPC does not support sending trailers from
-- the client to the server (only from the server to the client).
recvInput :: forall rpc. Call rpc -> IO (StreamElem () (Input rpc))
recvInput = atomically . recvInputSTM

-- | Send RPC output to the client
--
-- This will send a @grpc-status@ of @0@ to the client; for anything else (i.e.,
-- to indicate something went wrong), the server handler should throw a
-- 'GrpcException' (the @grapesy@ client API treats this the same way: a
-- @grpc-status@ other than @0@ will be raised as a 'GrpcException').
sendOutput :: Call rpc -> StreamElem [CustomMetadata] (Output rpc) -> IO ()
sendOutput call msg = do
    _updated <- initiateResponse call
    atomically $ sendOutputSTM call msg

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
-- Most server handlers will deal with single clients, but in principle
-- 'recvInputSTM' could be used to wait for the first message from any number
-- of clients.
recvInputSTM :: forall rpc. Call rpc -> STM (StreamElem () (Input rpc))
recvInputSTM Call{callChannel} =
    first ignoreTrailersOnly <$> Session.recv callChannel
  where
    ignoreTrailersOnly :: Either RequestHeaders () -> ()
    ignoreTrailersOnly _ = ()

-- | STM version of 'sendOutput'
--
-- You /MUST/ call 'initiateResponse' before calling 'sendOutputSTM'; throws
-- 'ResponseNotInitiated' otherwise. This is a low-level API; most users can use
-- 'sendOutput' instead.
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
    unless updated $ throwIO ResponseAlreadyInitiated
  where
    trailers :: TrailersOnly
    trailers = TrailersOnly $ ProperTrailers {
          trailerGrpcStatus  = GrpcOk
        , trailerGrpcMessage = Nothing
        , trailerMetadata    = metadata
        }

data ResponseKickoffException =
    ResponseAlreadyInitiated
  | ResponseNotInitiated
  deriving stock (Show)
  deriving anyclass (Exception)

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
    inp1 <- recvInput call
    case inp1 of
      NoMoreElems    () -> throwIO $ TooFewInputs @rpc
      FinalElem  inp () -> return inp
      StreamElem inp    -> do
        inp2 <- recvInput call
        case inp2 of
          NoMoreElems ()    -> return inp
          FinalElem  inp' _ -> throwIO $ TooManyInputs @rpc inp'
          StreamElem inp'   -> throwIO $ TooManyInputs @rpc inp'

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
-- included in the trailers (in this case, you can use the 'Default' instance
-- for 'ProperTrailers').
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
