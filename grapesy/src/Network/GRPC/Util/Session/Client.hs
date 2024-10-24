-- | Node with client role (i.e., its peer is a server)
module Network.GRPC.Util.Session.Client (
    ConnectionToServer(..)
  , NoTrailers(..)
  , CancelRequest
  , setupRequestChannel
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Proxy
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Client qualified as Client

import Network.GRPC.Util.HTTP2 (fromHeaderTable)
import Network.GRPC.Util.HTTP2.Stream
import Network.GRPC.Util.RedundantConstraint (addConstraint)
import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Session.Channel
import Network.GRPC.Util.Thread

{-------------------------------------------------------------------------------
  Connection
-------------------------------------------------------------------------------}

-- | Connection to the server, as provided by @http2@
data ConnectionToServer = ConnectionToServer {
      sendRequest :: forall a.
           Client.Request
        -> (Client.Response -> IO a)
        -> IO a
    }

{-------------------------------------------------------------------------------
  Initiate request

  Control flow and exception handling here is a little tricky.

  * 'sendRequest' (coming from @http2@) will itself spawn a separate thread to
    deal with sending inputs to the server (here, that is 'sendMessageLoop').

  * As the last thing it does, 'sendRequest' will then call its continuation
    argument in whatever thread it was called. Here, that continuation argument
    is 'recvMessageLoop' (dealing with outputs sent by the server), which we
    want to run in a separate thread also. We must therefore call 'sendRequest'
    in a newly forked thread.

    Note that 'sendRequest' will /only/ call its continuation once it receives
    a response from the server. Some servers will not send the (start of the)
    response until it has received (part of) the request, so it is important
    that we do not wait on 'sendRequest' before we return control to the caller.

  * We need to decide what to do with any exceptions that might arise in these
    threads. One option might be to rethrow those exceptions to the parent
    thread, but this presupposes a certain level of hygiene in client code: if
    the client code spawns threads of their own, and shares the open RPC call
    between them, then we rely on the client code to propagate the exception
    further.

    We therefore choose a different approach: we do not try to propagate the
    exception at all, but instead ensure that any (current or future) call to
    'read' or 'write' (or 'push') on the open 'Peer' will result in an exception
    when the respective threads have died.

  * This leaves one final problem to deal with: if setting up the request
    /itself/ throws an exception, one or both threads might never get started.
    To avoid client code blocking indefinitely we will therefore catch any
    exceptions that arise during the call setup, and use them to 'cancel' both
    threads (which will kill them or ensure they never get started).
-------------------------------------------------------------------------------}

-- | No trailers
--
-- We do not support outbound trailers in the client. This simplifies control
-- flow: when the inbound stream closes (because the server terminates the RPC),
-- we kill the outbound thread; by not supporting outbound trailers, we avoid
-- that exception propagating to the trailers maker, which causes http2 to
-- panic and shut down the entire connection.
class NoTrailers sess where
  -- | There is no interesting information in the trailers
  noTrailers :: Proxy sess -> Trailers (Outbound sess)

type CancelRequest = Maybe SomeException -> IO ()

-- | Setup request channel
--
-- This initiates a new request.
--
setupRequestChannel :: forall sess.
     (InitiateSession sess, NoTrailers sess)
  => sess
  -> ConnectionToServer
  -> (InboundResult sess -> SomeException)
  -- ^ We assume that when the server closes their outbound connection to us,
  -- the entire conversation is over (i.e., the server cannot "half-close").
  -> FlowStart (Outbound sess)
  -> IO (Channel sess, CancelRequest)
setupRequestChannel sess
                    ConnectionToServer{sendRequest}
                    terminateCall
                    outboundStart
                  = do
    channel <- initChannel
    let requestInfo = buildRequestInfo sess outboundStart

    cancelRequestVar <- newEmptyMVar
    let cancelRequest :: CancelRequest
        cancelRequest e = join . (fmap ($ e)) $ readMVar cancelRequestVar

    case outboundStart of
      FlowStartRegular headers -> do
        regular <- initFlowStateRegular headers
        let req :: Client.Request
            req = Client.requestStreamingIface
                    (requestMethod  requestInfo)
                    (requestPath    requestInfo)
                    (requestHeaders requestInfo)
                $ outboundThread channel cancelRequestVar regular
        forkRequest channel req
      FlowStartNoMessages trailers -> do
        let state :: FlowState (Outbound sess)
            state = FlowStateNoMessages trailers

            req :: Client.Request
            req = Client.requestNoBody
                    (requestMethod  requestInfo)
                    (requestPath    requestInfo)
                    (requestHeaders requestInfo)
        -- Can't cancel non-streaming request
        putMVar cancelRequestVar $ \_ -> return ()
        atomically $
          modifyTVar (channelOutbound channel) $ \oldState ->
            case oldState of
              ThreadNotStarted debugId ->
                ThreadDone debugId state
              _otherwise ->
                error "setupRequestChannel: expected thread state"
        forkRequest channel req

    return (channel, cancelRequest)
  where
    _ = addConstraint @(NoTrailers sess)

    forkRequest :: Channel sess -> Client.Request -> IO ()
    forkRequest channel req =
        forkThread "grapesy:clientInbound" (channelInbound channel) $ \unmask markReady _debugId -> unmask $
          linkOutboundToInbound (TerminateWhenInboundClosed terminateCall) channel $
            sendRequest req $ \resp -> do
              responseStatus <-
                case Client.responseStatus resp of
                  Just x  -> return x
                  Nothing -> throwM PeerMissingPseudoHeaderStatus

              -- Read the entire response body in case of a non-OK response
              responseBody :: Maybe Lazy.ByteString <-
                if HTTP.statusIsSuccessful responseStatus then
                  return Nothing
                else
                  Just <$> readResponseBody resp

              let responseHeaders =
                    fromHeaderTable $ Client.responseHeaders resp
                  responseInfo = ResponseInfo {
                    responseHeaders
                  , responseStatus
                  , responseBody
                  }

              flowStart <- parseResponse sess responseInfo
              case flowStart of
                FlowStartRegular headers -> do
                  state <- initFlowStateRegular headers
                  stream  <- clientInputStream resp
                  markReady $ FlowStateRegular state
                  Right <$> recvMessageLoop sess state stream
                FlowStartNoMessages trailers -> do
                  markReady $ FlowStateNoMessages trailers
                  return $ Left trailers

    outboundThread ::
         Channel sess
      -> MVar CancelRequest
      -> RegularFlowState (Outbound sess)
      -> Client.OutBodyIface
      -> IO ()
    outboundThread channel cancelRequestVar regular iface =
        threadBody "grapesy:clientOutbound" (channelOutbound channel) $ \markReady _debugId -> do
          markReady $ FlowStateRegular regular
          putMVar cancelRequestVar (Client.outBodyCancel iface)
          stream <- clientOutputStream iface
          -- Unlike the client inbound thread, or the inbound/outbound threads
          -- of the server, http2 knows about this particular thread and may
          -- raise an exception on it when the server dies. This results in a
          -- race condition between that exception and the exception we get from
          -- attempting to read the next message. No matter who wins that race,
          -- we need to mark that as 'ServerDisconnected'.
          --
          -- We don't have this top-level exception handler in other places
          -- because we don't want to mark /our own/ exceptions as
          -- 'ServerDisconnected' or 'ClientDisconnected'.
          wrapStreamExceptionsWith ServerDisconnected $
            Client.outBodyUnmask iface $ sendMessageLoop sess regular stream

{-------------------------------------------------------------------------------
   Auxiliary http2
-------------------------------------------------------------------------------}

readResponseBody :: Client.Response -> IO Lazy.ByteString
readResponseBody resp = go []
  where
    go :: [Strict.ByteString] -> IO Lazy.ByteString
    go acc = do
        chunk <- Client.getResponseBodyChunk resp
        if BS.Strict.null chunk then
          return $ BS.Lazy.fromChunks (reverse acc)
        else
          go (chunk:acc)