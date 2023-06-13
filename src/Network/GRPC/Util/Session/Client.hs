-- | Node with client role (i.e., its peer is a server)
module Network.GRPC.Util.Session.Client (
    ConnectionToServer(..)
  , initiateRequest
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Tracer
import Network.HTTP2.Client qualified as Client

import Network.GRPC.Util.HTTP2
import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Session.Channel
import Network.GRPC.Util.Thread

{-------------------------------------------------------------------------------
  Connection
-------------------------------------------------------------------------------}

-- | Connection to the server, as provided by @http2@
data ConnectionToServer = ConnectionToServer {
      sendRequest ::
           Client.Request
        -> (Client.Response -> IO ())
        -> IO ()
    }

{-------------------------------------------------------------------------------
  Initiate request

  Control flow and exception handling here is a little tricky.

  * 'sendRequest' (coming from @http2@) will itself spawn a separate thread to
    deal with sending inputs to the server (here, that is 'sendMessages').

  * As the last thing it does, 'sendRequest' will then call its continuation
    argument in whatever thread it was called. Here, that continuation argument
    is 'receiveMessages' (dealing with outputs sent by the server), which we
    want to run in a separate thread also. We must therefore call 'sendRequest'
    in a newly forked thread.

    (We /could/ spawn the thread inside the continuation to 'sendRequest', but
    its signature suggests some kind of scoping, so spawning it on the outside
    seems more forwards compatible.)

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

-- | Initiate new request
initiateRequest :: forall sess.
     InitiateSession sess
  => sess
  -> Tracer IO (DebugMsg sess)
  -> ConnectionToServer
  -> OutboundHeaders sess
  -> IO (Channel sess)
initiateRequest sess tracer ConnectionToServer{sendRequest} outboundHeaders = do
    channel     <- initChannel
    requestInfo <- buildRequestInfo sess outboundHeaders

    let req :: Client.Request
        req = setRequestTrailers sess channel $
          Client.requestStreaming
                (requestMethod  requestInfo)
                (requestPath    requestInfo)
                (requestHeaders requestInfo)
              $ \write flush -> do
            stream <- clientOutputStream write flush
            threadBody (channelOutbound channel) initOutbound $ \st -> do
              atomically $ putTMVar (channelOutboundHeaders st) outboundHeaders
              sendMessageLoop sess tracer st stream

    void $ forkIO $ threadBody (channelInbound channel) initInbound $ \st -> do
      setup <- try $ sendRequest req $ \resp -> do
        responseStatus <- case Client.responseStatus resp of
                            Just x  -> return x
                            Nothing -> throwIO PeerMissingPseudoHeaderStatus
        let responseHeaders = fromHeaderTable $ Client.responseHeaders resp
            responseInfo    = ResponseInfo {responseHeaders, responseStatus}
        inboundHeaders <- parseResponseInfo sess responseInfo
        atomically $ putTMVar (channelInboundHeaders st) inboundHeaders
        if Client.responseBodySize resp == Just 0 then do
          -- The gRPC specification defines
          --
          -- > Response → (Response-Headers *Length-Prefixed-Message Trailers)
          -- >          / Trailers-Only
          --
          -- The spec states in prose that
          --
          -- 1. For responses end-of-stream is indicated by the presence of the
          --    END_STREAM flag on the last received HEADERS frame that carries
          --    Trailers.
          -- 2. Implementations should expect broken deployments to send non-200
          --    HTTP status codes in responses as well as a variety of non-GRPC
          --    content-types and to omit Status & Status-Message.
          --    Implementations must synthesize a Status & Status-Message to
          --    propagate to the application layer when this occurs.
          --
          -- This means the only reliable way to determine if we're in the
          -- @Trailers-Only@ case is to see if the stream is terminated after we
          -- received the \"trailers\" (headers). We don't get direct access to
          -- this information from @http2@, /but/ we are told indirectly: if the
          -- stream is terminated, we are told that the body size is 0 (normally
          -- we do not expect an indication of body size at all); this is
          -- independent from any content-length header (which gRPC does not in
          -- fact allow for).
          processInboundTrailers sess tracer st responseHeaders
        else do
          stream <- clientInputStream resp
          recvMessageLoop
            sess
            tracer
            st
            stream
      case setup of
        Right ()                 -> return ()
        Left (e :: SomeException)-> close channel e

    return channel

{-------------------------------------------------------------------------------
   Auxiliary http2
-------------------------------------------------------------------------------}

setRequestTrailers ::
     IsSession sess
  => sess -> Channel sess -> Client.Request -> Client.Request
setRequestTrailers sess channel req =
    Client.setRequestTrailersMaker req $
      processOutboundTrailers sess channel
