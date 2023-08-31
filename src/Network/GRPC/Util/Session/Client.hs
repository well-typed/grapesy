-- | Node with client role (i.e., its peer is a server)
module Network.GRPC.Util.Session.Client (
    ConnectionToServer(..)
  , setupRequestChannel
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Tracer
import Network.HTTP2.Client qualified as Client

import Network.GRPC.Util.HTTP2 (fromHeaderTable)
import Network.GRPC.Util.HTTP2.Stream
import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Session.Channel
import Network.GRPC.Util.STM
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

-- | Setup request channel
--
-- This initiate a new request.
setupRequestChannel :: forall sess.
     InitiateSession sess
  => sess
  -> Tracer IO (DebugMsg sess)
  -> ConnectionToServer
  -> FlowStart (Outbound sess)
  -> IO (Channel sess)
setupRequestChannel sess tracer ConnectionToServer{sendRequest} outboundStart = do
    channel <- initChannel
    let requestInfo = buildRequestInfo sess outboundStart

    case outboundStart of
      FlowStartRegular headers -> do
        regular <- initFlowStateRegular headers
        let req :: Client.Request
            req = setRequestTrailers sess channel
                $ Client.requestStreamingUnmask
                    (requestMethod  requestInfo)
                    (requestPath    requestInfo)
                    (requestHeaders requestInfo)
                $ \unmask write' flush' -> unmask $ do
                     threadBody (channelOutbound channel)
                                unmask
                                (newTMVarIO (FlowStateRegular regular))
                              $ \_stVar -> do
                       stream <- clientOutputStream write' flush'
                       sendMessageLoop sess tracer regular stream
        forkRequest channel req
      FlowStartNoMessages trailers -> do
        stVar <- newTMVarIO $ FlowStateNoMessages trailers
        atomically $ writeTVar (channelOutbound channel) $ ThreadDone stVar
        let req :: Client.Request
            req = Client.requestNoBody
                    (requestMethod  requestInfo)
                    (requestPath    requestInfo)
                    (requestHeaders requestInfo)
        forkRequest channel req

    return channel
  where
    forkRequest :: Channel sess -> Client.Request -> IO ()
    forkRequest channel req =
        forkThread (channelInbound channel) newEmptyTMVarIO $ \stVar -> do
          setup <- try $ sendRequest req $ \resp -> do
            responseStatus <- case Client.responseStatus resp of
                                Just x  -> return x
                                Nothing -> throwM PeerMissingPseudoHeaderStatus
            let responseHeaders = fromHeaderTable $ Client.responseHeaders resp
                responseInfo    = ResponseInfo {responseHeaders, responseStatus}

            inboundStart <-
              if Client.responseBodySize resp == Just 0
                then FlowStartNoMessages <$>
                       parseResponseNoMessages sess responseInfo
                else FlowStartRegular <$>
                       parseResponseRegular sess responseInfo

            case inboundStart of
              FlowStartRegular headers -> do
                regular <- initFlowStateRegular headers
                stream  <- clientInputStream resp
                atomically $ putTMVar stVar $ FlowStateRegular regular
                recvMessageLoop sess tracer regular stream
              FlowStartNoMessages trailers ->
                atomically $ putTMVar stVar $ FlowStateNoMessages trailers

          case setup of
            Right () -> return ()
            Left  e  -> do
              void $ cancelThread (channelOutbound channel) e
              throwM e

{-------------------------------------------------------------------------------
   Auxiliary http2
-------------------------------------------------------------------------------}

setRequestTrailers ::
     IsSession sess
  => sess
  -> Channel sess
  -> Client.Request -> Client.Request
setRequestTrailers sess channel req =
    Client.setRequestTrailersMaker req $
      outboundTrailersMaker sess channel
