-- | Node with client role (i.e., its peer is a server)
module Network.GRPC.Util.Session.Client (
    ConnectionToServer(..)
  , setupRequestChannel
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Tracer
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Client qualified as Client

import Network.GRPC.Util.HTTP2 (fromHeaderTable)
import Network.GRPC.Util.HTTP2.Stream
import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Session.Channel
import Network.GRPC.Util.Thread

import Debug.Concurrent

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
                $ outboundThread channel regular
        forkRequest channel req
      FlowStartNoMessages trailers -> do
        let state :: FlowState (Outbound sess)
            state = FlowStateNoMessages trailers

            req :: Client.Request
            req = Client.requestNoBody
                    (requestMethod  requestInfo)
                    (requestPath    requestInfo)
                    (requestHeaders requestInfo)
        atomically $ writeTVar (channelOutbound channel) $ ThreadDone state
        forkRequest channel req

    return channel
  where
    forkRequest :: Channel sess -> Client.Request -> IO ()
    forkRequest channel req =
        forkThread (channelInbound channel) $ \unmask markReady -> unmask $ do
          handle (killOutbound channel) $ sendRequest req $ \resp -> do
            responseStatus <- case Client.responseStatus resp of
                                Just x  -> return x
                                Nothing -> throwM PeerMissingPseudoHeaderStatus

            -- Read the entire response body in case of a non-OK response
            responseBody :: Maybe Lazy.ByteString <-
              if HTTP.statusIsSuccessful responseStatus then
                return Nothing
              else
                Just <$> readResponseBody resp

            let responseHeaders = fromHeaderTable $ Client.responseHeaders resp
                responseInfo    = ResponseInfo {
                                      responseHeaders
                                    , responseStatus
                                    , responseBody
                                    }

            if Client.responseBodySize resp /= Just 0 then do
              headers <- parseResponseRegular sess responseInfo
              regular <- initFlowStateRegular headers
              stream  <- clientInputStream resp
              markReady $ FlowStateRegular regular
              recvMessageLoop sess tracer regular stream
            else do
              trailers <- parseResponseNoMessages sess responseInfo
              markReady $ FlowStateNoMessages trailers

    -- Kill the outbound thread if we fail to send the request
    -- (the /inbound/ thread is never started in this case)
    killOutbound :: Channel sess -> SomeException -> IO ()
    killOutbound channel err = do
        void $ cancelThread (channelOutbound channel) err
        throwM err

    outboundThread ::
         Channel sess
      -> RegularFlowState (Outbound sess)
      -> (forall x. IO x -> IO x)
      -> (Builder -> IO ())
      -> IO ()
      -> IO ()
    outboundThread channel regular unmask write' flush' =
       threadBody (channelOutbound channel) $ \markReady -> unmask $ do
         markReady $ FlowStateRegular regular
         -- Initialize the output stream to initiate the request.
         -- See 'clientOutputStream' for details.
         stream <- clientOutputStream write' flush'
         sendMessageLoop sess tracer regular stream

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