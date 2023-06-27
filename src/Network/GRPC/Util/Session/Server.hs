-- | Node with server role (i.e., its peer is a client)
module Network.GRPC.Util.Session.Server (
    ConnectionToClient(..)
  , initiateResponse
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Tracer
import Network.HTTP2.Server qualified as Server

import Network.GRPC.Util.HTTP2 (fromHeaderTable)
import Network.GRPC.Util.HTTP2.Stream
import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Session.Channel
import Network.GRPC.Util.Thread

{-------------------------------------------------------------------------------
  Connection
-------------------------------------------------------------------------------}

-- | Connection to the client, as provided by @http2@
data ConnectionToClient = ConnectionToClient {
      request :: Server.Request
    , respond :: Server.Response -> IO ()
    }

{-------------------------------------------------------------------------------
  Initiate response
-------------------------------------------------------------------------------}

-- | Initiate response to the client
initiateResponse :: forall sess.
     AcceptSession sess
  => sess
  -> Tracer IO (DebugMsg sess)
  -> ConnectionToClient
  -> (Headers (Inbound sess) -> IO (Headers (Outbound sess)))
  -> IO (Channel sess)
initiateResponse sess tracer conn mkOutboundHeaders = do
    channel <- initChannel

    let requestHeaders = fromHeaderTable $ Server.requestHeaders (request conn)
    requestMethod <- case Server.requestMethod (request conn) of
                       Just x  -> return x
                       Nothing -> throwIO PeerMissingPseudoHeaderMethod
    requestPath   <- case Server.requestPath (request conn) of
                       Just x  -> return x
                       Nothing -> throwIO PeerMissingPseudoHeaderPath
    let requestInfo = RequestInfo {requestHeaders, requestMethod, requestPath}

    inboundHeaders <- parseRequestInfo sess requestInfo

    void $ forkIO $
      threadBody (channelInbound channel) initFlowState $ \st -> do
        atomically $ putTMVar (flowHeaders st) inboundHeaders
        if Server.requestBodySize (request conn)  == Just 0 then
          processInboundTrailers sess tracer st requestHeaders
        else do
          stream <- serverInputStream (request conn)
          recvMessageLoop
            sess
            tracer
            st
            stream

    outboundHeaders <- mkOutboundHeaders inboundHeaders
    responseInfo    <- buildResponseInfo sess outboundHeaders

    let resp :: FlowState (Outbound sess) -> Server.Response
        resp st = setResponseTrailers sess channel $
          Server.responseStreaming
                        (responseStatus  responseInfo)
                        (responseHeaders responseInfo)
                      $ \write flush -> do
            stream <- serverOutputStream write flush
            sendMessageLoop sess tracer st stream

    void $ forkIO $
      threadBody (channelOutbound channel) initFlowState $ \st -> do
        atomically $ putTMVar (flowHeaders st) outboundHeaders
        respond conn $ resp st

    return channel

{-------------------------------------------------------------------------------
  Auxiliary http2
-------------------------------------------------------------------------------}

setResponseTrailers ::
     IsSession sess
  => sess
  -> Channel sess
  -> Server.Response -> Server.Response
setResponseTrailers sess channel resp =
    Server.setResponseTrailersMaker resp $
      processOutboundTrailers sess channel
