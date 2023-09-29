-- | Node with server role (i.e., its peer is a client)
module Network.GRPC.Util.Session.Server (
    ConnectionToClient(..)
  , setupResponseChannel
  ) where

import Control.Exception
import Control.Tracer
import Network.HTTP2.Server qualified as Server
import GHC.Stack

import Network.GRPC.Util.Concurrency
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

-- | Setup response channel
--
-- The actual response will not immediately be initiated; see below.
setupResponseChannel :: forall sess.
     (AcceptSession sess, HasCallStack)
  => sess
  -> Tracer IO (DebugMsg sess)
  -> ConnectionToClient
  -> (FlowStart (Inbound sess) -> IO (FlowStart (Outbound sess)))
  -- ^ Construct headers for the initial response
  --
  -- This function has quite a bit of control over how the outbound part of
  -- the channel gets established:
  --
  -- * If it blocks, the response will not be initiated until it returns.
  -- * If it throws an exception, no response will be attempted at all. This
  --   allows the function to send a response of its own (typically, some kind
  --   of error response).
  -> IO (Channel sess)
setupResponseChannel sess tracer conn startOutbound = do
    channel <- initChannel

    let requestHeaders = fromHeaderTable $ Server.requestHeaders (request conn)
    requestMethod <- case Server.requestMethod (request conn) of
                       Just x  -> return x
                       Nothing -> throwIO PeerMissingPseudoHeaderMethod
    requestPath   <- case Server.requestPath (request conn) of
                       Just x  -> return x
                       Nothing -> throwIO PeerMissingPseudoHeaderPath
    let requestInfo = RequestInfo {requestHeaders, requestMethod, requestPath}

    inboundStart <-
      if Server.requestBodySize (request conn) == Just 0 then
        FlowStartNoMessages <$> parseRequestNoMessages sess requestInfo
      else
        FlowStartRegular <$> parseRequestRegular sess requestInfo

    forkThread (channelInbound channel) newEmptyTMVarIO $ \unmask stVar -> do
      case inboundStart of
        FlowStartRegular headers -> do
          regular <- initFlowStateRegular headers
          stream  <- serverInputStream (request conn)
          atomically $ putTMVar stVar $ FlowStateRegular regular
          recvMessageLoop sess unmask tracer regular stream
        FlowStartNoMessages trailers -> do
          atomically $ putTMVar stVar $ FlowStateNoMessages trailers

    forkThread (channelOutbound channel) newEmptyTMVarIO $ \unmask stVar -> do
      outboundStart <- startOutbound inboundStart
      let responseInfo = buildResponseInfo sess outboundStart
      case outboundStart of
        FlowStartRegular headers -> do
          regular <- initFlowStateRegular headers
          atomically $ putTMVar stVar $ FlowStateRegular regular
          let resp :: Server.Response
              resp = setResponseTrailers sess channel
                   $ Server.responseStreaming
                           (responseStatus  responseInfo)
                           (responseHeaders responseInfo)
                   $ \write' flush' -> do
                        stream <- serverOutputStream write' flush'
                        sendMessageLoop sess unmask tracer regular stream
          respond conn resp
        FlowStartNoMessages trailers -> do
          atomically $ putTMVar stVar $ FlowStateNoMessages trailers
          let resp :: Server.Response
              resp = Server.responseNoBody
                       (responseStatus  responseInfo)
                       (responseHeaders responseInfo)
          respond conn $ resp

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
      outboundTrailersMaker sess channel
