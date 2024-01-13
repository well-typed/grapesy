-- | Node with server role (i.e., its peer is a client)
module Network.GRPC.Util.Session.Server (
    ConnectionToClient(..)
  , determineFlowStart
  , setupResponseChannel
  ) where

import Control.Tracer
import GHC.Stack
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as Server

import Network.GRPC.Util.HTTP2 (fromHeaderTable)
import Network.GRPC.Util.HTTP2.Stream
import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Session.Channel
import Network.GRPC.Util.Thread

import Debug.Concurrent

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

-- | Distinguish between Regular and Trailers-Only flow
determineFlowStart ::
     AcceptSession sess
  => sess
  -> Server.Request
  -> Either PeerException (FlowStart (Inbound sess))
determineFlowStart sess req
  | Server.requestBodySize req == Just 0
  = FlowStartNoMessages <$> parseRequestNoMessages sess requestHeaders

  | otherwise
  = FlowStartRegular <$> parseRequestRegular sess requestHeaders
  where
    requestHeaders :: [HTTP.Header]
    requestHeaders = fromHeaderTable $ Server.requestHeaders req

-- | Setup response channel
--
-- The actual response will not immediately be initiated; see below.
--
-- Does not throw any exceptions.
setupResponseChannel :: forall sess.
     (AcceptSession sess, HasCallStack)
  => sess
  -> Tracer IO (DebugMsg sess)
  -> ConnectionToClient
  -> FlowStart (Inbound sess)
  -> IO (FlowStart (Outbound sess))
  -- ^ Construct headers for the initial response
  --
  -- This function is allowed to block. If it does, no response will not be
  -- initiated until it returns.
  --
  -- It should /not/ throw any exceptions.
  -> IO (Channel sess)
setupResponseChannel sess tracer conn inboundStart startOutbound = do
    channel <- initChannel

    forkThread (channelInbound channel) newEmptyTMVarIO $ \stVar ->
      case inboundStart of
        FlowStartRegular headers -> do
          regular <- initFlowStateRegular headers
          stream  <- serverInputStream (request conn)
          atomically $ putTMVar stVar $ FlowStateRegular regular
          recvMessageLoop sess tracer regular stream
        FlowStartNoMessages trailers ->
          atomically $ putTMVar stVar $ FlowStateNoMessages trailers

    forkThread (channelOutbound channel) newEmptyTMVarIO $ \stVar -> do
      outboundStart <- startOutbound
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
                        sendMessageLoop sess tracer regular stream
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
