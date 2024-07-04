-- | Node with server role (i.e., its peer is a client)
module Network.GRPC.Util.Session.Server (
    ConnectionToClient(..)
  , setupResponseChannel
  ) where

import Control.Monad.XIO (XIO', NeverThrows)
import Control.Monad.XIO qualified as XIO
import Network.HTTP2.Server qualified as Server

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
--
-- We assume that the client is allowed to close their outbound stream to us.
setupResponseChannel :: forall sess.
     IsSession sess
  => sess
  -> ConnectionToClient
  -> FlowStart (Inbound sess)
  -> IO (FlowStart (Outbound sess), ResponseInfo)
  -- ^ Construct headers for the initial response
  --
  -- This function is allowed to block. If it does, no response will not be
  -- initiated until it returns.
  --
  -- If this function throws an exception, the response is never initiated;
  -- this is treated the same was as when we fail to set up the outbound
  -- connection due to a network failure.
  -> XIO' NeverThrows (Channel sess)
setupResponseChannel sess
                     conn
                     inboundStart
                     startOutbound
                   = XIO.unsafeTrustMe $ do
    channel <- initChannel

    forkThread "grapesy:serverInbound" (channelInbound channel) $
      \unmask markReady _debugId -> unmask $
        linkOutboundToInbound ContinueWhenInboundClosed channel $ do
          case inboundStart of
            FlowStartRegular headers -> do
              regular <- initFlowStateRegular headers
              stream  <- serverInputStream (request conn)
              markReady $ FlowStateRegular regular
              Right <$> recvMessageLoop sess regular stream
            FlowStartNoMessages trailers -> do
              -- The client sent a request with an empty body
              markReady $ FlowStateNoMessages trailers
              return $ Left trailers
              -- Thread terminates immediately

    forkThread "grapesy:serverOutbound" (channelOutbound channel) $
      \unmask markReady _debugId -> unmask $ do
        (outboundStart, responseInfo) <- startOutbound
        case outboundStart of
          FlowStartRegular headers -> do
            regular <- initFlowStateRegular headers
            markReady $ FlowStateRegular regular
            let resp :: Server.Response
                resp = setResponseTrailers sess channel regular
                     $ Server.responseStreamingIface
                             (responseStatus  responseInfo)
                             (responseHeaders responseInfo)
                     $ \iface -> do
                          stream <- serverOutputStream iface
                          sendMessageLoop sess regular stream
            respond conn resp
          FlowStartNoMessages trailers -> do
            markReady $ FlowStateNoMessages trailers
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
  -> RegularFlowState (Outbound sess)
  -> Server.Response -> Server.Response
setResponseTrailers sess channel regular resp =
    Server.setResponseTrailersMaker resp $
      outboundTrailersMaker sess channel regular
