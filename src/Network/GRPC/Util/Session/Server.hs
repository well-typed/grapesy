-- | Node with server role (i.e., its peer is a client)
module Network.GRPC.Util.Session.Server (
    ConnectionToClient(..)
  , determineFlowStart
  , setupResponseChannel
  ) where

import Control.Monad.XIO (XIO', NeverThrows)
import Control.Monad.XIO qualified as XIO
import Control.Tracer
import Network.HTTP.Types qualified as HTTP
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

-- | Distinguish between Regular and Trailers-Only flow
determineFlowStart ::
     AcceptSession sess
  => sess
  -> Server.Request
  -> IO (FlowStart (Inbound sess))
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
setupResponseChannel :: forall sess.
     AcceptSession sess
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
  -- If this function throws an exception, the response is never initiated;
  -- this is treated the same was as when we fail to set up the outbound
  -- connection due to a network failure.
  -> XIO' NeverThrows (Channel sess)
setupResponseChannel sess tracer conn inboundStart startOutbound =
    XIO.unsafeTrustMe $ do
      channel <- initChannel

      forkThread (channelInbound channel) $ \unmask markReady -> unmask $
        linkOutboundToInbound channel $ do
          case inboundStart of
            FlowStartRegular headers -> do
              regular <- initFlowStateRegular headers
              stream  <- serverInputStream (request conn)
              markReady $ FlowStateRegular regular
              recvMessageLoop sess tracer regular stream
            FlowStartNoMessages trailers -> do
              markReady $ FlowStateNoMessages trailers
              -- Thread terminates immediately

      forkThread (channelOutbound channel) $ \unmask markReady -> unmask $ do
        outboundStart <- startOutbound
        let responseInfo = buildResponseInfo sess outboundStart
        case outboundStart of
          FlowStartRegular headers -> do
            regular <- initFlowStateRegular headers
            markReady $ FlowStateRegular regular
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
  -> Server.Response -> Server.Response
setResponseTrailers sess channel resp =
    Server.setResponseTrailersMaker resp $
      outboundTrailersMaker sess channel
