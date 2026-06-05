-- | Node with server role (i.e., its peer is a client)
module Network.GRPC.Util.Session.Server (
    ConnectionToClient(..)
  , setupResponseChannel
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import GHC.Stack
import Network.HTTP.Semantics.Server qualified as Server

import Network.GRPC.Common.Exception
import Network.GRPC.Util.ServerStream
import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Session.Channel
import Network.GRPC.Util.Stream
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
  Internal auxiliary: constructing responses
-------------------------------------------------------------------------------}

respondStreaming ::
     ConnectionToClient
  -> Server.TrailersMaker
  -> ResponseInfo
  -> (OutputStream -> IO ())
  -> IO ()
respondStreaming conn trailers responseInfo body =
    respond conn resp
  where
    resp :: Server.Response
    resp =
          flip Server.setResponseTrailersMaker trailers
        . Server.responseStreamingIface
            (responseStatus  responseInfo)
            (responseHeaders responseInfo)
        $ body <=< serverOutputStream

respondStreamingWithResult :: forall a.
     ConnectionToClient
  -> Server.TrailersMaker
  -> ResponseInfo
  -> (OutputStream -> IO a)
  -> IO a
respondStreamingWithResult conn trailers responseInfo body = do
    resultVar :: MVar (Either ExactException a) <- newEmptyMVar
    respondStreaming conn trailers responseInfo $ \iface -> do
      -- This will be running in an auxiliary thread, spawned by http2. Any
      -- exceptions that are thrown by that thread will remain uncaught, and
      -- will trigger the top-level uncaught exception handler. We therefore
      -- catch all of these and store them in 'resultVar', which the main
      -- grapesy outbound thread is waiting on.
      --
      -- When the connection is closed, this thread will be cleaned up by
      -- the ThreadManager in http2.
      result <- try $ body iface
      putMVar resultVar result

    -- Any exception thrown here is thrown in the context of a 'Thread'.
    either throwExact return =<< takeMVar resultVar

respondNoBody :: ConnectionToClient -> ResponseInfo -> IO ()
respondNoBody conn responseInfo =
    respond conn resp
  where
    resp :: Server.Response
    resp = Server.responseNoBody
             (responseStatus  responseInfo)
             (responseHeaders responseInfo)

{-------------------------------------------------------------------------------
  Initiate response
-------------------------------------------------------------------------------}

-- | Setup response channel
--
-- Notes:
--
-- * The actual response will not immediately be initiated; see below.
-- * We assume that the client is allowed to close their outbound stream to us.
-- * 'setupResponseChannel' will not throw any exceptions.
setupResponseChannel :: forall sess.
     (HasCallStack, IsSession sess)
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
  -> IO (Channel sess)
setupResponseChannel sess
                     conn
                     inboundStart
                     startOutbound
                   = do
    channel <- initChannel "server"

    forkThread "grapesy:serverInbound" (channelInbound channel) $ \unmask ctxt -> unmask $
      case inboundStart of
        FlowStartRegular headers -> do
          regular <- initFlowStateRegular headers
          stream  <- serverInputStream (request conn)
          threadMainBody ctxt regular $
            recvMessageLoop sess regular stream
        FlowStartNoMessages trailers ->
          -- The client sent a request with an empty body
          threadTrivial ctxt trailers

    forkThread "grapesy:serverOutbound" (channelOutbound channel) $ \unmask ctxt -> unmask $ do
      (outboundStart, responseInfo) <- startOutbound
      case outboundStart of
        FlowStartRegular headers -> do
          regular <- initFlowStateRegular headers
          threadMainBody ctxt regular $ do
            -- Monitoring here is a bit subtle: http2 will spawn an auxiliary
            -- thread, which will be the one that will actually run the
            -- 'sendMessageLoop'. Meanwhile, we will simply be waiting for that
            -- auxiliary thread to terminate; if the monitor fires, is it
            -- /this/ thread that receives it, not the auxiliary http2 thread.
            monitor <- threadMonitor ctxt (channelInbound channel) monitorPred
            respondStreamingWithResult
                conn
                (outboundTrailersMaker sess channel regular)
                responseInfo $ \stream ->
              sendMessageLoop sess regular stream monitor
        FlowStartNoMessages trailers -> do
          respondNoBody conn responseInfo
          threadTrivial ctxt trailers

    return channel
  where
    -- Unlike on the client-side, if the input thread terminates normally, the
    -- server can continue to run normally (the stream can be half-closed from
    -- the client). We only only terminate if the inbound thread threw an
    -- exception, indicating that the client disconnected abruptly.
    --
    -- See also 'Network.GRPC.Util.Session.Client.setupRequestChannel'.
    monitorPred ::
          Either
            ThreadException
            ( Either
                (NoMessages (Inbound sess))
                (Trailers   (Inbound sess))
            )
       -> Maybe ExactException
    monitorPred = \case
        Left  e         -> Just $ threadException e
        Right _trailers -> Nothing
