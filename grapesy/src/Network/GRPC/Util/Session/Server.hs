-- | Node with server role (i.e., its peer is a client)
module Network.GRPC.Util.Session.Server (
    ConnectionToClient(..)
  , setupResponseChannel
  ) where

import Network.HTTP2.Server qualified as Server

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef

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
-- Notes:
--
-- * The actual response will not immediately be initiated; see below.
-- * We assume that the client is allowed to close their outbound stream to us.
-- * 'setupResponseChannel' will not throw any exceptions.
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
  -> IO (Channel sess)
setupResponseChannel sess
                     conn
                     inboundStart
                     startOutbound
                   = do
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
            (waitForWorker, wrapWorker) <- joinWithWorker
            let resp :: Server.Response
                resp = setResponseTrailers sess channel regular
                     $ Server.responseStreamingIface
                             (responseStatus  responseInfo)
                             (responseHeaders responseInfo)
                     $ \iface -> wrapWorker $ do
                          stream <- serverOutputStream iface
                          sendMessageLoop sess regular stream
            respond conn resp
            waitForWorker
          FlowStartNoMessages trailers -> do
            markReady $ FlowStateNoMessages trailers
            let resp :: Server.Response
                resp = Server.responseNoBody
                         (responseStatus  responseInfo)
                         (responseHeaders responseInfo)
            respond conn $ resp

    return channel

{-------------------------------------------------------------------------------
  Auxiliary: join two threads
-------------------------------------------------------------------------------}

data WorkerStatus =
    -- | The thread ID of the worker thread is not yet known
    WorkerUnknown

    -- | The worker thread is known, and has installed an exception handler
  | WorkerReady ThreadId

    -- | We don't need the worker anymore, but its identity is not yet known
  | WorkerPreventStart

-- | Join current thread with worker thread
--
-- The result of the parent thread will be the result of the worker thread;
-- any exception thrown to the parent will also interrupt the worker, and
-- any exception raised in the worker will be also be raised in the parent.
joinWithWorker :: forall a. IO (IO a, IO a -> IO ())
joinWithWorker = do
    sync   :: MVar (Either SomeException a) <- newEmptyMVar
    cancel :: IORef WorkerStatus <- newIORef WorkerUnknown

    let waitForWorker :: IO a
        waitForWorker = do
            handle cancelWorker $
              either throwIO return =<< readMVar sync
          where
            cancelWorker :: SomeException -> IO a
            cancelWorker e = do
                mWorker <- atomicModifyIORef cancel $ \case
                  WorkerUnknown      -> (WorkerPreventStart, Nothing)
                  WorkerReady tid    -> (undefined, Just tid)
                  WorkerPreventStart -> error "impossible"
                forM_ mWorker $ \worker -> throwTo worker e
                throwIO e

        wrapWorker :: IO a -> IO ()
        wrapWorker k = do
            workerId <- myThreadId
            handle (putMVar sync . Left) $ do
              preventStart <- atomicModifyIORef cancel $ \case
                WorkerUnknown      -> (WorkerReady workerId, False)
                WorkerPreventStart -> (undefined, True)
                WorkerReady _tid   -> error "impossible"
              unless preventStart $
                putMVar sync =<< (Right <$> k)

    return (waitForWorker, wrapWorker)

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
