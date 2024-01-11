-- | Channel
--
-- You should not have to import this module directly; instead import
-- "Network.GRPC.Util.Session".
module Network.GRPC.Util.Session.Channel (
    -- * Main definition
    Channel(..)
  , initChannel
    -- ** Flow state
  , FlowState(..)
  , RegularFlowState(..)
  , initFlowStateRegular
    -- * Working with an open channel
  , getInboundHeaders
  , send
  , recv
  , RecvAfterFinal(..)
  , SendAfterFinal(..)
    -- * Closing
  , waitForOutbound
  , close
  , ChannelUncleanClose(..)
  , ChannelDiscarded(..)
  , ChannelException(..)
  , ChannelAborted(..)
    -- ** Exceptions
    -- * Constructing channels
  , sendMessageLoop
  , recvMessageLoop
  , outboundTrailersMaker
    -- * Status
  , ChannelStatus(..)
  , FlowStatus(..)
  , checkChannelStatus
  , isChannelHealthy
    -- ** Logging
  , DebugMsg(..)
  ) where

import Control.Exception
import Control.Monad.Catch
import Control.Tracer
import Data.Bifunctor
import Data.ByteString qualified as BS.Strict
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as BS.Lazy
import GHC.Stack
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Internal qualified as HTTP2

import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Internal
import Network.GRPC.Util.HTTP2.Stream
import Network.GRPC.Util.Parser
import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Thread

import Debug.Concurrent

{-------------------------------------------------------------------------------
  Definitions

  The fields of 'Channel' are its /implementation/, not its interface. It is
  kept opaque in the top-level @.Peer@ module.

  Implementation note: it is tempting to try and define 'Channel' purely in
  terms of bytestrings, and deal with serialization and deserialization to and
  from messages in a higher layer. However, this does not work:

  - For deserialization, if we make chunks of messages available in the 'TMVar',
    then if multiple threads are reading from that one 'TMVar', one thread might
    get the first chunk of a message and another thread the second.
  - Similarly, for serialization, if multiple threads are trying to write to the
    'TMVar', we might get interleaving of fragments of messages.

  Thus, to ensure thread safety, we work at the level of messages, not bytes.
-------------------------------------------------------------------------------}

-- | Bidirectional open channel on a node to a peer node
--
-- The node might be a client (and its peer a server), or the node might be
-- a server (and its peer a client); the main purpose of this abstraction
-- is precisely to abstract over that difference.
--
-- Each channel is constructed for a /single/ session (request/response).
data Channel sess = Channel {
      -- | Thread state of the thread receiving messages from the peer
      channelInbound :: TVar (ThreadState (TMVar (FlowState (Inbound sess))))

      -- | Thread state of the thread sending messages to the peer
    , channelOutbound :: TVar (ThreadState (TMVar (FlowState (Outbound sess))))

      -- | 'CallStack' of the final call to 'send'
      --
      -- The sole purpose of this 'TVar' is catching user mistakes: if there is
      -- another 'send' after the final message, we can throw an exception,
      -- rather than the message simply being lost or blockng indefinitely.
    , channelSentFinal :: TVar (Maybe CallStack)

      -- | 'CallStack' of the final call to 'recv'
      --
      -- This is just to improve the user experience; see 'channelSentFinal'.
    , channelRecvFinal :: TVar (Maybe CallStack)
    }

-- | Data flow state
data FlowState flow =
    FlowStateRegular (RegularFlowState flow)
  | FlowStateNoMessages (NoMessages flow)

-- | Regular (streaming) flow state
data RegularFlowState flow = RegularFlowState {
      -- | Headers
      --
      -- On the client side, the outbound headers are specified when the request
      -- is made ('callRequestMetadata'), and the inbound headers are recorded
      -- once the responds starts to come in; clients can block-and-wait for
      -- these headers ('getInboundHeaders').
      --
      -- On the server side, the inbound headers are recorded when the request
      -- comes in, and the outbound headers are specified
      -- ('setResponseMetadata') before the response is initiated
      -- ('initiateResponse'/'sendTrailersOnly').
      flowHeaders :: Headers flow

      -- | Messages
      --
      -- This TMVar is written to for incoming messages ('recvMessageLoop') and
      -- read from for outgoing messages ('sendMessageLoop'). It acts as a
      -- one-place buffer, providing backpressure in both directions.
      --
      -- TODO: It might make sense to generalize this to an @N@-place buffer,
      -- for configurable @N@. This might result in better latency masking.
    , flowMsg :: TMVar (StreamElem (Trailers flow) (Message flow))

      -- | Trailers
      --
      -- Unlike 'flowMsg', which is /written/ to in 'recvMessageLoop' and /read/
      -- from in 'sendMessageLoop', both loops /set/ 'flowTerminated', once,
      -- just before they terminate.
      --
      -- * For 'sendMessageLoop', this means that the last message has been
      --   written (that is, the last call to 'writeChunk' has happened).
      --   This has two consequences:
      --
      --   1. @http2@ can now construct the trailers ('outboundTrailersMaker')
      --   2. Higher layers can wait on 'flowTerminated' to be /sure/ that the
      --      last message has been written.
      --
      -- * For 'recvMessageLoop', this means that the trailers have been
      --   received from the peer. Higher layers can use this to check for, or
      --   block-and-wait, to receive those trailers.
      --
      -- == Relation to 'channelSentFinal'/'channelRecvFinal'
      --
      -- 'flowTerminated' is set at different times than 'channelSentFinal' and
      -- 'channelRecvFinal' are:
      --
      -- * 'channelSentFinal' is set on the last call to 'send', but /before/
      --   the message is processed by 'sendMessageLoop'.
      -- * 'channelRecvFinal', dually, is set on the last call to 'recv, which
      --   must (necessarily) happen /before/ that message is actually made
      --   available by 'recvMessageLoop'.
      --
      -- /Their/ sole purpose is to catch user errors, not capture data flow.
    , flowTerminated :: TMVar (Trailers flow)
    }

-- | 'Show' instance is useful in combination with @stm-debug@ only
deriving instance (
    Show (Headers flow)
  , Show (TMVar (StreamElem (Trailers flow) (Message flow)))
  , Show (TMVar (Trailers flow))
  ) => Show (RegularFlowState flow)

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

initChannel :: IO (Channel sess)
initChannel =
    Channel
      <$> newThreadState
      <*> newThreadState
      <*> newTVarIO Nothing
      <*> newTVarIO Nothing

initFlowStateRegular :: Headers flow -> IO (RegularFlowState flow)
initFlowStateRegular headers = do
    RegularFlowState headers
      <$> newEmptyTMVarIO
      <*> newEmptyTMVarIO

{-------------------------------------------------------------------------------
  Status check
-------------------------------------------------------------------------------}

data ChannelStatus sess = ChannelStatus {
      channelStatusInbound   :: FlowStatus (Inbound sess)
    , channelStatusOutbound  :: FlowStatus (Outbound sess)
    , channelStatusSentFinal :: Bool
    , channelStatusRecvFinal :: Bool
    }

data FlowStatus flow =
    FlowNotYetEstablished
  | FlowEstablished (FlowState flow)
  | FlowTerminated
  | FlowFailed SomeException

-- | Get channel status
--
-- This is inherently non-deterministic: it is entirely possible that the
-- connection to the peer has already been broken but we haven't noticed yet,
-- or that the connection will be broken immediately after this call and before
-- whatever the next call is.
checkChannelStatus :: Channel sess -> STM (ChannelStatus sess)
checkChannelStatus Channel{ channelInbound
                   , channelOutbound
                   , channelSentFinal
                   , channelRecvFinal
                   } = do
    stInbound  <- readTVar channelInbound
    stOutbound <- readTVar channelOutbound
    channelStatusInbound   <- go stInbound
    channelStatusOutbound  <- go stOutbound
    channelStatusSentFinal <- isFinal <$> readTVar channelSentFinal
    channelStatusRecvFinal <- isFinal <$> readTVar channelRecvFinal
    return ChannelStatus{
        channelStatusInbound
      , channelStatusOutbound
      , channelStatusSentFinal
      , channelStatusRecvFinal
      }
  where
    go :: ThreadState (TMVar (FlowState flow)) -> STM (FlowStatus flow)
    go ThreadNotStarted       = return FlowNotYetEstablished
    go (ThreadInitializing _) = return FlowNotYetEstablished
    go (ThreadRunning _ a)    = FlowEstablished <$> readTMVar a
    go (ThreadDone _)         = return $ FlowTerminated
    go (ThreadException e)    = return $ FlowFailed e

    isFinal :: Maybe CallStack -> Bool
    isFinal = maybe False (const True)

-- | Check if the channel is healthy
--
-- This is a simplified API on top of 'getChannelStatus, which
--
-- * retries if the flow in either direction has not yet been established
-- * returns 'False' if the flow in either direction has been terminated,
--   or has failed
-- * returns 'True' otherwise.
isChannelHealthy :: Channel sess -> STM Bool
isChannelHealthy channel = do
    status <- checkChannelStatus channel
    case (channelStatusInbound status, channelStatusOutbound status) of
      (FlowNotYetEstablished , _                    ) -> retry
      (_                     , FlowNotYetEstablished) -> retry

      (FlowTerminated        , _                    ) -> return False
      (_                     , FlowTerminated       ) -> return False
      (FlowFailed _          , _                    ) -> return False
      (_                     , FlowFailed _         ) -> return False

      (FlowEstablished _     , FlowEstablished _    ) -> return True

{-------------------------------------------------------------------------------
  Working with an open channel
-------------------------------------------------------------------------------}

-- | The inbound headers
--
-- Will block if the inbound headers have not yet been received.
getInboundHeaders ::
     Channel sess
  -> IO (Either (NoMessages (Inbound sess)) (Headers (Inbound sess)))
getInboundHeaders Channel{channelInbound} =
    withThreadInterface channelInbound aux
  where
    aux :: forall flow.
         TMVar (FlowState flow)
      -> STM (Either (NoMessages flow) (Headers flow))
    aux iface = do
        st <- readTMVar iface
        return $ case st of
          FlowStateRegular    regular  -> Right $ flowHeaders regular
          FlowStateNoMessages trailers -> Left trailers

-- | Send a message to the node's peer
--
-- It is a bug to call 'send' again after the final message (that is, a message
-- which 'StreamElem.definitelyFinal' considers to be final). Doing so will
-- result in a 'SendAfterFinal' exception.
send :: forall sess.
     HasCallStack
  => Channel sess
  -> StreamElem (Trailers (Outbound sess)) (Message (Outbound sess))
  -> IO ()
send Channel{channelOutbound, channelSentFinal} msg =
    withThreadInterface channelOutbound aux
  where
    aux :: TMVar (FlowState (Outbound sess)) -> STM ()
    aux iface = do
        -- By checking that we haven't sent the final message yet, we know that this
        -- call to 'putMVar' will not block indefinitely: the thread that sends
        -- messages to the peer will get to it eventually (unless it dies, in which
        -- case the thread status will change and the call to 'getThreadInterface'
        -- will be retried).
        sentFinal <- readTVar channelSentFinal
        case sentFinal of
          Just cs -> throwSTM $ SendAfterFinal cs
          Nothing -> do
            st <- readTMVar iface
            case st of
              FlowStateRegular regular -> do
                StreamElem.whenDefinitelyFinal msg $ \_trailers ->
                  writeTVar channelSentFinal $ Just callStack
                putTMVar (flowMsg regular) msg
              FlowStateNoMessages _ ->
                -- For outgoing messages, the caller decides to use Trailers-Only,
                -- so if they then subsequently call 'send', we throw an exception.
                -- This is different for /inbound/ messages; see 'recv', below.
                throwSTM $ SendButTrailersOnly

-- | Receive a message from the node's peer
--
-- It is a bug to call 'recv' again after receiving the final message; Doing so
-- will result in a 'RecvAfterFinal' exception.
recv :: forall sess.
     HasCallStack
  => Channel sess
  -> IO ( StreamElem
            (Either (NoMessages (Inbound sess)) (Trailers (Inbound sess)))
            (Message (Inbound sess))
        )
recv Channel{channelInbound, channelRecvFinal} =
    withThreadInterface channelInbound aux
  where
    aux ::
         TMVar (FlowState (Inbound sess))
      -> STM ( StreamElem
                 (Either (NoMessages (Inbound sess)) (Trailers (Inbound sess)))
                 (Message (Inbound sess))
             )
    aux iface = do
        -- By checking that we haven't received the final message yet, we know that
        -- this call to 'takeTMVar' will not block indefinitely: the thread that
        -- receives messages from the peer will get to it eventually (unless it
        -- dies, in which case the thread status will change and the call to
        -- 'getThreadInterface' will be retried).
        readFinal <- readTVar channelRecvFinal
        case readFinal of
          Just cs -> throwSTM $ RecvAfterFinal cs
          Nothing -> do
            -- We get the TMVar in the same transaction as reading from it (below).
            -- This means that /if/ the thread running 'recvMessageLoop' throws an
            -- exception and is killed, the 'takeTMVar' below cannot block
            -- indefinitely (because the transaction will be retried).
            st <- readTMVar iface
            case st of
              FlowStateRegular regular -> do
                msg <- takeTMVar (flowMsg regular)
                -- We update 'channelRecvFinal' in the same tx as the read, to
                -- atomically change from "there is a value" to "all values read".
                StreamElem.whenDefinitelyFinal msg $ \_trailers ->
                  writeTVar channelRecvFinal $ Just callStack
                return $ first Right msg
              FlowStateNoMessages trailers -> do
                writeTVar channelRecvFinal $ Just callStack
                return $ NoMoreElems (Left trailers)

-- | Thrown by 'send'
--
-- The 'CallStack' is the callstack of the final call to 'send'.
--
-- See 'send' for additional discussion.
data SendAfterFinal =
    -- | Call to 'send' after the final message was sent
    SendAfterFinal CallStack

    -- | Call to 'send', but we are in the Trailers-Only case
  | SendButTrailersOnly
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Thrown by 'recv'
--
-- The 'CallStack' is the callstack of the final call to 'recv'.
--
-- See 'recv' for additional discussion.
data RecvAfterFinal =
     -- | Call to 'recv' after the final message was already received
     RecvAfterFinal CallStack
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Closing
-------------------------------------------------------------------------------}

-- | Wait for the outbound thread to terminate
--
-- See 'close' for discussion.
waitForOutbound :: HasCallStack => Channel sess -> IO (FlowState (Outbound sess))
waitForOutbound Channel{channelOutbound} = atomically $
    readTMVar =<< waitForThread channelOutbound

-- | Close the channel
--
-- Before a channel can be closed, you should 'send' the final outbound message
-- and then 'waitForOutbound' until all outbound messages have been processed.
-- Not doing so is considered a bug (it is not possible to do this implicitly,
-- because the final call to 'send' involves a choice of trailers, and calling
-- 'waitForOutbound' /without/ a final close to 'send' will result in deadlock).
-- Typically code will also process all /incoming/ messages, but doing so of
-- course not mandatory.
--
-- Calling 'close' will kill the outbound thread ('sendMessageLoop'), /if/ it is
-- still running. If the thread was terminated with an exception, this could
-- mean one of two things:
--
-- * The connection to the peer was lost
-- * Proper procedure for outbound messages was not followed (see above)
--
-- In this case, 'close' will return a 'ChannelUncleanClose' exception.
--
-- TODO: @http2@ does not offer an API for indicating that we want to ignore
-- further output. We should check that this does not result in memory leak (if
-- the server keeps sending data and we're not listening.)
close ::
     HasCallStack
  => Channel sess
  -> ExitCase a    -- ^ The reason why the channel is being closed
  -> IO (Maybe ChannelUncleanClose)
close Channel{channelOutbound} reason = do
    -- We leave the inbound thread running. Although the channel is closed,
    -- there might still be unprocessed messages in the queue. The inbound
    -- thread will terminate once it reaches the end of the queue
     outbound <- cancelThread channelOutbound channelClosed
     case outbound of
       Right _   -> return $ Nothing
       Left  err -> return $ Just (ChannelUncleanClose err)
  where
    channelClosed :: SomeException
    channelClosed =
        case reason of
          ExitCaseSuccess _   -> toException $ ChannelDiscarded callStack
          ExitCaseException e -> toException $ ChannelException callStack e
          ExitCaseAbort       -> toException $ ChannelAborted   callStack

-- | Thrown by 'close' if not all outbound messages have been processed
--
-- See 'close' for discussion.
data ChannelUncleanClose = ChannelUncleanClose SomeException
  deriving stock (Show)
  deriving Exception via ExceptionWrapper ChannelUncleanClose

instance HasNestedException ChannelUncleanClose where
  getNestedException (ChannelUncleanClose e) = e

-- | Channel was closed because it was discarded
--
-- This typically corresponds to leaving the scope of 'acceptCall' or
-- 'withRPC' (without throwing an exception).
data ChannelDiscarded = ChannelDiscarded CallStack
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Channel was closed with an exception
data ChannelException = ChannelException CallStack SomeException
  deriving stock (Show)
  deriving Exception via ExceptionWrapper ChannelException

instance HasNestedException ChannelException where
  getNestedException (ChannelException _ e) = e

-- | Channel was closed for an unknown reason
--
-- This will only be used in monad stacks that have error mechanisms other
-- than exceptions.
data ChannelAborted = ChannelAborted CallStack
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Constructing channels

  Both 'sendMessageLoop' and 'recvMessageLoop' will be run in newly forked
  threads, using the 'Thread' API from "Network.GRPC.Util.Thread". We are
  therefore not particularly worried about these loops being interrupted by
  asynchronous exceptions: this only happens if the threads are explicitly
  terminated (when the corrresponding channels are closed), in which case any
  attempt to interact with them after they have been killed will be handled by
  'getThreadInterface' throwing 'ThreadInterfaceUnavailable'.
-------------------------------------------------------------------------------}

-- | Send all messages to the node's peer
sendMessageLoop :: forall sess.
     IsSession sess
  => sess
  -> Tracer IO (DebugMsg sess)
  -> RegularFlowState (Outbound sess)
  -> OutputStream
  -> IO ()
sendMessageLoop sess tracer st stream =
    go $ buildMsg sess (flowHeaders st)
  where
    go :: (Message (Outbound sess) -> Builder) -> IO ()
    go build = do
        trailers <- loop
        atomically $ putTMVar (flowTerminated st) trailers
      where
        loop :: IO (Trailers (Outbound sess))
        loop = do
            traceWith tracer $ NodeSendAwaitMsg
            msg <- atomically $ takeTMVar (flowMsg st)
            traceWith tracer $ NodeSendMsg msg

            case msg of
              StreamElem x -> do
                writeChunk stream $ build x
                flush stream
                loop
              FinalElem x trailers -> do
                -- We don't flush the last message, so that http2 can mark the
                -- stream as END_STREAM (rather than having to send a separate
                -- empty data frame).
                writeChunk stream $ build x
                return trailers
              NoMoreElems trailers -> do
                return trailers

-- | Receive all messages sent by the node's peer
--
-- TODO: This is wrong, we are never marking the final element as final.
-- (But fixing this requires a patch to http2.)
recvMessageLoop :: forall sess.
     IsSession sess
  => sess
  -> Tracer IO (DebugMsg sess)
  -> RegularFlowState (Inbound sess)
  -> InputStream
  -> IO ()
recvMessageLoop sess tracer st stream =
    go $ parseMsg sess (flowHeaders st)
  where
    go :: Parser (Message (Inbound sess)) -> IO ()
    go = \parser -> do
        trailers <- loop parser >>= parseInboundTrailers sess
        traceWith tracer $ NodeRecvFinal trailers
        atomically $ putTMVar (flowTerminated st) $ trailers
        atomically $ putTMVar (flowMsg        st) $ NoMoreElems trailers
      where
        loop :: Parser (Message (Inbound sess)) -> IO [HTTP.Header]
        loop (ParserError err) =
            throwIO $ PeerSentMalformedMessage err
        loop (ParserDone x p') = do
            traceWith tracer $ NodeRecvMsg (StreamElem x)
            atomically $ putTMVar (flowMsg st) $ StreamElem x
            loop p'
        loop (ParserNeedsData acc p') = do
            traceWith tracer $ NodeNeedsData acc
            bs <- getChunk stream

            if | not (BS.Strict.null bs) ->
                   loop $ p' bs

               | not (BS.Lazy.null acc) ->
                   throwIO PeerSentIncompleteMessage

               | otherwise ->
                   getTrailers stream

outboundTrailersMaker :: forall sess.
     IsSession sess
  => sess
  -> Channel sess
  -> HTTP2.TrailersMaker
outboundTrailersMaker sess channel = go
  where
    go :: HTTP2.TrailersMaker
    go (Just _) = return $ HTTP2.NextTrailersMaker go
    go Nothing  = do
        -- Wait for the thread to terminate
        --
        -- If the thread was killed, this will throw an exception (which will
        -- then result in @http2@ cancelling the corresponding stream).
        flowState <- waitForOutbound channel
        trailers  <- case flowState of
                       FlowStateRegular regular ->
                         atomically $ readTMVar $ flowTerminated regular
                       FlowStateNoMessages _ ->
                         error "unexpected FlowStateNoMessages"
        return $ HTTP2.Trailers $ buildOutboundTrailers sess trailers

data DebugMsg sess =
    -- | Thread sending messages is awaiting a message
    NodeSendAwaitMsg

    -- | Thread sending message will send a message
  | NodeSendMsg (
        StreamElem
          (Trailers (Outbound sess))
          (Message  (Outbound sess))
      )

    -- | Receive thread requires data
    --
    -- We also record the data already received
  | NodeNeedsData BS.Lazy.ByteString

    -- | Receive thread received a message
  | NodeRecvMsg (
        StreamElem
          (Either (NoMessages (Inbound sess)) (Trailers (Inbound sess)))
          (Message (Inbound sess))
      )

    -- | Receive thread received the trailers
  | NodeRecvFinal (Trailers (Inbound sess))

deriving instance IsSession sess => Show (DebugMsg sess)
