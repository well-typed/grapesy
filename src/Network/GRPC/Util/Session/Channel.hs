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
  , close
    -- ** Exceptions
  , RecvAfterFinal(..)
  , SendAfterFinal(..)
  , ChannelClosed(..)
    -- * Constructing channels
  , sendMessageLoop
  , recvMessageLoop
  , processInboundTrailers
  , processOutboundTrailers
    -- ** Logging
  , DebugMsg(..)
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Tracer
import Data.Bifunctor
import Data.ByteString qualified as BS.Strict
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as BS.Lazy
import GHC.Stack
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Internal qualified as HTTP2

import Network.GRPC.Common.StreamElem (StreamElem (..))
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Util.Parser
import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Thread
import Network.GRPC.Util.HTTP2.Stream

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
      -- | Thread state of the thread receiving messages from the sess's peer
      channelInbound :: TVar (ThreadState (TMVar (FlowState (Inbound sess))))

      -- | Thread state of the thread sending messages to the sess's peer
    , channelOutbound :: TVar (ThreadState (TMVar (FlowState (Outbound sess))))

      -- | 'CallStack' of the final call to 'send'
      --
      -- The sole purpose of this 'TMVar' is catching user mistakes: if there is
      -- another 'send' after the final message, we can throw an exception,
      -- rather than the message simply being lost or blockng indefinitely.
    , channelSentFinal :: TMVar CallStack

      -- | 'CallStack' of the final call to 'recv'
      --
      -- This is just to improve the user experience; see 'channelSentFinal'.
    , channelRecvFinal :: TMVar CallStack
    }

-- | Data flow state
--
-- We maintain three separate pieces of state:
--
-- 1. 'flowHeaders' is written to once, when we receive the inbound headers. It
--    is intended to support client code that wants to inspects these headers; a
--    'readTMVar' of this variable will block until the headers are received,
--    but will return /before/ the first message is received.
--
-- 2. 'flowMsg' is written to for every incoming message. This can be regarded
--    as a 1-place buffer, and it provides backpressure: we cannot receive
--    messages from the peer faster than the user is reading them.
--
--    TODO: It might make sense to generalize this to an @N@-place buffer, for
--    configurable @N@. This might provide for better latency masking.
--
-- 3. 'flowTrailers' is written to once, when we receive the trailers.
--
-- Invariant: when the trailers are written to 'flowTrailers', they will also
-- (atomically) be written to 'flowMsg'. We maintain both to allow the
-- 'HTTP2.TrailersMaker' to wait on 'flowTrailers'.
--
-- NOTE: 'flowTrailers' and 'channelRecvFinal' are set at different times:
-- 'flowTrailers' is set immediately upon receiving the trailers from the peer;
-- 'channelRecvFinal' is set when the user does the final call to 'recv' to
-- actually receive those trailers.
data FlowState flow =
    FlowStateRegular (RegularFlowState flow)
  | FlowStateTrailersOnly (TrailersOnly flow)

data RegularFlowState flow = RegularFlowState {
      flowHeaders  :: Headers flow
    , flowMsg      :: TMVar (StreamElem (ProperTrailers flow) (Message flow))
    , flowTrailers :: TMVar (ProperTrailers flow)
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

initChannel :: IO (Channel sess)
initChannel =
    Channel
      <$> newTVarIO ThreadNotStarted
      <*> newTVarIO ThreadNotStarted
      <*> newEmptyTMVarIO
      <*> newEmptyTMVarIO

initFlowStateRegular :: Headers flow -> IO (RegularFlowState flow)
initFlowStateRegular headers = do
    RegularFlowState headers
      <$> newEmptyTMVarIO
      <*> newEmptyTMVarIO

{-------------------------------------------------------------------------------
  Working with an open channel
-------------------------------------------------------------------------------}

-- | The inbound headers
--
-- Will block if the inbound headers have not yet been received.
getInboundHeaders ::
     Channel sess
  -> STM (Either (TrailersOnly (Inbound sess)) (Headers (Inbound sess)))
getInboundHeaders Channel{channelInbound} = do
    st <- readTMVar =<< getThreadInterface channelInbound
    return $ case st of
      FlowStateRegular      regular  -> Right $ flowHeaders regular
      FlowStateTrailersOnly trailers -> Left trailers

-- | Send a message to the node's peer
--
-- It is a bug to call 'send' again after the final message (that is, a message
-- which 'StreamElem.definitelyFinal' considers to be final). Doing so will
-- result in a 'SendAfterFinal' exception.
send ::
     HasCallStack
  => Channel sess
  -> StreamElem (ProperTrailers (Outbound sess)) (Message (Outbound sess))
  -> STM ()
send Channel{channelOutbound, channelSentFinal} msg = do
    -- By checking that we haven't sent the final message yet, we know that this
    -- call to 'putMVar' will not block indefinitely: the thread that sends
    -- messages to the peer will get to it eventually (unless it dies, in which
    -- case the thread status will change and the call to 'getThreadInterface'
    -- will be retried).
    sentFinal <- tryReadTMVar channelSentFinal
    case sentFinal of
      Just cs -> throwSTM $ SendAfterFinal cs
      Nothing -> do
        st <- readTMVar =<< getThreadInterface channelOutbound
        case st of
          FlowStateRegular regular -> do
            forM_ (StreamElem.definitelyFinal msg) $ \_trailers ->
              putTMVar channelSentFinal callStack
            putTMVar (flowMsg regular) msg
          FlowStateTrailersOnly _ ->
            -- For outgoing messages, the caller decides to use Trailers-Only,
            -- so if they then subsequently call 'send', we throw an exception.
            -- This is different for /inbound/ messages; see 'recv', below.
            throwSTM $ SendButTrailersOnly

-- | Receive a message from the node's peer
--
-- It is a bug to call 'recv' again after receiving the final message; Doing so
-- will result in a 'RecvAfterFinal' exception.
recv ::
     HasCallStack
  => Channel sess
  -> STM ( StreamElem
             (Either (TrailersOnly (Inbound sess)) (ProperTrailers (Inbound sess)))
             (Message (Inbound sess))
         )
recv Channel{channelInbound, channelRecvFinal} = do
    -- By checking that we haven't received the final message yet, we know that
    -- this call to 'takeTMVar' will not block indefinitely: the thread that
    -- receives messages from the peer will get to it eventually (unless it
    -- dies, in which case the thread status will change and the call to
    -- 'getThreadInterface' will be retried).
    readFinal <- tryReadTMVar channelRecvFinal
    case readFinal of
      Just cs -> throwSTM $ RecvAfterFinal cs
      Nothing -> do
        st  <- readTMVar =<< getThreadInterface channelInbound
        case st of
          FlowStateRegular regular -> do
            msg <- takeTMVar (flowMsg regular)
            -- We update 'channelRecvFinal' in the same tx as the read, to
            -- atomically change from "there is a value" to "all values read".
            forM_ (StreamElem.definitelyFinal msg) $ \_trailers ->
              putTMVar channelRecvFinal callStack
            return $ first Right msg
          FlowStateTrailersOnly trailers -> do
            putTMVar channelRecvFinal callStack
            return $ NoMoreElems (Left trailers)

-- | Close the channel
--
-- This should only be used under exceptional circumstances; the normal
-- way to terminate a connection is to
--
-- * Read until receiving the final message
-- * Write until sending the final message
--
-- At this point a call to 'close' is unnecessary though harmless: calling
-- 'close' on a channel that is terminated is a no-op.
--
-- TODO: @http2@ does not offer an API for indicating that we want to ignore
-- further output. We should check that this does not result in memory leak if
-- the server keeps sending data and we're not listening.)
close :: (HasCallStack, Exception e) => Channel sess -> e -> IO ()
close Channel{channelInbound, channelOutbound} e = do
    cancelThread channelInbound  $ ChannelClosed callStack (toException e)
    cancelThread channelOutbound $ ChannelClosed callStack (toException e)

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

-- | Channel was closed
data ChannelClosed = ChannelClosed {
      channelClosedAt     :: CallStack
    , channelClosedReason :: SomeException
    }
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Constructing channels
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
    go build = loop
      where
        loop :: IO ()
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
                atomically $ putTMVar (flowTrailers st) trailers
              NoMoreElems trailers -> do
                closeOutputStream stream
                atomically $ putTMVar (flowTrailers st) trailers

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
    go = loop
      where
        loop :: Parser (Message (Inbound sess)) -> IO ()
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

               | otherwise -> do
                   processInboundTrailers sess tracer st =<< getTrailers stream

processInboundTrailers :: forall sess.
     IsSession sess
  => sess
  -> Tracer IO (DebugMsg sess)
  -> RegularFlowState (Inbound sess)
  -> [HTTP.Header] -> IO ()
processInboundTrailers sess tracer st trailers = do
    inboundTrailers <- parseProperTrailers sess trailers
    traceWith tracer $ NodeRecvFinal inboundTrailers
    atomically $ do
      putTMVar (flowMsg      st) $ NoMoreElems inboundTrailers
      putTMVar (flowTrailers st) $ inboundTrailers

processOutboundTrailers :: forall sess.
     IsSession sess
  => sess
  -> RegularFlowState (Outbound sess)
  -> HTTP2.TrailersMaker
processOutboundTrailers sess st = go
  where
    go :: HTTP2.TrailersMaker
    go (Just _) = return $ HTTP2.NextTrailersMaker go
    go Nothing  = do
        outboundTrailers <- atomically $ readTMVar (flowTrailers st)
        HTTP2.Trailers <$> buildProperTrailers sess outboundTrailers

data DebugMsg sess =
    -- | Thread sending messages is awaiting a message
    NodeSendAwaitMsg

    -- | Thread sending message will send a message
  | NodeSendMsg (
        StreamElem
          (ProperTrailers (Outbound sess))
          (Message (Outbound sess))
      )

    -- | Receive thread requires data
    --
    -- We also record the data already received
  | NodeNeedsData BS.Lazy.ByteString

    -- | Receive thread received a message
  | NodeRecvMsg (
        StreamElem
          (Either (TrailersOnly (Inbound sess)) (ProperTrailers (Inbound sess)))
          (Message (Inbound sess))
      )

    -- | Receive thread received the trailers
  | NodeRecvFinal (ProperTrailers (Inbound sess))

deriving instance IsSession sess => Show (DebugMsg sess)
