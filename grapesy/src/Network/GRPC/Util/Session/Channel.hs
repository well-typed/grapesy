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
  , recvBoth
  , recvEither
  , RecvFinal(..)
  , RecvAfterFinal(..)
  , SendAfterFinal(..)
    -- * Closing
  , waitForOutbound
  , close
  , ChannelDiscarded(..)
  , ChannelAborted(..)
    -- * Support for half-closing
  , InboundResult
  , AllowHalfClosed(..)
  , linkOutboundToInbound
    -- * Constructing channels
  , sendMessageLoop
  , recvMessageLoop
  , outboundTrailersMaker
  ) where

import Control.Concurrent.STM
import Control.DeepSeq (NFData, force)
import Control.Exception
import Control.Monad
import Control.Monad.Catch (ExitCase(..))
import Data.Bifunctor
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as BS.Lazy
import GHC.Stack

-- Doesn't really matter if we import from .Client or .Server
import Network.HTTP2.Client qualified as HTTP2 (
    TrailersMaker
  , NextTrailersMaker(..)
  )

import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Spec.Util.Parser (Parser)
import Network.GRPC.Spec.Util.Parser qualified as Parser
import Network.GRPC.Util.HTTP2.Stream
import Network.GRPC.Util.RedundantConstraint
import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Thread
import GHC.Conc

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
      channelInbound :: TVar (ThreadState (FlowState (Inbound sess)))

      -- | Thread state of the thread sending messages to the peer
    , channelOutbound :: TVar (ThreadState (FlowState (Outbound sess)))

      -- | Have we sent the final message?
      --
      -- The sole purpose of this 'TVar' is catching user mistakes: if there is
      -- another 'send' after the final message, we can throw an exception,
      -- rather than the message simply being lost or blockng indefinitely.
    , channelSentFinal :: TVar (Maybe CallStack)

      -- | Have we received the final message?
      --
      -- This is used to improve the user experience; see 'channelSentFinal'.
      -- It is also used when checking if a call should be considered
      -- \"cancelled\"; see 'withRPC'.
    , channelRecvFinal :: TVar (RecvFinal (Inbound sess))
    }

data RecvFinal flow =
    -- | We have not yet delivered the final message to the client
    RecvNotFinal

    -- | We delivered the final message, but not yet the trailers
  | RecvWithoutTrailers (Trailers flow)

    -- | We delivered the final message and the trailers
  | RecvFinal CallStack

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

initChannel :: HasCallStack => IO (Channel sess)
initChannel = do
    channelInbound   <- newThreadState
    channelOutbound  <- newThreadState
    channelSentFinal <- newTVarIO Nothing
    channelRecvFinal <- newTVarIO RecvNotFinal
    return Channel{
        channelInbound
      , channelOutbound
      , channelSentFinal
      , channelRecvFinal
      }

initFlowStateRegular :: Headers flow -> IO (RegularFlowState flow)
initFlowStateRegular flowHeaders = do
   flowMsg        <- newEmptyTMVarIO
   flowTerminated <- newEmptyTMVarIO
   return RegularFlowState {
       flowHeaders
     , flowMsg
     , flowTerminated
     }

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
    withThreadInterface channelInbound (return . aux)
  where
    aux :: forall flow.
         FlowState flow
      -> Either (NoMessages flow) (Headers flow)
    aux (FlowStateRegular    regular)  = Right $ flowHeaders regular
    aux (FlowStateNoMessages trailers) = Left trailers

-- | Send a message to the node's peer
--
-- It is a bug to call 'send' again after the final message (that is, a message
-- which 'StreamElem.whenDefinitelyFinal' considers to be final). Doing so will
-- result in a 'SendAfterFinal' exception.
send :: forall sess.
     (HasCallStack, NFData (Message (Outbound sess)))
  => Channel sess
  -> StreamElem (Trailers (Outbound sess)) (Message (Outbound sess))
  -> IO ()
send Channel{channelOutbound, channelSentFinal} = \msg -> do
    msg' <- evaluate $ force <$> msg
    withThreadInterface channelOutbound $ aux msg'
  where
    aux ::
         StreamElem (Trailers (Outbound sess)) (Message (Outbound sess))
      -> FlowState (Outbound sess)
      -> STM ()
    aux msg st = do
        -- By checking that we haven't sent the final message yet, we know that
        -- this call to 'putMVar' will not block indefinitely: the thread that
        -- sends messages to the peer will get to it eventually (unless it dies,
        -- in which case the thread status will change and the call to
        -- 'getThreadInterface' will be retried).
        sentFinal <- readTVar channelSentFinal
        case sentFinal of
          Just cs -> throwSTM $ SendAfterFinal cs
          Nothing -> return ()
        case st of
          FlowStateRegular regular -> do
            StreamElem.whenDefinitelyFinal msg $ \_trailers ->
              writeTVar channelSentFinal $ Just callStack
            unsafeIOToSTM $ putStrLn "\n\nPUTTING MSG\n\n"
            s <- readTVar channelOutbound
            unsafeIOToSTM $ putStrLn $ "\n\nSTATE " ++ showThreadStateConstructor s
            putTMVar (flowMsg regular) msg
            unsafeIOToSTM $ putStrLn "\n\nPUT MSG\n\n"
          FlowStateNoMessages _ ->
            -- For outgoing messages, the caller decides to use Trailers-Only,
            -- so if they then subsequently call 'send', we throw an exception.
            -- This is different for /inbound/ messages; see 'recv', below.
            throwSTM $ SendButTrailersOnly

-- | Receive a message from the node's peer
--
-- If the sender indicates that the message is final /when/ they send it, by
-- sending the HTTP trailers in the same frame, then we will return the message
-- and the trailers together. It is a bug to call 'recvBoth' again after this;
-- doing so will result in a 'RecvAfterFinal' exception.
recvBoth :: forall sess.
     HasCallStack
  => Channel sess
  -> IO ( Either
            (NoMessages (Inbound sess))
            (StreamElem (Trailers (Inbound sess)) (Message (Inbound sess)))
        )
recvBoth =
    recv'
      StreamElem
      NoMoreElems
      ((,Nothing) . uncurry FinalElem)

-- | Variant on 'recvBoth' where trailers are always returned separately
--
-- Unlike in 'recvBoth', even if the sender indicates that the final message is
-- final when they send it, we will store these trailers internally and return
-- only that final message. The trailers are then returned on the /next/ call to
-- 'recvEither'. Call 'recvEither' again /after/ receiving the trailers is a
-- bug; doing so will result in a 'RecvAfterFinal' exception.
recvEither ::
     HasCallStack
  => Channel sess
  -> IO ( Either
            (NoMessages (Inbound sess))
            (Either (Trailers (Inbound sess)) (Message (Inbound sess)))
        )
recvEither =
    recv'
      Right
      Left
      (bimap Right Just)

-- | Internal generalization of 'recvBoth' and 'recvEither'
recv' :: forall sess b.
     HasCallStack
  => (Message    (Inbound sess) -> b)  -- ^ Message without trailers
  -> (Trailers   (Inbound sess) -> b)  -- ^ Trailers without (final) message
  -> (    (Message (Inbound sess), Trailers (Inbound sess))
       -> (b, Maybe (Trailers (Inbound sess)))
     )
     -- ^ Message with trailers
     --
     -- In addition to the result, should also return the trailers to keep for
     -- the next call to 'recv'' (if any).
  -> Channel sess
  -> IO (Either (NoMessages (Inbound sess)) b)
recv' messageWithoutTrailers
      trailersWithoutMessage
      messageWithTrailers
      Channel{channelInbound, channelRecvFinal} =
    withThreadInterface channelInbound aux
  where
    aux ::
         FlowState (Inbound sess)
      -> STM (Either (NoMessages (Inbound sess)) b)
    aux st = do
        -- By checking that we haven't received the final message yet, we know
        -- that this call to 'takeTMVar' will not block indefinitely: the thread
        -- that receives messages from the peer will get to it eventually
        -- (unless it dies, in which case the thread status will change and the
        -- call to 'getThreadInterface' will be retried).
        readFinal <- readTVar channelRecvFinal
        case readFinal of
          RecvNotFinal ->
            case st of
              FlowStateRegular regular -> Right <$> do
                streamElem <- takeTMVar (flowMsg regular)
                -- We update 'channelRecvFinal' in the same tx as the read, to
                -- atomically change "there is a value" to "all values read".
                case streamElem of
                  StreamElem msg ->
                    return $ messageWithoutTrailers msg
                  FinalElem msg trailers -> do
                    let (b, mTrailers) = messageWithTrailers (msg, trailers)
                    writeTVar channelRecvFinal $
                      maybe (RecvFinal callStack) RecvWithoutTrailers mTrailers
                    return $ b
                  NoMoreElems trailers -> do
                    writeTVar channelRecvFinal $ RecvFinal callStack
                    return $ trailersWithoutMessage trailers
              FlowStateNoMessages trailers -> do
                writeTVar channelRecvFinal $ RecvFinal callStack
                return $ Left trailers
          RecvWithoutTrailers trailers -> do
            writeTVar channelRecvFinal $ RecvFinal callStack
            return $ Right $ trailersWithoutMessage trailers
          RecvFinal cs ->
            throwSTM $ RecvAfterFinal cs

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
waitForOutbound :: Channel sess -> IO ()
waitForOutbound Channel{channelOutbound} = atomically $
    waitForNormalThreadTermination channelOutbound

-- | Close the channel
--
-- Before a channel can be closed, you should 'send' the final outbound message
-- and then 'waitForOutbound' until all outbound messages have been processed.
-- Not doing so is considered a bug (it is not possible to do this implicitly,
-- because the final call to 'send' involves a choice of trailers, and calling
-- 'waitForOutbound' /without/ a final close to 'send' will result in deadlock).
-- Typically code will also process all /incoming/ messages, but doing so is of
-- course not mandatory.
--
-- Calling 'close' will kill the outbound thread ('sendMessageLoop'), /if/ it is
-- still running. If the thread was terminated with an exception, this could
-- mean one of two things:
--
-- 1. The connection to the peer was lost
-- 2. Proper procedure for outbound messages was not followed (see above)
--
-- In the case of (2) this is bug in the caller, and so 'close' will return an
-- exception. In the case of (1), however, very likely an exception will
-- /already/ have been thrown when a communication attempt was made, and 'close'
-- will return 'Nothing'. This matches the design philosophy in @grapesy@ that
-- exceptions are thrown \"lazily\" rather than \"strictly\".
close ::
     HasCallStack
  => Channel sess
  -> ExitCase a    -- ^ The reason why the channel is being closed
  -> IO (Maybe SomeException)
close Channel{channelOutbound} reason = do
    -- We leave the inbound thread running. Although the channel is closed,
    -- there might still be unprocessed messages in the queue. The inbound
    -- thread will terminate once it reaches the end of the queue.
    outbound <- cancelThread channelOutbound channelClosed
    case outbound of
      AlreadyTerminated _ ->
        return $ Nothing
      AlreadyAborted _err ->
        -- Connection to the peer was lost prior to closing
        return $ Nothing
      Cancelled ->
        -- Proper procedure for outbound messages was not followed
        return $ Just channelClosed
  where
    channelClosed :: SomeException
    channelClosed =
        case reason of
          ExitCaseSuccess _   -> toException $ ChannelDiscarded callStack
          ExitCaseAbort       -> toException $ ChannelAborted   callStack
          ExitCaseException e -> e

-- | Channel was closed because it was discarded
--
-- This typically corresponds to leaving the scope of 'runHandler' or
-- 'withRPC' (without throwing an exception).
data ChannelDiscarded = ChannelDiscarded CallStack
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Channel was closed for an unknown reason
--
-- This will only be used in monad stacks that have error mechanisms other
-- than exceptions.
data ChannelAborted = ChannelAborted CallStack
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Support for half-closing
-------------------------------------------------------------------------------}

type InboundResult sess =
       Either (NoMessages (Inbound sess))
              (Trailers   (Inbound sess))

-- | Should we allow for a half-clsoed connection state?
--
-- In HTTP2, streams are bidirectional and can be half-closed in either
-- direction. This is however not true for all applications /of/ HTTP2. For
-- example, in gRPC the stream can be half-closed from the client to the server
-- (indicating that the client will not send any more messages), but not from
-- the server to the client: when the server half-closes their connection, it
-- sends the gRPC trailers and this terminates the call.
data AllowHalfClosed sess =
    ContinueWhenInboundClosed
  | TerminateWhenInboundClosed (InboundResult sess -> SomeException)

-- | Link outbound thread to the inbound thread
--
-- This should be wrapped around the body of the inbound thread. It ensures that
-- when the inbound thread throws an exception, the outbound thread dies also.
-- This improves predictability of exceptions: the inbound thread spends most of
-- its time blocked on messages from the peer, and will therefore notice when
-- the connection is lost. This is not true for the outbound thread, which
-- spends most of its time blocked waiting for messages to send to the peer.
linkOutboundToInbound :: forall sess.
     IsSession sess
  => AllowHalfClosed sess
  -> Channel sess
  -> IO (InboundResult sess)
  -> IO ()
linkOutboundToInbound allowHalfClosed channel inbound = do
    mResult <- try inbound

    -- Implementation note: After cancelThread returns, 'channelOutbound' has
    -- been updated, and considered dead, even if perhaps the thread is still
    -- cleaning up.

    case (mResult, allowHalfClosed) of
      (Right _result, ContinueWhenInboundClosed) ->
        return ()
      (Right result, TerminateWhenInboundClosed f) ->
        void $ cancelThread (channelOutbound channel) (f result)
      (Left (exception :: SomeException), _) -> do
        void $ cancelThread (channelOutbound channel) exception
        throwIO exception
  where
    _ = addConstraint @(IsSession sess)

{-------------------------------------------------------------------------------
  Constructing channels

  Both 'sendMessageLoop' and 'recvMessageLoop' will be run in newly forked
  threads, using the 'Thread' API from "Network.GRPC.Util.Thread". We are
  therefore not particularly worried about these loops being interrupted by
  asynchronous exceptions: this only happens if the threads are explicitly
  terminated (when the corresponding channels are closed), in which case any
  attempt to interact with them after they have been killed will be handled by
  'getThreadInterface' throwing 'ThreadInterfaceUnavailable'.
-------------------------------------------------------------------------------}

-- | Send all messages to the node's peer
sendMessageLoop :: forall sess.
     IsSession sess
  => sess
  -> RegularFlowState (Outbound sess)
  -> OutputStream
  -> IO ()
sendMessageLoop sess st stream = do
    trailers <- loop
    atomically $ putTMVar (flowTerminated st) trailers
  where
    build :: (Message (Outbound sess) -> Builder)
    build = buildMsg sess (flowHeaders st)

    loop :: IO (Trailers (Outbound sess))
    loop = do
        msg <- atomically $ takeTMVar (flowMsg st)
        case msg of
          StreamElem x -> do
            writeChunk stream $ build x
            flush stream
            loop
          FinalElem x trailers -> do
            writeChunkFinal stream $ build x
            return trailers
          NoMoreElems trailers -> do
            -- It is crucial to still 'writeChunkFinal' here to guarantee that
            -- cancellation is a no-op. Without it, cancellation may result in a
            -- @RST_STREAM@ frame being sent to the peer.
            --
            -- This does not necessarily write a DATA frame, since http2 avoids
            -- writing empty data frames unless they are marked @END_OF_STREAM@.
            writeChunkFinal stream $ mempty
            return trailers

-- | Receive all messages sent by the node's peer
recvMessageLoop :: forall sess.
     IsSession sess
  => sess
  -> RegularFlowState (Inbound sess)
  -> InputStream
  -> IO (Trailers (Inbound sess))
recvMessageLoop sess st stream =
    go $ parseMsg sess (flowHeaders st)
  where
    go :: Parser String (Message (Inbound sess)) -> IO (Trailers (Inbound sess))
    go parser = do
        mProcessedFinal <- throwParseErrors =<< Parser.processAll
          (getChunk stream)
          processOne
          processFinal
          parser
        case mProcessedFinal of
          Just trailers ->
            return trailers
          Nothing -> do
            trailers <- processTrailers
            atomically $ putTMVar (flowMsg st) $ NoMoreElems trailers
            return trailers

    processOne :: Message (Inbound sess) -> IO ()
    processOne msg = do
        atomically $ putTMVar (flowMsg st) $ StreamElem msg

    processFinal :: Message (Inbound sess) -> IO (Trailers (Inbound sess))
    processFinal msg = do
        trailers <- processTrailers
        atomically $ putTMVar (flowMsg st) $ FinalElem msg trailers
        return trailers

    processTrailers :: IO (Trailers (Inbound sess))
    processTrailers = do
        trailers <- parseInboundTrailers sess =<< getTrailers stream
        atomically $ putTMVar (flowTerminated st) $ trailers
        return trailers

    throwParseErrors :: Parser.ProcessResult String b -> IO (Maybe b)
    throwParseErrors (Parser.ProcessError err) =
        throwIO $ PeerSentMalformedMessage err
    throwParseErrors (Parser.ProcessedWithFinal b leftover) = do
        unless (BS.Lazy.null leftover) $ throwIO PeerSentIncompleteMessage
        return $ Just b
    throwParseErrors (Parser.ProcessedWithoutFinal leftover) = do
        unless (BS.Lazy.null leftover) $ throwIO PeerSentIncompleteMessage
        return $ Nothing

outboundTrailersMaker :: forall sess.
     IsSession sess
  => sess
  -> Channel sess
  -> RegularFlowState (Outbound sess)
  -> HTTP2.TrailersMaker
outboundTrailersMaker sess Channel{channelOutbound} regular = go
  where
    go :: HTTP2.TrailersMaker
    go (Just _) = return $ HTTP2.NextTrailersMaker go
    go Nothing  = do
        mFlowState <- atomically $
          unlessAbnormallyTerminated channelOutbound $
            readTMVar (flowTerminated regular)
        case mFlowState of
            Right trailers ->
              return $ HTTP2.Trailers $ buildOutboundTrailers sess trailers
            Left _exception ->
              return $ HTTP2.Trailers []
