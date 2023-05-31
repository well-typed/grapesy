-- | Abstract over server/client
--
-- Nothing in this module is gRPC specific.
--
-- Intended for qualified import import.
--
-- > import Network.GRPC.Util.Peer (Peer)
-- > import Network.GRPC.Util.Peer qualified as Peer
module Network.GRPC.Util.Peer (
    -- * Definition
    Peer -- opaque
  , HttpTrailers
    -- * Construction
  , Config(..)
    -- ** Connect to server
  , Server(..)
  , ServerContext(..)
  , connectToServer
    -- ** Accept client
  , Client(..)
  , acceptClient
    -- * Interacting
  , recv
  , send
  , disconnect
    -- * Logging
  , DebugMsg(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Tracer
import Data.Bifunctor (Bifunctor(..))
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Builder qualified as BS (Builder)
import Data.ByteString.Lazy qualified as BS.Lazy
import GHC.Stack
import Network.HPACK qualified as HPACK
import Network.HPACK.Token qualified as HPACK
import Network.HTTP.Types
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Client qualified as Client
import Network.HTTP2.Client qualified as HTTP2
import Network.HTTP2.Server qualified as Server

import Network.GRPC.Util.Parser
import Network.GRPC.Util.StreamElem
import Network.GRPC.Util.Thread

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type HttpTrailers = [HTTP.Header]

-- | Peer (client or server)
--
-- Type variables:
--
-- * We send messages of type @s@ to the peer
-- * We receive messages of type @r@ from the peer
data Peer s r = Peer {
      -- | Thread state of the thread sending messages to the peer
      peerOutbound  :: TVar (ThreadState (TMVar (StreamElem HttpTrailers s)))

      -- | Thread state of the thread receiving messages from the peer
    , peerInbound   :: TVar (ThreadState (TMVar (StreamElem HttpTrailers r)))

      -- | Have we sent the final message?
    , peerSentFinal :: TMVar (CallStack, HttpTrailers)

      -- | Have we received the final message?
    , peerRecvFinal :: TMVar (CallStack, HttpTrailers)
    }

initPeer :: IO (Peer s r)
initPeer = do
    peerOutbound  <- newTVarIO ThreadNotStarted
    peerInbound   <- newTVarIO ThreadNotStarted
    peerSentFinal <- newEmptyTMVarIO
    peerRecvFinal <- newEmptyTMVarIO

    return Peer{peerOutbound, peerInbound, peerSentFinal, peerRecvFinal}

{-------------------------------------------------------------------------------
  Peer setup common to client and server
-------------------------------------------------------------------------------}

-- | Peer configuration
--
-- We do not provide the serialization with the context @cxt@: a server might
-- not start sending their response until it has received the complete request,
-- including the request body (this is the case for example for non-streaming
-- gRPC requests).
data Config cxt s r = Config {
      -- | Serialize message to the peer
      serializeOutbound :: s -> BS.Builder

      -- | Parse messages from the peer
    , parseInbound :: cxt -> Parser r
    }

{-------------------------------------------------------------------------------
  Public API

  Implementation note: it is tempting to try and define 'Peer' purely in terms
  of bytestrings, and deal with serialization and deserialization to and from
  messages in a higher layer. However, this does not work:

  - For deserialization, if we make chunks of messages available in the 'TMVar',
    then if multiple threads are reading from that one 'TMVar', one thread might
    get the first chunk of a message and another thread the second.
  - Similarly, for serialization, if multiple threads are trying to write to the
    'TMVar', we might get interleaving of fragments of messages.

  Thus, to ensure thread safety, we work at the level of messages, not bytes.
-------------------------------------------------------------------------------}

-- | Receive a message from the peer
--
-- It is a bug to call 'read' again after receiving 'ReadFinal'; doing so will
-- result in a 'RecvAfterFinal' exception.
recv :: HasCallStack => Peer s r -> STM (StreamElem HttpTrailers r)
recv Peer{peerInbound, peerRecvFinal} = do
    -- By checking that we haven't received the trailers yet, we know that this
    -- call to 'takeTMVar' will not block indefinitely: the thread that receives
    -- messages from the peer will get to it eventually (unless it dies, in
    -- which case the thread status will change and the call to
    -- 'getThreadInterface' will be retried).
    readFinal <- tryReadTMVar peerRecvFinal
    case readFinal of
      Just (cs, trailers) ->
        throwSTM $ RecvAfterFinal cs trailers
      Nothing -> do
        queue <- getThreadInterface peerInbound
        msg   <- takeTMVar queue
        -- We update 'peerRecvFinal' in the same tx as the read, so that we
        -- atomically change from "there is a value" to "all values read".
        forM_(streamElemDefinitelyFinal msg) $ \trailers ->
          putTMVar peerRecvFinal (callStack, trailers)
        return msg

-- | Send a message to the peer
--
-- It is a bug to call 'send' again after sending 'SendFinal'; doing so will
-- result in a 'SendAfterFinal' exception.
send :: HasCallStack => Peer s r -> StreamElem HttpTrailers s -> STM ()
send Peer{peerOutbound, peerSentFinal} msg = do
    -- By checking that we haven't sent the final message yet, we know that this
    -- call to 'putMVar' will not block indefinitely: the thread that sends
    -- messages to the peer will get to it eventually (unless it dies, in which
    -- case the thread status will change and the call to 'getThreadInterface'
    -- will be retried).
    sentFinal <- tryReadTMVar peerSentFinal
    case sentFinal of
      Just (cs, trailers) ->
        throwSTM $ SendAfterFinal cs trailers
      Nothing -> do
        queue <- getThreadInterface peerOutbound
        forM_ (streamElemDefinitelyFinal msg) $ \trailers ->
          putTMVar peerSentFinal (callStack, trailers)
        putTMVar queue msg

-- | Disconnect from the peer
--
-- This should only be used under exceptional circumstances; the normal
-- way to terminate a connection is to
--
-- * Read until receiving 'ReadTrailers'
-- * Write 'WriteTrailers'
--
-- TODO: @http2@ does not offer an API for indicating that we want to ignore
-- further output. We should check that this does not result in memory leak if
-- the server keeps sending data and we're not listening.)
disconnect :: (HasCallStack, Exception e) => Peer s r -> e -> IO ()
disconnect Peer{peerInbound, peerOutbound} e = do
    cancelThread peerInbound  $ Disconnected callStack (toException e)
    cancelThread peerOutbound $ Disconnected callStack (toException e)

{-------------------------------------------------------------------------------
  Client

  Control flow and exception handling here is a little tricky.

  * 'sendRequest' (coming from @http2@) will itself spawn a separate thread to
    deal with sending inputs to the server (here, that is 'sendMessages').

  * As the last thing it does, 'sendRequest' will then call its continuation
    argument in whatever thread it was called. Here, that continuation argument
    is 'receiveMessages' (dealing with outputs sent by the server), which we
    want to run in a separate thread also. We must therefore call 'sendRequest'
    in a newly forked thread.

    (We /could/ spawn the thread inside the continuation to 'sendRequest', but
    its signature suggests some kind of scoping, so spawning it on the outside
    seems more forwards compatible.)

  * We need to decide what to do with any exceptions that might arise in these
    threads. One option might be to rethrow those exceptions to the parent
    thread, but this presupposes a certain level of hygiene in client code: if
    the client code spawns threads of their own, and shares the open RPC call
    between them, then we rely on the client code to propagate the exception
    further.

    We therefore choose a different approach: we do not try to propagate the
    exception at all, but instead ensure that any (current or future) call to
    'read' or 'write' (or 'push') on the open 'Peer' will result in an exception
    when the respective threads have died.

  * This leaves one final problem to deal with: if setting up the request
    /itself/ throws an exception, one or both threads might never get started.
    To avoid client code blocking indefinitely we will therefore catch any
    exceptions that arise during the call setup, and use them to 'cancel' both
    threads (which will kill them or ensure they never get started).
-------------------------------------------------------------------------------}

data Server cxt = Server {
      -- | Setup message context
      serverContext ::
           Maybe Status   -- ^ HTTP status code
        -> Maybe Int      -- ^ Body size
        -> [HTTP.Header]  -- ^ Response headers
        -> IO (ServerContext cxt)

      -- | Request method
    , requestMethod :: Method

      -- | Request path
    , requestPath :: Client.Path

      -- | Request headers
    , requestHeaders :: RequestHeaders
    }

data ServerContext cxt =
    -- | Server context
    --
    -- See 'Config' for a discussion of the use of @cxt@.
    ServerContext cxt

    -- | No server context
    --
    -- This should be used if no messages will be exchanged. In this case, the
    -- response headers will be treated as trailers.
  | NoServerContext

connectToServer :: forall cxt s r.
     HasCallStack
  => Tracer IO (DebugMsg s r)
  -> (Client.Request -> (Client.Response -> IO ()) -> IO ()) -- ^ From http2
  -> Config cxt s r
  -> Server cxt
  -> IO (Peer s r)
connectToServer tracer sendRequest cfg server = do
    peer <- initPeer

    let req :: Client.Request
        req = setRequestTrailers peer $
          Client.requestStreaming
                (requestMethod  server)
                (requestPath    server)
                (requestHeaders server)
              $ \write flush ->
            threadBody (peerOutbound peer) newEmptyTMVarIO $ \q ->
              sendMessages tracer (serializeOutbound cfg) q write flush

    void $ forkIO $ threadBody (peerInbound peer) newEmptyTMVarIO $ \q -> do
      setup <- try $ sendRequest req $ \resp -> do
        let responseHeaders = fromHeaderTable $ Client.responseHeaders resp
        mCxt <- serverContext server
                  (Client.responseStatus   resp)
                  (Client.responseBodySize resp)
                  responseHeaders
        case mCxt of
          NoServerContext -> do
            atomically $ do
              -- Instead of calling 'receiveMessages' to listen for incoming
              -- messages, indicate that there /are/ no messages, so that a
              -- call to 'recv' will return the correct result.
              putTMVar q $ NoMoreElems responseHeaders

              -- TODO: Not entirely sure what we should do with the outbound
              -- thread. What happens if the user calls 'send' in this situation?
              -- Will the outbound thread throw an exception? If so, when is
              -- that detected by the user, one the /next/ call to 'send', or
              -- on the first call? And how is this different from a 'send'
              -- when the server sends a RST_STREAM to indicate the stream
              -- should be closed?
          ServerContext cxt ->
            receiveMessages
              tracer
              (parseInbound cfg cxt)
              q
              (Client.getResponseBodyChunk resp)
              (maybe [] fromHeaderTable <$> Client.getResponseTrailers resp)
      case setup of
        Right () -> return ()
        Left e   -> disconnect peer (e :: SomeException)

    return peer

{-------------------------------------------------------------------------------
  Server

  The structure here is a bit different from the client, because in the client
  case @http2@ spawns one of the two threads for us, whereas here we are
  responsible for spawning both.

  (TODO: Verify that this is really true in the http2 source code.)
-------------------------------------------------------------------------------}

data Client s r = Client {
      -- | HTTP response status
      responseStatus  :: Status

      -- | Response headers
    , responseHeaders :: ResponseHeaders
    }

acceptClient :: forall s r.
     Tracer IO (DebugMsg s r)
  -> Server.Request
  -> (Server.Response -> IO ())
  -> Config () s r
  -> Client s r
  -> IO (Peer s r)
acceptClient tracer req respond cfg client = do
    peer <- initPeer

    void $ forkIO $ threadBody (peerInbound peer) newEmptyTMVarIO $ \q ->
      receiveMessages
        tracer
        (parseInbound cfg ())
        q
        (Server.getRequestBodyChunk req)
        (maybe [] fromHeaderTable <$> Server.getRequestTrailers req)

    let resp :: TMVar (StreamElem HttpTrailers s) -> Server.Response
        resp q = setResponseTrailers peer $
          Server.responseStreaming
                        (responseStatus  client)
                        (responseHeaders client)
                      $ \write flush ->
            sendMessages tracer (serializeOutbound cfg) q write flush

    void $ forkIO $ threadBody (peerOutbound peer) newEmptyTMVarIO $ \q ->
      respond $ resp q

    return peer

{-------------------------------------------------------------------------------
  Functionality shared by the client and the server
-------------------------------------------------------------------------------}

sendMessages ::
     Tracer IO (DebugMsg s r)
  -> (s -> BS.Builder)
  -> TMVar (StreamElem HttpTrailers s)
  -> (BS.Builder -> IO ())  -- Write chunk
  -> IO ()                  -- Flush
  -> IO ()
sendMessages tracer serialize q write flush =
    loop
  where
    loop :: IO ()
    loop = do
        traceWith tracer $ PeerSendAwaitMsg
        msg <- atomically $ takeTMVar q
        traceWith tracer $ PeerSendMsg msg

        case msg of
          StreamElem x -> do
            write $ serialize x
            flush
            loop
          FinalElem x _trailers ->
            -- We don't flush the last message, so that http2 can mark the
            -- stream as END_STREAM (rather than having to send a separate
            -- empty data frame).
            write $ serialize x
          NoMoreElems _trailers ->
            return ()

-- TODO: This is wrong, we are never marking the final element as final.
-- (But fixing this requires a patch to http2.)
receiveMessages :: forall s r.
     Tracer IO (DebugMsg s r)
  -> Parser r
  -> TMVar (StreamElem HttpTrailers r)
  -> IO Strict.ByteString
  -> IO HttpTrailers
  -> IO ()
receiveMessages tracer deserialize q getChunk getTrailers =
    loop deserialize
  where
    loop :: Parser r -> IO ()
    loop (ParserDone x p') = do
        traceWith tracer $ PeerRecvMsg x
        atomically $ putTMVar q $ StreamElem x
        loop p'
    loop (ParserNeedsData acc p') = do
        traceWith tracer $ PeerNeedsData acc
        bs <- getChunk

        if not (BS.Strict.null bs) then
          loop $ p' bs
        else do
          if not (BS.Lazy.null acc) then
            throwIO RecvTrailingData
          else do
            mTrailers <- getTrailers
            traceWith tracer $ PeerRecvFinal mTrailers
            atomically $ putTMVar q $ NoMoreElems mTrailers
    loop (ParserError err) =
        throwIO $ RecvMalformed err

{-------------------------------------------------------------------------------
  Trailers

  The trailers that we create are /always/ constructed from the values we
  /send/, whether we are a server or a client.
-------------------------------------------------------------------------------}

setResponseTrailers :: Peer s v -> Server.Response -> Server.Response
setResponseTrailers peer resp =
    Server.setResponseTrailersMaker resp $
      trailersMaker (peerSentFinal peer)

setRequestTrailers :: Peer s v -> Client.Request -> Client.Request
setRequestTrailers peer req =
    Client.setRequestTrailersMaker req $
      trailersMaker (peerSentFinal peer)

trailersMaker :: TMVar (CallStack, HttpTrailers) -> HTTP2.TrailersMaker
trailersMaker var = go
  where
    go :: HTTP2.TrailersMaker
    go (Just _) = return $ HTTP2.NextTrailersMaker go
    go Nothing  = do
        (_cs, trailers) <- atomically $ readTMVar var
        return $ HTTP2.Trailers trailers

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Thrown by 'read'
--
-- The 'CallStack' is the callstack of the final call to 'recv'.
--
-- See 'read' for additional discussion.
data RecvAfterFinal = RecvAfterFinal CallStack HttpTrailers
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Thrown by 'send'
--
-- The 'CallStack' is teh callstack of the final call to 'send'.
--
-- See 'send' for additional discussion.
data SendAfterFinal = SendAfterFinal CallStack HttpTrailers
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Received malformed message
--
-- This is thrown when the peer sends a malformed message
data RecvMalformed =
    -- | Parser returned an error
    RecvMalformed String

    -- | Not all data was consumed
  | RecvTrailingData
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Connection to the peer was disconnected
data Disconnected = Disconnected {
      disconnectedAt     :: CallStack
    , disconnectedReason :: SomeException
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | This particular peer sent trailers only
--
-- This exception is thrown when trying to send messages to or receive messages
-- to peer which has indicated no messages are expected.
data TrailersOnly = TrailersOnly
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data DebugMsg s r =
    -- | Thread sending messages is awaiting a message
    PeerSendAwaitMsg

    -- | Thread sending message will send a message
  | PeerSendMsg (StreamElem HttpTrailers s)

    -- | Receive thread requires data
    --
    -- We also record the data already received
  | PeerNeedsData BS.Lazy.ByteString

    -- | Receive thread received a message
  | PeerRecvMsg r

    -- | Receive thread received the trailers
  | PeerRecvFinal HttpTrailers
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Auxiliary http2
-------------------------------------------------------------------------------}

fromHeaderTable :: HPACK.HeaderTable -> [HTTP.Header]
fromHeaderTable = map (first HPACK.tokenKey) . fst

