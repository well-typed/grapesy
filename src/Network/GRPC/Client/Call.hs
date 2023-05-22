-- | Open (ongoing) RPC call
--
-- Intended for uqnqualified import.
module Network.GRPC.Client.Call (
    -- * Definition
    Call -- opaque
    -- * Construction
  , openCall
  , closeCall
    -- * Open call
  , sendInput
  , recvOutput
    -- * Logging
  , LogRPC(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Tracer
import Data.Bifunctor
import Data.ByteString qualified as BS.Strict
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import GHC.Stack
import Network.HPACK qualified as HPACK
import Network.HPACK.Token qualified as HPACK
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Client qualified as Client

import Network.GRPC.Client.Connection
import Network.GRPC.Client.Connection.Meta qualified as ConnMeta
import Network.GRPC.Client.Connection.Meta qualified as Meta
import Network.GRPC.Client.Connection.Params
import Network.GRPC.Client.Logging
import Network.GRPC.Compression (Compression(..), CompressionId)
import Network.GRPC.Compression qualified as Compression
import Network.GRPC.Spec
import Network.GRPC.Spec.LengthPrefixed (MessagePrefix)
import Network.GRPC.Spec.LengthPrefixed qualified as LengthPrefixed
import Network.GRPC.Spec.PseudoHeaders
import Network.GRPC.Spec.Request qualified as Request
import Network.GRPC.Spec.Response qualified as Response
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Thread

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | State of the call
--
-- This type is kept abstract (opaque) in the public facing API.
data Call rpc = Call {
      -- | Thread sending messages to the peer
      callOutbound :: TVar (ThreadState (OutboundQ rpc))

      -- | Thread receiving messages from the peer
    , callInbound :: TVar (ThreadState (InboundQ rpc))

      -- | Set to 'True' once we sent the final message
    , callSentFinal :: TVar Bool

      -- | Set to 'False' once we received the 'Trailers'
    , callRecvTrailers :: TVar Bool
    }

-- | Outbound queue
--
-- The use of a one-place buffer introduce back-pressure: we cannot produce
-- inputs faster than that we can send them to the peer.
type OutboundQ rpc = TMVar (IsFinal, Maybe (Input rpc))

-- | Inbound queue
--
-- Like in 'OutboundQ', the use of a one-place buffer introduces back-pressure,
-- but now on the peer: they cannot send responses to us faster than we can
-- process them.
type InboundQ rpc = TMVar (Either Trailers (Output rpc))

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

getRequestHeaders :: Connection -> CallParams -> IO RequestHeaders
getRequestHeaders Connection{connParams, connMeta} requestParams = do
    currentMeta <- atomically $ readTVar connMeta
    return RequestHeaders{
        requestParams
      , requestCompression       = fromMaybe Compression.identity $
                                  Meta.outboundCompression currentMeta
      , requestAcceptCompression = connCompression connParams
      }

openCall :: forall rpc.
     IsRPC rpc
  => Connection -> CallParams -> rpc -> IO (Call rpc)
openCall conn@Connection{connSend, connParams} perCallParams rpc = do
    callOutbound     <- newTVarIO ThreadNotStarted
    callInbound      <- newTVarIO ThreadNotStarted
    callSentFinal    <- newTVarIO False
    callRecvTrailers <- newTVarIO False

    let call :: Call rpc
        call = Call{callOutbound, callInbound, callSentFinal, callRecvTrailers}

        resourceHeaders :: RawResourceHeaders
        resourceHeaders = buildResourceHeaders $ ResourceHeaders {
              resourceMethod = Post
            , resourcePath   = rpcPath rpc
            }

    requestHeaders <- getRequestHeaders conn perCallParams

    -- Control flow and exception handling here is a little tricky.
    --
    -- * 'sendRequest' (coming from @http2@) will itself spawn a separate thread
    --   to deal with sending inputs to the peer (here, that is 'sendInputs').
    --
    -- * As the last thing it does, 'sendRequest' will then call its
    --   continuation argument in whatever thread it was called. Here, that
    --   continuation argument is 'recvOutputs' (dealing with outputs sent by
    --   the peer), which we want to run in a separate thread also. We must
    --   therefore call 'sendRequest' in a newly forked thread.
    --
    --   (We /could/ spawn the thread inside the continuation to 'sendRequest',
    --   but its signature suggests some kind of scoping, so spawning it on
    --   the outside seems more forwards compatible.)
    --
    -- * We need to decide what to do with any exceptions that might arise in
    --   these threads. One option might be to rethrow those exceptions to the
    --   parent thread, but this presupposes a certain level of hygiene in
    --   client code: if the client code spawns threads of their own, and shares
    --   the open RPC call between them, then we rely on the client code to
    --   propagate the exception further.
    --
    --   We therefore choose a different approach: we do not try to propagate
    --   the exception at all, but instead ensure that any (current or future)
    --   call to 'sendInput' or 'recvOutput' on the open RPC call will result
    --   in an exception when the respective threads have died.
    --
    -- * The clean way to shut down the 'sendInputs' thread is by writing
    --   @(IsFinal, ..)@ to 'callInputQ'. If a connection is closed /before/
    --   the final message is sent, we consider this to be an indication that
    --   something has gone wrong, and we must forcefully terminate the outbound
    --   thread. (TODO: We should document that.)
    --
    --   Similarly, when we close the connection before we have received all
    --   outputs, we must terminate the inbound thread. (TODO: @http2@ does not
    --   offer an API for indicating that we want to ignore further output.
    --   We should check that this does not result in memory leak if the server
    --   keeps sending data and we're not listening.)
    --
    --   We therefore need the thread IDs of the outbound and inbound threads.
    --   For the inbound thread this is easy, as we spawn it here, but for the
    --   outbound thread it's trickier, as this thread is spawned under the hood
    --   by @http2@. For uniformity's sake, we treat both cases the same, and
    --   have the threads record their own ID as part of their state.
    --
    -- * This leaves one final problem to deal with: if setting up the request
    --   /itself/ throws an exception, one or both threads might never get
    --   started. To avoid client code blocking indefinitely we will therefore
    --   catch any exceptions that arise during the call setup, and use the to
    --   change 'OutboundInitializing' to 'OutboundException' and similarly
    --   'InboundInitializing' to 'InboundException'.
    let req :: Client.Request
        req = Client.requestStreaming
                (rawMethod resourceHeaders)
                (rawPath resourceHeaders)
                (Request.buildHeaders requestHeaders rpc)
                (\push flush ->
                    threadBody callOutbound newEmptyTMVarIO $ \outboundQ -> do
                      sendInputs
                        connParams
                        (requestCompression requestHeaders)
                        rpc
                        push
                        flush
                        outboundQ
                )

    _inboundThread <- forkIO $
      -- We are careful to call 'threadBody' as the first thing we do in the
      -- thread; this ensures that if the call to 'sendRequest' itself is taking
      -- a long time, an invocation to 'closeCall' (e.g. due to a timeout) will
      -- can interrupt this setup process.
      threadBody callInbound newEmptyTMVarIO $ \inboundQ -> do
        -- Any exceptions that we catch on the outside here /must/ come from the
        -- setup phase, because the threads install their own exception handlers.
        setup <- try $ connSend req $ \resp -> do
          mCompr <- processResponseHeaders conn rpc resp
          case mCompr of
            Left  trailers -> atomically $ putTMVar inboundQ $ Left trailers
            Right compr    -> recvOutputs connParams compr rpc resp inboundQ
        case setup of
          Right () -> return ()
          Left e   -> closeCall call $ Just e

    return call

{-------------------------------------------------------------------------------
  Closing an open RPC
-------------------------------------------------------------------------------}

-- | Close an open RPC call
--
-- Under normal circumstances this should only be called once the final input
-- has been sent and the final output has been received.
closeCall :: Call rpc -> HasCallStack => Maybe SomeException -> IO ()
closeCall Call{callOutbound, callInbound} e = do
    cancelThread callOutbound $ toException $ CallClosed callStack e
    cancelThread callInbound  $ toException $ CallClosed callStack e

-- | The RPC call was closed using 'closeCall'.
data CallClosed = CallClosed {
      callClosedAt     :: CallStack
    , callClosedReason :: Maybe SomeException
    }
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Open call
-------------------------------------------------------------------------------}

-- | Send an input to the peer
--
-- This lives in @STM@ for improved composability. For example, if the peer is
-- currently busy then 'sendInput' will block, but you can then use 'orElse' to
-- provide an alternative codepath.
--
-- Calling 'sendInput' again after sending the final message is a bug.
sendInput ::
     Call rpc
  -> IsFinal
  -> Maybe (Input rpc)
  -> STM ()
sendInput Call{callOutbound, callSentFinal} isFinal input = do
    sentFinal <- readTVar callSentFinal

    if sentFinal then
      throwSTM SendAfterFinal
    else do
      q <- getThreadInterface callOutbound

      -- We write to the queue and update 'callSentFinal' in the same STM tx;
      -- this is important to avoid race conditions.
      case isFinal of
        Final    -> writeTVar callSentFinal True
        NotFinal -> return ()

      -- By checking that we haven't sent the final message yet, we know that
      -- this call to 'putMVar' will not block indefinitely: the thread that
      -- sends messages to the peer will get to it eventually (unless it dies,
      -- in which case the thread status will change and the call to
      -- 'getThreadInterface' will be retried).
      putTMVar q (isFinal, input)

-- | Receive an output from the peer
--
-- This lives in @STM@ for improved compositionality. For example, you can wait
-- on multiple clients and see which one responds first.
--
-- Though the types cannot guarantee it, you will receive 'Trailers' once, after
-- the final 'Output'. Calling 'recvOutput' again after receiving 'Trailers' is
-- a bug.
recvOutput :: Call rpc -> STM (Either Trailers (Output rpc))
recvOutput Call{callInbound, callRecvTrailers} = do
    recvTrailers <- readTVar callRecvTrailers

    if recvTrailers then
      throwSTM RecvAfterTrailers
    else do
      q <- getThreadInterface callInbound

      -- By checking that we haven't received the trailers yet, we know that
      -- this call to 'takeTMVar' will not block indefinitely: the thread that
      -- receives messages from the peer will get to it eventually (unless it
      -- dies, in which case the thread status will change and the call to
      -- 'getThreadInterface' will be retried).
      r <- takeTMVar q

      -- We read from the queue and update 'calRecvTrailers' in the same STM tx;
      -- this is important to avoid race conditions.
      case r of
        Left _trailers -> writeTVar callRecvTrailers True
        Right _output  -> return ()

      return r

-- | Thrown by 'sendInput'
--
-- This indicates a bug in client code. See 'sendInput' for discussion.
data SendAfterFinal = SendAfterFinal
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Thrown by 'recvOutput'
--
-- This indicates a bug in client code. See 'recvOutput' for discussion.
data RecvAfterTrailers = RecvAfterTrailers
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Response headers
-------------------------------------------------------------------------------}

-- | Process response headers
--
-- The gRPC specification states that
--
-- > Trailers-Only is permitted for calls that produce an immediate error
--
-- Spec notwithstanding, gRPC servers can also respond with @Trailers-Only@ in
-- non-error cases, simply indicating that the server considers the conversation
-- over. To distinguish, we look at 'trailerGrpcStatus': in case of 'GrpcOk' we
-- return the 'Trailers', and in the case of 'GrpcError' we throw
-- 'RpcImmediateError'.
processResponseHeaders ::
     IsRPC rpc
  => Connection
  -> rpc
  -> Client.Response
  -> IO (Either Trailers Compression)
processResponseHeaders Connection{connMeta, connParams} rpc resp = do
    let status = Client.responseStatus resp
    case HTTP.statusCode <$> status of
      Just 200   -> return ()
      _otherwise -> throwIO $ ResponseInvalidStatus status

    -- The gRPC specification defines
    --
    -- > Response → (Response-Headers *Length-Prefixed-Message Trailers)
    -- >          / Trailers-Only
    --
    -- Furthermore, in prose, it tells us that
    --
    -- 1. For responses end-of-stream is indicated by the presence of the
    --    END_STREAM flag on the last received HEADERS frame that carries
    --    Trailers.
    -- 2. Implementations should expect broken deployments to send non-200 HTTP
    --    status codes in responses as well as a variety of non-GRPC
    --    content-types and to omit Status & Status-Message. Implementations
    --    must synthesize a Status & Status-Message to propagate to the
    --    application layer when this occurs.
    --
    -- This means the only reliable way to determine if we are in the
    -- @Trailers-Only@ case is to see if the stream is terminated after we
    -- received the "trailers" (headers). We don't get direct access to this
    -- information from @http2@, /but/ we are told indirectly: if the stream is
    -- terminated, we are told that the body size is 0 (normally we do not
    -- expect an indication of body size at all); this is independent from any
    -- content-length header (which gRPC does not in fact allow for).

    let contentLength = Client.responseBodySize resp
        headers       = getResponseHeaders resp

    case contentLength of
      Just 0 -> do
        trailers <- parseTrailers rpc headers
        -- Allow for the exception to the specification mentioned above.
        case trailerGrpcStatus trailers of
          GrpcOk      -> return  $ Left trailers
          GrpcError _ -> throwIO $ RpcImmediateError trailers
      Just l ->
        throwIO $ ResponseUnexpectedContentLength l
      Nothing -> do
        -- The common @Response@ case (i.e., not @Trailers-Only@)
        hdrs <- parseHeaders rpc headers

        atomically $ do
          meta <- readTVar connMeta
          case ConnMeta.update (connCompression connParams) hdrs meta of
            Left  err   -> throwSTM err
            Right meta' -> writeTVar connMeta meta'

        case responseCompression hdrs of
          Nothing  -> return $ Right Compression.identity
          Just cid -> case NE.filter
                             ((== cid) . compressionId)
                             (connCompression connParams) of
                        []  -> throwIO $ ResponseUnexpectedCompression cid
                        c:_ -> return $ Right c

-- | Thrown by 'openCall' when the remote node indicates an immediate error
--
-- The gRPC spec defines:
--
-- > Response → (Response-Headers *Length-Prefixed-Message Trailers)
-- >          / Trailers-Only
--
-- and then says in prose: "@Trailers-Only@ is permitted for calls that produce
-- an immediate error".
data RpcImmediateError = RpcImmediateError Trailers
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Internal thread: send inputs to the peer
-------------------------------------------------------------------------------}

sendInputs :: forall rpc.
     IsRPC rpc
  => ConnParams
  -> Compression
  -> rpc
  -> (BS.Builder -> IO ())  -- ^ Push input
  -> IO ()                  -- ^ Flush
  -> TMVar (IsFinal, Maybe (Input rpc))
  -> IO ()
sendInputs ConnParams{connTracer} compr rpc push flush q =
    loop
  where
    loop :: IO ()
    loop = do
        traceWith connTracer $ LogRPC rpc $ WaitingForMessage

        (isFinal, input) <- atomically $ takeTMVar q
        traceWith connTracer $ LogRPC rpc $ SendInput input isFinal

        case input of
          Nothing -> return ()
          Just i  -> push $ LengthPrefixed.build compr rpc i

        -- When we return from this thread, the final message will be
        -- marked with the END_STREAM flag.
        unless (isFinal == Final) $ flush >> loop

{-------------------------------------------------------------------------------
  Internal thread: receive outputs from the peer
-------------------------------------------------------------------------------}

recvOutputs :: forall rpc.
     IsRPC rpc
  => ConnParams
  -> Compression
  -> rpc
  -> Client.Response
  -> TMVar (Either Trailers (Output rpc))
  -> IO ()
recvOutputs ConnParams{connTracer} compr rpc resp q =
    loop Nothing BS.Lazy.empty
  where
    loop ::
         Maybe MessagePrefix  -- Message prefix for next message, if known
      -> BS.Lazy.ByteString   -- Accumulated unconsumed bytes
      -> IO ()
    loop mPrefix acc = do
        traceWith connTracer $ LogRPC rpc $ WaitingForResponseChunk mPrefix acc
        chunk <- Client.getResponseBodyChunk resp
        if BS.Strict.null chunk then
          if BS.Lazy.null acc then do
            trailers <- parseTrailers rpc =<< getResponseTrailers resp
            atomically $ putTMVar q $ Left trailers
          else
            throwIO $ ResponseTrailingData acc
        else do
          let acc' = BS.Lazy.fromChunks $ BS.Lazy.toChunks acc ++ [chunk]
          case LengthPrefixed.parse compr rpc mPrefix acc' of
            LengthPrefixed.InsufficientBytes mPrefix' acc'' ->
              loop mPrefix' acc''
            LengthPrefixed.ParseError err ->
              throwIO $ ResponseParseError err
            LengthPrefixed.Parsed output acc'' -> do
              atomically $ putTMVar q $ Right output
              loop Nothing acc''

-- | Remote peer sent an invalid response
--
-- If this happens, it's indicative of a bad server (or a bug in @grapesy@..).
-- It is /not/ indicative of a bug in client code.
data InvalidResponse =
    -- | Server sent a HTTP status other than 200 OK
    --
    -- The spec explicitly disallows this; if a particular method is not
    -- supported, then the server will respond with HTTP 200 and then a
    -- grpc-status of 'GrpcUnimplemented'.
    ResponseInvalidStatus (Maybe HTTP.Status)

    -- | gRPC does not support the HTTP2 content-length header
  | ResponseUnexpectedContentLength Int

    -- | We received a response header we could not parse
  | ResponseInvalidHeaders String

    -- | We received a trailing header we could not parse
  | ResponseInvalidTrailers String

    -- | When the server indicated end of stream, we still had unparsed data
  | ResponseTrailingData Lazy.ByteString

    -- | We failed to parse an incoming message
  | ResponseParseError String

    -- | Server used an unexpected compression algorithm
  | ResponseUnexpectedCompression CompressionId
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Internal auxiliary: dealing with the header table
-------------------------------------------------------------------------------}

parseHeaders :: IsRPC rpc => rpc -> [HTTP.Header] -> IO ResponseHeaders
parseHeaders rpc headers =
    case Response.parseHeaders rpc headers of
      Left  err    -> throwIO $ ResponseInvalidHeaders err
      Right parsed -> return parsed

parseTrailers :: IsRPC rpc => rpc -> [HTTP.Header] -> IO Trailers
parseTrailers rpc trailers =
    case Response.parseTrailers rpc trailers of
      Left err     -> throwIO $ ResponseInvalidTrailers err
      Right parsed -> return  parsed

getResponseHeaders :: Client.Response -> [HTTP.Header]
getResponseHeaders = fromHeaderTable . Client.responseHeaders

-- | Wrapper around 'Client.getResponseTrailers'
--
-- The underlying function has this proviso: "must be called after
-- getResponseBodyChunk returns an empty".
getResponseTrailers :: Client.Response -> IO [HTTP.Header]
getResponseTrailers =
    fmap (maybe [] fromHeaderTable) . Client.getResponseTrailers

fromHeaderTable :: HPACK.HeaderTable -> [HTTP.Header]
fromHeaderTable = map (first HPACK.tokenKey) . fst

