-- | Internal state of an open RPC call
--
-- Intended for uqnqualified import.
module Network.GRPC.Client.OpenCall (
    -- * Definition
    IsFinal(..)
  , Trailers(..)
  , OpenCall(..)
  , InboundQ
  , OutboundQ
    -- * Construction
  , openCall
  , closeCall
    -- * Logging
  , LogRPC(..)
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Tracer
import Data.Bifunctor
import Data.ByteString qualified as BS.Strict
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy
import Data.Maybe (fromMaybe)
import Data.SOP
import GHC.Stack
import Network.HPACK qualified as HPACK
import Network.HPACK.Token qualified as HPACK
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Client qualified as Client

import Network.GRPC.Spec
import Network.GRPC.Spec.HTTP2
import Network.GRPC.Spec.HTTP2 qualified as HTTP2
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Thread
import Control.Concurrent

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Mark a input send over a request as final
data IsFinal = Final | NotFinal
  deriving stock (Show, Eq)

-- | Response trailers
--
-- 'Trailers' are a
-- [HTTP2 concept](https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.3):
-- they are HTTP headers that are sent /after/ the content body. For example,
-- imagine the server is streaming a file that it's reading from disk; it could
-- use trailers to give the client an MD5 checksum when streaming is complete.
data Trailers = Trailers {
      grpcStatus  :: Word
    , grpcMessage :: Maybe String
    }
  deriving stock (Show, Eq)

-- | State of the call
--
-- This type is kept abstract (opaque) in the public facing API.
data OpenCall rpc = OpenCall {
      -- | One-place buffer for inputs to be sent to the server
      --
      -- The use of a one-place buffer introduce back-pressure: we cannot
      -- produce inputs faster than that we can send them to the peer.
      callOutbound :: TVar (ThreadState (OutboundQ rpc))

      -- | One-place buffer for responses to the request
      --
      -- Like in 'requestInputQ', the use of a one-place buffer introduces
      -- back-pressure, but now on the peer: they cannot send responses to us
      -- faster than we can process them.
    , callInbound :: TVar (ThreadState (InboundQ rpc))
    }

type OutboundQ rpc = TMVar (IsFinal, Maybe (Input rpc))
type InboundQ  rpc = TMVar (Either Trailers (Output rpc))

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

openCall :: forall rpc.
     IsRPC rpc
  => Tracer IO (LogRPC rpc)
  -> (forall a. Client.Request -> (Client.Response -> IO a) -> IO a)
  -> RequestMeta
  -> rpc
  -> IO (OpenCall rpc)
openCall tracer sendRequest meta rpc = do
    callOutbound <- newTVarIO ThreadNotStarted
    callInbound  <- newTVarIO ThreadNotStarted

    let call :: OpenCall rpc
        call = OpenCall{callOutbound, callInbound}

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
                HTTP2.method
                (HTTP2.path rpc)
                (HTTP2.buildRequestHeaders meta rpc)
                (\push flush ->
                    threadBody callOutbound newEmptyTMVarIO $ \outboundQ ->
                      sendInputs tracer meta rpc push flush outboundQ
                )

    _inboundThread <- forkIO $
      -- We are careful to call 'threadBody' as the first thing we do in the
      -- thread; this ensures that if the call to 'sendRequest' itself is taking
      -- a long time, an invocation to 'closeCall' (e.g. due to a timeout) will
      -- can interrupt this setup process.
      threadBody callInbound newEmptyTMVarIO $ \inboundQ -> do
        -- Any exceptions that we catch on the outside here /must/ come from the
        -- setup phase, because the threads install their own exception handlers.
        setup <- try $ sendRequest req $ \resp -> do
                         processResponseHeaders resp
                         recvOutputs tracer rpc resp inboundQ
        case setup of
          Right () -> return ()
          Left e   -> closeCall call $ Just e

    return call

-- | Close an open RPC call
--
-- Under normal circumstances this should only be called once the final input
-- has been sent and the final output has been received.
closeCall :: OpenCall rpc -> HasCallStack => Maybe SomeException -> IO ()
closeCall OpenCall{callOutbound, callInbound} e = do
    cancelThread callOutbound $ toException $ CallClosed callStack e
    cancelThread callInbound  $ toException $ CallClosed callStack e

processResponseHeaders :: Client.Response -> IO ()
processResponseHeaders resp = do
    -- TODO: We should figure out the various error codes
    -- <https://grpc.github.io/grpc/core/md_doc_statuscodes.html>
    -- not sure if there is a better link.
    --
    -- It seems that if there is an error (for example, serialization
    -- error), we get INTERNAL 13 as grpc-status?
    --
    -- TODO: We need to keep some state to remember what the peer's
    -- supported compression algorithms is.
     let status = Client.responseStatus resp
     case HTTP.statusCode <$> status of
       Just 200   -> return ()
       _otherwise -> throwIO $ ResponseInvalidStatus status

     let contentLength = Client.responseBodySize resp
     case contentLength of
       Nothing -> return ()
       Just l  -> throwIO $ ResponseUnexpectedContentLength l

     -- TODO: We should make the headers available as part of the request
     hdrs <- case HTTP2.parseResponseHeaders (getResponseHeaders resp) of
               Left  err  -> throwIO $ ResponseInvalidHeaders err
               Right hdrs -> return hdrs
     return ()

-- | The RPC call was closed using 'closeCall'.
--
-- Also records the 'CallStack' to the call to 'closeCall'.
data CallClosed = CallClosed CallStack (Maybe SomeException)
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Internal thread: send inputs to the peer
-------------------------------------------------------------------------------}

sendInputs :: forall rpc.
     IsRPC rpc
  => Tracer IO (LogRPC rpc)
  -> RequestMeta
  -> rpc
  -> (BS.Builder -> IO ())  -- ^ Push input
  -> IO ()                  -- ^ Flush
  -> TMVar (IsFinal, Maybe (Input rpc))
  -> IO ()
sendInputs tracer meta rpc push flush q = loop
  where
    loop :: IO ()
    loop = do
        traceWith tracer $ WaitingForMessage

        (isFinal, input) <- atomically $ takeTMVar q
        traceWith tracer $ SendInput input isFinal

        case input of
          Nothing -> return ()
          Just i  -> push $ HTTP2.buildLengthPrefixedMessage meta rpc i

        -- When we return from this thread, the final message will be
        -- marked with the END_STREAM flag.
        unless (isFinal == Final) $ flush >> loop

{-------------------------------------------------------------------------------
  Internal thread: receive outputs from the peer
-------------------------------------------------------------------------------}

recvOutputs :: forall rpc.
     IsRPC rpc
  => Tracer IO (LogRPC rpc)
  -> rpc
  -> Client.Response
  -> TMVar (Either Trailers (Output rpc))
  -> IO ()
recvOutputs tracer rpc resp q = loop Nothing BS.Lazy.empty
  where
    loop ::
         Maybe MessagePrefix  -- Message prefix for next message, if known
      -> BS.Lazy.ByteString   -- Accumulated unconsumed bytes
      -> IO ()
    loop mPrefix acc = do
        traceWith tracer $ WaitingForResponseChunk mPrefix acc
        chunk <- Client.getResponseBodyChunk resp
        if BS.Strict.null chunk then
          if BS.Lazy.null acc then do
            trailers <- getResponseTrailers resp

            case HTTP2.parseResponseTrailers trailers of
              Left err ->
                throwIO $ ResponseInvalidTrailers err
              Right parsed -> do
                atomically $ putTMVar q $ Left (mkTrailers parsed)
                -- Terminate only once the final trailers have been read.
                -- This ensures that we don't put the thread into 'ThreadDone'
                -- state too early.
                atomically $ do
                  trailersRead <- isEmptyTMVar q
                  unless trailersRead $ retry
          else
            throwIO $ ResponseTrailingData acc
        else do
          let acc' = BS.Lazy.fromChunks $ BS.Lazy.toChunks acc ++ [chunk]
          case HTTP2.parseLengthPrefixedMessage rpc mPrefix acc' of
            InsufficientBytes mPrefix' acc'' ->
              loop mPrefix' acc''
            ParseError err ->
              throwIO $ ResponseParseError err
            Parsed output acc'' -> do
              atomically $ putTMVar q $ Right output
              loop Nothing acc''

-- | Remote peer sent an invalid response
--
-- If this happens, it's indicative of a bad server (or a bug in @grapesy@..).
-- It is /not/ indicative of a bug in client code.
data InvalidResponse =
    -- | Server sent a HTTP status other than 200 OK
    --
    -- TODO: The spec explicitly disallows this; not sure what something like
    -- an invalid path would result in though.
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
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Internal auxiliary: dealing with the header table
-------------------------------------------------------------------------------}

getResponseHeaders :: Client.Response -> [HTTP.Header]
getResponseHeaders =
      fromHeaderTable
    . Client.responseHeaders

getResponseTrailers :: Client.Response -> IO [HTTP.Header]
getResponseTrailers =
      fmap (fromMaybe [] . fmap fromHeaderTable)
    . Client.getResponseTrailers

fromHeaderTable :: HPACK.HeaderTable -> [HTTP.Header]
fromHeaderTable = map (first HPACK.tokenKey) . fst

mkTrailers :: ResponseTrailers I -> Trailers
mkTrailers trailers = Trailers {
      grpcStatus  = unI $ responseGrpcStatus  trailers
    , grpcMessage = unI $ responseGrpcMessage trailers
    }

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data LogRPC rpc =
    WaitingForMessage
  | SendInput (Maybe (Input rpc)) IsFinal
  | WaitingForResponseChunk (Maybe MessagePrefix) Lazy.ByteString

deriving instance IsRPC rpc => Show (LogRPC rpc)

