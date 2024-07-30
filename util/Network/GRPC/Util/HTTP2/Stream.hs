module Network.GRPC.Util.HTTP2.Stream (
    -- * Streams
    OutputStream -- opaque
  , writeChunk
  , writeChunkFinal
  , flush
  , InputStream -- opaque
  , getChunk
  , getTrailers
    -- * Server API
  , serverOutputStream
  , serverInputStream
    -- ** Client API
  , clientInputStream
  , clientOutputStream
    -- * Exceptions
  , ClientDisconnected(..)
  , ServerDisconnected(..)
  , wrapStreamExceptionsWith
  ) where

import Control.Exception
import Data.Binary.Builder (Builder)
import Data.ByteString qualified as Strict (ByteString)
import GHC.Stack
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Client qualified as Client
import Network.HTTP2.Server qualified as Server
import Network.HTTP2.Server (OutBodyIface(..))

import Network.GRPC.Util.HTTP2 (fromHeaderTable)

{-------------------------------------------------------------------------------
  Streams
-------------------------------------------------------------------------------}

data OutputStream = OutputStream {
      -- | Write a chunk to the stream
      _writeChunk :: HasCallStack => Builder -> IO ()

      -- | Write the final chunk to the stream
    , _writeChunkFinal :: HasCallStack => Builder -> IO ()

      -- | Flush the stream (send frames to the peer)
    , _flush :: HasCallStack => IO ()
    }

data InputStream = InputStream {
      _getChunk    :: HasCallStack => IO (Strict.ByteString, Bool)
    , _getTrailers :: HasCallStack => IO [HTTP.Header]
    }

{-------------------------------------------------------------------------------
  Wrappers to get the proper CallStack
-------------------------------------------------------------------------------}

writeChunk :: HasCallStack => OutputStream -> Builder -> IO ()
writeChunk = _writeChunk

writeChunkFinal :: HasCallStack => OutputStream -> Builder -> IO ()
writeChunkFinal = _writeChunkFinal

flush :: HasCallStack => OutputStream -> IO ()
flush = _flush

getChunk :: HasCallStack => InputStream -> IO (Strict.ByteString, Bool)
getChunk = _getChunk

getTrailers :: HasCallStack => InputStream -> IO [HTTP.Header]
getTrailers = _getTrailers

{-------------------------------------------------------------------------------
  Server API
-------------------------------------------------------------------------------}

serverInputStream :: Server.Request -> IO InputStream
serverInputStream req = do
    return InputStream {
        _getChunk =
           wrapStreamExceptionsWith ClientDisconnected $
             Server.getRequestBodyChunk' req
      , _getTrailers =
           wrapStreamExceptionsWith ClientDisconnected $
             maybe [] fromHeaderTable <$> Server.getRequestTrailers req
      }

-- | Create output stream
--
-- == Note on the use of Trailers-Only in non-error cases
--
-- If the stream is closed without writing anything, the situation is similar to
-- the gRPC @Trailers-Only@ case, except that we have already sent the initial
-- set of headers. In this case, http2 will (reasonably enough) create an empty
-- DATA frame, and then another HEADERS frame for the trailers. This is conform
-- the gRPC specification, which mandates:
--
-- > Most responses are expected to have both headers and trailers but
-- > Trailers-Only is permitted for calls that produce an immediate error.
--
-- If we compare this to the official Python example @RouteGuide@ server,
-- however, we see that the @Trailers-Only@ case is sometimes also used in
-- non-error cases. An example is @RouteGuide.listFeatures@: when there /are/ no
-- features in the specified rectangle, the server will send no messages back to
-- the client. The example Python server will use the gRPC Trailers-Only case
-- here (and so we must be able to deal with that in our client implementation).
--
-- We do provide this functionality, but only through a specific API (see
-- 'sendTrailersOnly'); when that API is used, we do not make use of this
-- 'OutputStream' abstraction (indeed, we do not stream at all). In streaming
-- cases (the default) we do not make use of @Trailers-Only@.
serverOutputStream :: OutBodyIface -> IO OutputStream
serverOutputStream iface = do
    -- Make sure that http2 does not wait for the first message before sending
    -- the response headers. This is important: the client might want the
    -- initial response metadata before the first message.
    --
    -- This does require some justification; if any of the reasons below is
    -- no longer true, we might need to reconsider:
    --
    -- o The extra cost of this flush is that we might need an additional TCP
    --   packet; no big deal.
    -- o We only create the 'OutputStream' once the user actually initiates the
    --   response, at which point the headers are fixed.
    -- o We do not use an 'OutputStream' at all when we are in the Trailers-Only
    --   case (see discussion above).

    let outputStream = OutputStream {
            _writeChunk = \c ->
               wrapStreamExceptionsWith ClientDisconnected $
                 outBodyPush iface c
          , _writeChunkFinal = \c ->
               wrapStreamExceptionsWith ClientDisconnected $
                 outBodyPushFinal iface c
          , _flush =
               wrapStreamExceptionsWith ClientDisconnected $
                 outBodyFlush iface
          }

    flush outputStream
    return outputStream

{-------------------------------------------------------------------------------
  Client API
-------------------------------------------------------------------------------}

clientInputStream :: Client.Response -> IO InputStream
clientInputStream resp = do
    return InputStream {
        _getChunk =
           wrapStreamExceptionsWith ServerDisconnected $
             Client.getResponseBodyChunk' resp
      , _getTrailers =
           wrapStreamExceptionsWith ServerDisconnected $
             maybe [] fromHeaderTable <$> Client.getResponseTrailers resp
      }

-- | Construct a client 'OutputStream'
--
-- We do not wrap the members of the 'OutputStream' with
-- 'wrapStreamExceptionsWith', since we do this around the entire
-- 'sendMessageLoop'. See the comment for @outboundThread@ in
-- 'Network.GRPC.Util.Session.Client.setupRequestChannel'.
clientOutputStream :: OutBodyIface -> IO OutputStream
clientOutputStream iface =
    return OutputStream {
        _writeChunk = \c ->
          outBodyPush iface c
      , _writeChunkFinal = \c ->
          outBodyPushFinal iface c
      , _flush =
          outBodyFlush iface
      }

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Client disconnected unexpectedly
--
-- /If/ you choose to catch this exception, you are advised to match against
-- the type, rather than against the constructor, and then use the record
-- accessors to get access to the fields. Future versions of @grapesy@ may
-- record more information.
data ClientDisconnected = ClientDisconnected {
      clientDisconnectedException :: SomeException
    , clientDisconnectedCallStack :: CallStack
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Server disconnected unexpectedly
--
-- See comments for 'ClientDisconnected' on how to catch this exception.
data ServerDisconnected = ServerDisconnected {
      serverDisconnectedException :: SomeException
    , serverDisconnectedCallstack :: CallStack
    }
  deriving stock (Show)
  deriving anyclass (Exception)

wrapStreamExceptionsWith ::
     (HasCallStack, Exception e)
  => (SomeException -> CallStack -> e)
  -> IO a -> IO a
wrapStreamExceptionsWith f action =
    action `catch` \err ->
      throwIO $ f err callStack