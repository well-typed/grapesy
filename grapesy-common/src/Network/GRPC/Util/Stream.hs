{-# LANGUAGE CPP #-}

module Network.GRPC.Util.Stream (
    -- * Streams
    OutputStream(..)
  , writeChunk
  , writeChunkFinal
  , flush
  , InputStream(..)
  , getChunk
  , getTrailers
    -- * Exceptions
  , ClientDisconnected(..)
  , ServerDisconnected(..)
  , wrapServerDisconnected
  , wrapClientDisconnected
  ) where

import Control.Exception
import Data.Binary.Builder (Builder)
import Data.ByteString qualified as Strict (ByteString)
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Common.Exception
import Network.GRPC.Util.Imports

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
  Exceptions
-------------------------------------------------------------------------------}

-- | Client disconnected unexpectedly
data ClientDisconnected = ClientDisconnected {
      -- | The exception as reported by the transport (typically @http2@)
      clientDisconnectedException :: ExactException

      -- | Backtrace, /if/ different
      --
      -- In most cases we wrap the underlying exception the moment it arises.
      -- When this happens, there is no meaningful difference between the
      -- backtrace associated with the exception and the backtrace to where we
      -- wrap it.
      --
      -- Sometimes, however, transport exceptions are thrown asynchronously. For
      -- example, @http2@ will throw 'KilledByThreadManager' to server handlers
      -- when a client disconnects. In this case the backtrace of the exception
      -- and where we catch it might be quite different.
    , clientDisconnectedBacktrace :: Maybe Backtraces
    }
  deriving stock (Generic, Show)
  deriving anyclass ToExceptionDoc

-- | Server disconnected unexpectedly
--
-- See also 'ClientDisconnected' for additional discussion.
data ServerDisconnected = ServerDisconnected {
      -- | The exception as reported by the transport (typically @http2@)
      serverDisconnectedException :: ExactException

      -- | Backtrace, /if/ different
      --
      -- As discussed in 'clientDisconnectedBacktrace', normally we there is
      -- no meaningful difference between the backtrace of the exception and the
      -- backtrace to where we wrap it. An example where they /are/ different is
      -- when the entire connection to the server is closed, and we notice this
      -- elsewhere and close individual RPCs on that connection; the backtrace
      -- of the latter will be different to the former.
    , serverDisconnectedBacktrace :: Maybe Backtraces
    }
  deriving stock (Generic, Show)
  deriving anyclass ToExceptionDoc

#if MIN_VERSION_base(4,20,0)
-- See discussion of 'clientDisconnectedBacktrace'
instance Exception ClientDisconnected where backtraceDesired _ = False
instance Exception ServerDisconnected where backtraceDesired _ = False
#else
instance Exception ClientDisconnected
instance Exception ServerDisconnected
#endif

wrapClientDisconnected :: HasCallStack => IO a -> IO a
wrapClientDisconnected = catchAndWrap $ \err -> ClientDisconnected err Nothing

wrapServerDisconnected :: HasCallStack => IO a -> IO a
wrapServerDisconnected = catchAndWrap $ \err -> ServerDisconnected err Nothing
