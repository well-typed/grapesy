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
  , wrapStreamExceptionsWith
  ) where

import Network.GRPC.Util.Imports

import Data.Binary.Builder (Builder)
import Data.ByteString qualified as Strict (ByteString)
import Network.HTTP.Types qualified as HTTP

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
