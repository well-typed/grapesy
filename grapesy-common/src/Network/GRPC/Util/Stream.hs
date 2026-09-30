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
  Internal auxiliary
-------------------------------------------------------------------------------}

wrapClientDisconnected :: HasCallStack => IO a -> IO a
wrapClientDisconnected = catchAndWrap $ \err -> ClientDisconnected err Nothing

wrapServerDisconnected :: HasCallStack => IO a -> IO a
wrapServerDisconnected = catchAndWrap $ \err -> ServerDisconnected err Nothing
