module Network.GRPC.Util.HTTP2.TLS (
    -- * HTTP2 over TLS
    allocTlsConfig
  , freeTlsConfig
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.IORef
import Foreign (mallocBytes)
import Network.HPACK qualified as HPACK
import Network.Socket
import Network.TLS qualified as TLS
import System.TimeManager qualified as TimeManager

-- Despite the import of @.Server@, this config works for clients also
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Util.AccumulatedByteString (AccumulatedByteString)
import Network.GRPC.Util.AccumulatedByteString qualified as AccBS

{-------------------------------------------------------------------------------
  HTTP2 over TLS
-------------------------------------------------------------------------------}

-- | Make configuration for TLS (server or client)
--
-- Adapted from 'HTTP2.allocSimpleConfig'.
allocTlsConfig :: Socket -> TLS.Context -> HPACK.BufferSize -> IO HTTP2.Config
allocTlsConfig sock tlsContext bufSize = do
    writeBuffer <- mallocBytes bufSize
    leftoverRef <- newIORef Nothing
    timeManager <- TimeManager.initialize 30_000_000
    sockName    <- getSocketName sock
    peerName    <- getPeerName sock
    return HTTP2.Config {
        confWriteBuffer       = writeBuffer
      , confBufferSize        = bufSize
      , confSendAll           = TLS.sendData tlsContext . BS.Lazy.fromStrict
      , confReadN             = tlsReadN tlsContext leftoverRef . fromIntegral
      , confPositionReadMaker = HTTP2.defaultPositionReadMaker
      , confTimeoutManager    = timeManager
      , confMySockAddr        = sockName
      , confPeerSockAddr      = peerName
      }

freeTlsConfig :: TLS.Context -> HTTP2.Config -> IO ()
freeTlsConfig tlsContext cfg = do
    TLS.bye tlsContext
    HTTP2.freeSimpleConfig cfg

-- | Read @n@ bytes
tlsReadN ::
     TLS.Context
  -> IORef (Maybe Strict.ByteString)  -- ^ Leftover data
  -> Word                             -- ^ Number of bytes to read (@n@)
  -> IO Strict.ByteString
tlsReadN tlsContext leftoverRef n =
    go . AccBS.init =<< readIORef leftoverRef
  where
    -- Precondition: precondition (2) of 'AccBS.splitAt'
    go :: AccumulatedByteString -> IO Strict.ByteString
    go !acc
      | AccBS.length acc < n = go =<< AccBS.append acc <$> TLS.recvData tlsContext
      | otherwise            = do
          let (bs, acc') = AccBS.splitAt n acc
          writeIORef leftoverRef acc'
          return bs
