{-# LANGUAGE RecordWildCards #-}
module Network.GRPC.Util.BufferedSocket (
    BufferedSocket,
    mkBufferedSocket,
    -- * Receiving
    recvWord32be,
    -- * Sending
    sendBS,
    sendLBS,
) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Word (Word32)
import Data.ByteString.Lazy qualified as LBS
import Network.Socket (Socket)
import Network.Socket.ByteString qualified as Socket

data BufferedSocket = BufSock
    { bsSocket :: !Socket
    }

mkBufferedSocket :: Socket -> IO BufferedSocket
mkBufferedSocket bsSocket = do
    return $! BufSock {..}

-- receiving

recvWord32be :: BufferedSocket -> IO Word32
recvWord32be _ = fail "TODO" 

-- sending

sendBS :: BufferedSocket -> ByteString -> IO ()
sendBS BufSock { bsSocket = s } bs = Socket.sendAll s bs

sendLBS :: BufferedSocket -> LBS.ByteString -> IO ()
sendLBS s lbs = forM_ (LBS.toChunks lbs) $ \chunk -> sendBS s chunk
