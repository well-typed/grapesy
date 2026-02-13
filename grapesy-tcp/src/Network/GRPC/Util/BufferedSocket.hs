{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Network.GRPC.Util.BufferedSocket (
    BufferedSocket,
    mkBufferedSocket,
    -- * Receiving
    recvWord32be,
    recvLBS,
    -- * Sending
    sendWord32be,
    sendBS,
    sendLBS,
    -- sendBuilder,
) where

import Control.Monad (forM_)
import Control.Monad.Primitive (RealWorld)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Primitive.ByteArray (MutableByteArray, newPinnedByteArray, writeByteArray, readByteArray, mutableByteArrayContents, copyMutableByteArray)
import Data.Word (Word32)
import Network.Socket (Socket, recvBuf)
import Foreign.Ptr (plusPtr)
import Network.Socket.ByteString qualified as Socket
import Data.Binary.Put qualified as Put

-- for readWord8ArrayAsWord32#
import Data.Primitive.ByteArray (MutableByteArray (..))
import GHC.Exts (readWord8ArrayAsWord32#, Int (..), mutableByteArrayContents#)
import GHC.Word (Word32 (..))
import GHC.IO (IO (..))
import Data.Word (byteSwap32)
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (..))

import Data.ByteString.Internal (ByteString (..))

-------------------------------------------------------------------------------
-- BufferedSocket
-------------------------------------------------------------------------------

data BufferedSocket = BufferedSocket
    { bsSocket :: !Socket
    , bsBuffer :: {-# UNPACK #-} !(MutableByteArray RealWorld)
    }

-- | (read) buffer size
bufferSize :: Int
bufferSize = 4096
{-# INLINE bufferSize #-}

dataOffset :: Int
dataOffset = 8 + 8

fullBufferSize :: Int
fullBufferSize = dataOffset + 2 * bufferSize

mkBufferedSocket :: Socket -> IO BufferedSocket
mkBufferedSocket bsSocket = do
    bsBuffer <- newPinnedByteArray fullBufferSize
    writeU bsBuffer 0
    writeV bsBuffer 0
    return $! BufferedSocket {..}

readU :: MutableByteArray RealWorld -> IO Int
readU b = readByteArray b 0

readV :: MutableByteArray RealWorld -> IO Int
readV b = readByteArray b 1

writeU :: MutableByteArray RealWorld -> Int -> IO ()
writeU b u = writeByteArray b 0 u

writeV :: MutableByteArray RealWorld -> Int -> IO ()
writeV b v = writeByteArray b 1 v

-------------------------------------------------------------------------------
-- Receiving
-------------------------------------------------------------------------------

recvMoreData :: BufferedSocket -> Int -> Int -> IO Int
recvMoreData BufferedSocket { bsSocket, bsBuffer } _u v
    -- if there's a bufferSize data left in the buffer size,
    -- we can receive
    | v < bufferSize
    = do
        let ptr = plusPtr (mutableByteArrayContents bsBuffer) (dataOffset + v)
        i <- recvBuf bsSocket ptr bufferSize
        putStrLn $ "recvMoreData got " ++ show i
        writeV bsBuffer (v + i)
        return i

    | otherwise
    = fail "TODO: recvMoreData - no buffer to read"

recvWord32be :: BufferedSocket -> IO Word32
recvWord32be socket@BufferedSocket { bsBuffer } = do
    u <- readU bsBuffer
    v <- readV bsBuffer

    if v - u >= 4
    then do
        w <- readWord8ArrayAsWord32 bsBuffer (dataOffset + u)
        print (u, v, w, byteSwapNetwork32 w)
        let w' = byteSwapNetwork32 w
        writeU bsBuffer (u + 4)
        return w'
    else do
        i <- recvMoreData socket u v
        if i <= 0
        then fail "recvWord32be: EOF"
        else recvWord32be socket

recvLBS :: BufferedSocket -> Int -> IO LBS.ByteString
recvLBS _ len | len <= 0 = return LBS.empty
recvLBS socket@BufferedSocket { bsBuffer } len = do
    u <- readU bsBuffer
    v <- readV bsBuffer
    putStrLn $ "recvLBS " ++ show (u, v, len)

    -- if ther are data in the buffer
    if u < v
    then do
        putStrLn "nice1"
        -- check if there's enough data, if there are we are done.
        if len <= v - u
        then do
            putStrLn "nice2"
            ba <- newPinnedByteArray len
            copyMutableByteArray ba 0 bsBuffer (dataOffset + u) len
            writeU bsBuffer (u + len)
            putStrLn "nice3"
            return (LBS.fromStrict (pinnedMutableByteArrayToBS ba len))

        -- if there aren't, create first chunk and go into the loop
        else do
            let bufferLen = v - u
            ba <- newPinnedByteArray bufferLen
            copyMutableByteArray ba 0 bsBuffer (dataOffset + u) bufferLen
            writeU bsBuffer 0
            writeV bsBuffer 0
            recvLBS' socket (pinnedMutableByteArrayToBS ba bufferLen  :) (len - bufferLen)

    else do
        i <- recvMoreData socket u v
        if i <= 0
        then fail "recvWord32be: EOF"
        else recvLBS socket len

recvLBS' :: BufferedSocket -> ([ByteString] -> [ByteString]) -> Int -> IO LBS.ByteString
recvLBS' socket@BufferedSocket { bsBuffer } acc len = do
    -- we need more data
    i <- recvMoreData socket 0 0
    if i <= 0
    then fail "recvLBS': EOF"
    else do
        u <- readU bsBuffer
        v <- readV bsBuffer
        let bufferLen = v - u
        if len < bufferLen
        then do
            -- if we got enough
            end <- copyToBS bsBuffer (dataOffset + u) len
            writeU bsBuffer (u + len)
            return (LBS.fromChunks (acc [end]))
        else do
            -- if we didn't, we make a chunk and loop
            chunk <- copyToBS bsBuffer (dataOffset + u) bufferLen
            writeU bsBuffer 0
            writeV bsBuffer 0
            recvLBS' socket (acc . (chunk :)) (len - bufferLen)


-- | Copy part of MutableByteArray (offset, length) to strict ByteString
copyToBS :: MutableByteArray RealWorld -> Int -> Int -> IO ByteString
copyToBS src off len = do
    dst <- newPinnedByteArray len
    copyMutableByteArray dst 0 src off len
    return $! pinnedMutableByteArrayToBS dst len

-------------------------------------------------------------------------------
-- Sending
-------------------------------------------------------------------------------

sendWord32be :: BufferedSocket -> Word32 -> IO ()
sendWord32be bsock w32 = do
    print ('x', w32)
    sendLBS bsock (Put.runPut (Put.putWord32be w32))

sendBS :: BufferedSocket -> ByteString -> IO ()
sendBS BufferedSocket { bsSocket = s } bs = Socket.sendAll s bs

sendLBS :: BufferedSocket -> LBS.ByteString -> IO ()
sendLBS s lbs = forM_ (LBS.toChunks lbs) $ \chunk -> sendBS s chunk

-------------------------------------------------------------------------------
-- primitive
-------------------------------------------------------------------------------

-- TODO: move to own module
readWord8ArrayAsWord32 :: MutableByteArray RealWorld -> Int -> IO Word32
readWord8ArrayAsWord32 (MutableByteArray ba) (I# i) = IO
    (\s -> case readWord8ArrayAsWord32# ba i s of
        (# s', w #) -> (# s', W32# w #))

byteSwapNetwork32 :: Word32 -> Word32
byteSwapNetwork32 = byteSwap32 -- TODO: fix me on big endian machines

-- | See implementation of 'GHC.Foreign.Ptr.mallocForeignPtrBytes'
pinnedMutableByteArrayToBS :: MutableByteArray RealWorld -> Int -> ByteString
pinnedMutableByteArrayToBS (MutableByteArray ba) len =
    BS (ForeignPtr (mutableByteArrayContents# ba) (PlainPtr ba)) len
