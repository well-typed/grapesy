{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
module Network.GRPC.TCP (
    -- * Server
    readRequest,
    writeResponse,
    -- * Client
    readResponse,
    writeRequest,
) where

import Data.Array (array)
import Data.Binary qualified as Binary
import Data.Binary.Put qualified as Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Coerce (coerce)
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import Data.Word (Word32)
import Network.HTTP.Semantics (OutObj (..), InpObj (..), OutBody (..), OutBodyIface (..), TokenHeaderTable, TokenHeaderList, ValueTable, toToken, InpBody)
import Network.HTTP.Semantics qualified as HTTP
import Network.HTTP.Semantics.Client.Internal qualified as Client (Request (..), Response (..))
import Network.HTTP.Semantics.Server.Internal qualified as Server (Request (..), Response (..))
import Network.HTTP.Types (Header)

import Network.GRPC.Util.Imports
import Network.GRPC.Util.BufferedSocket

-- #define GRAPESY_TCP_DEBUG

#ifdef GRAPESY_TCP_DEBUG
import System.IO (hPutStrLn, stderr)
debugStrLn :: String -> IO ()
debugStrLn = hPutStrLn stderr
#else
debugStrLn :: String -> IO ()
debugStrLn _ = return ()
#endif

-------------------------------------------------------------------------------
-- Message
-------------------------------------------------------------------------------

-- message (frame) type.
newtype Message = Message Word32
  deriving (Eq, Show)

-- TODO: trailers are sent with MessageHeaders as well.
pattern MessageHeaders :: Message
pattern MessageHeaders = Message 1

pattern MessageData :: Message
pattern MessageData = Message 2

pattern MessageDataLast :: Message
pattern MessageDataLast = Message 3

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

readRequest :: BufferedSocket -> IO Server.Request
readRequest = coerce readInpObj

writeResponse :: BufferedSocket -> Server.Response -> IO ()
writeResponse = coerce writeOutObj

-------------------------------------------------------------------------------
-- Client
-------------------------------------------------------------------------------

readResponse :: BufferedSocket -> IO Client.Response
readResponse = coerce readInpObj

writeRequest :: BufferedSocket -> Client.Request -> IO ()
writeRequest = coerce writeOutObj

-------------------------------------------------------------------------------
-- Write object
-------------------------------------------------------------------------------

writeOutObj :: BufferedSocket -> OutObj -> IO ()
writeOutObj bsock OutObj { outObjHeaders = headers, outObjBody = body, outObjTrailers = trailers } = do
    sendHeaders bsock headers
    trailersRef <- newIORef trailers
    case body of
        OutBodyNone                 -> fail "none"
        OutBodyStreaming _body'     -> fail "streaming"
        OutBodyBuilder _builder     -> fail "builder"
        OutBodyFile _filespec       -> fail "OutBodyFile" -- TODO: do we need to support this?
        OutBodyStreamingIface body' -> do
            -- write body
            body' (iface trailersRef)

            -- now send the trailers
            trailersMaker <- readIORef trailersRef
            last' <- trailersMaker Nothing
            case last' of
                HTTP.Trailers t -> do
                    debugStrLn $ "sending TRAILERS: " ++ show t
                    sendHeaders bsock t

                HTTP.NextTrailersMaker _next ->
                    debugStrLn "ERROR: wrong trailersmaker"

            -- TODO

  where
    iface :: IORef HTTP.TrailersMaker -> OutBodyIface
    iface trailersRef = OutBodyIface
        { outBodyUnmask    = id
        , outBodyPush      = writeBuilderTrailers bsock MessageData trailersRef
        , outBodyPushFinal = \b -> do
            -- debug
            debugStrLn "outBodyPushFinal"

            -- send last builder-chunk
            writeBuilderTrailers bsock MessageDataLast trailersRef b

        , outBodyCancel    = \_ -> fail "cancel"
        , outBodyFlush     = return ()
        }

writeMessage :: BufferedSocket -> Message -> IO ()
writeMessage bsock (Message msg) =
    sendWord32be bsock msg

sendHeaders :: BufferedSocket -> [Header] -> IO ()
sendHeaders bsock headers = writeBuilder bsock MessageHeaders headersBuilder
  where
    headers' :: [(ByteString, ByteString)]
    headers' = map (first CI.original) headers

    headersBuilder :: BSB.Builder
    headersBuilder = Binary.Put.execPut (Binary.put headers')

-- | write builder as single frame.
writeBuilder :: BufferedSocket -> Message -> BSB.Builder -> IO ()
writeBuilder bsock msg builder
    | len <= 0  = return ()
    | otherwise = do
        debugStrLn $ "writeBuilder len: " ++ show len
        writeMessage bsock msg
        sendWord32be bsock (fromIntegral len) -- TODO: add size check
        sendLBS      bsock lbs
  where
    lbs = BSB.toLazyByteString builder
    len = LBS.length lbs

writeBuilderTrailers :: BufferedSocket -> Message -> IORef HTTP.TrailersMaker -> BSB.Builder -> IO ()
writeBuilderTrailers bsock msg trailersRef builder
    | null chunks = return ()
    | otherwise = do
        writeMessage bsock msg
        t <- readIORef trailersRef
        loop t chunks

        -- end chunk stream with zero
        sendWord32be bsock 0
  where
    lbs = BSB.toLazyByteString builder
    -- len = LBS.length lbs
    chunks = filter (not . BS.null) $ LBS.toChunks lbs

    loop :: HTTP.TrailersMaker -> [ByteString] -> IO ()
    loop t [] = do
        -- at the end write trailersmaker to the reference
        writeIORef trailersRef t
    loop t (c:cs) = do
        -- debug
        debugStrLn $ "sending chunk: " ++ show c

        -- send chunk
        sendWord32be bsock (fromIntegral (BS.length c))
        sendBS bsock c

        -- next trailersmaker
        t' <- t (Just c) >>= \case
            HTTP.NextTrailersMaker next -> do
                debugStrLn "writeBuilderTrailers got next trailersmaker"
                return next
            res@(HTTP.Trailers ts) -> do
                debugStrLn $ "writeBuilderTrailers: got trailers " ++ show ts
                return (\_ -> return res)

        -- loop with rest of the chunks
        loop t' cs

-------------------------------------------------------------------------------
-- Read object
-------------------------------------------------------------------------------

readInpObj :: BufferedSocket -> IO InpObj
readInpObj bsock = do
    headers' <- readHeaders bsock
    debugStrLn $ "read headers: " ++ show headers'

    -- ioref for trailers
    trailersRef <- newIORef (Just (mkTokenHeaderTable headers')) -- TODO: for trailers we just copy headers for now.

    -- read body
    let body :: InpBody -- IO (ByteString, Bool)
        body = do
            msg <- readMessageType bsock
            debugStrLn $ "readInpObj got message: " ++ show msg
            unless (msg == MessageData || msg == MessageDataLast) $
                fail $ "unexpected message: " ++ show msg

            let loop :: ([LBS.ByteString] -> [LBS.ByteString]) -> IO LBS.ByteString
                loop !acc = do
                    chunkLen <- recvWord32be bsock
                    debugStrLn $ "readInpObj chunk size" ++ show chunkLen

                    if chunkLen == 0
                    then return (LBS.concat (acc []))
                    else do
                        lbs <- recvLBS bsock (fromIntegral chunkLen)
                        loop (acc . (lbs :))

            lbs <- loop id

            -- eagearly read trailers after MessageDataLast
            when (msg == MessageDataLast) $ do
                trailers <- readHeaders bsock
                debugStrLn $ "read trailers " ++ show trailers
                writeIORef trailersRef (Just (mkTokenHeaderTable trailers))

            return (LBS.toStrict lbs, msg == MessageDataLast)

    return InpObj
        { inpObjHeaders  = mkTokenHeaderTable headers'
        , inpObjBodySize = Nothing
        , inpObjTrailers = trailersRef
        , inpObjBody     = body
        }

readMessageType :: BufferedSocket -> IO Message
readMessageType = coerce recvWord32be

readHeaders :: BufferedSocket -> IO [(ByteString, ByteString)]
readHeaders bsock = do
    msg <- readMessageType bsock
    unless (msg == MessageHeaders) $ fail $ "expecting headers: got" ++ show msg
    len <- recvWord32be bsock
    debugStrLn $ "readInptObj len " ++ show len
    lbs <- recvLBS bsock (fromIntegral len)
    let headers' :: [(ByteString, ByteString)]
        headers' = Binary.decode lbs -- TODO: don't use decode
    return headers'

mkTokenHeaderTable :: [(ByteString, ByteString)] -> TokenHeaderTable
mkTokenHeaderTable headers = (tokenHeader, valueTable)
  where
    tokenHeader :: TokenHeaderList
    tokenHeader = map (first toToken) headers

    valueTable :: ValueTable
    valueTable = array (HTTP.minTokenIx, HTTP.maxTokenIx) $
        -- populate with `Nothing`s.
        [ (ix, Nothing)
        | ix <- [HTTP.minTokenIx .. HTTP.maxTokenIx]
        ] ++
        -- and overwrite with real values
        [ (HTTP.tokenIx token, Just value)
        | (token, value) <- tokenHeader
        ]
