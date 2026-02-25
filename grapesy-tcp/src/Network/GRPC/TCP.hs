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
import Network.HTTP.Semantics (OutObj (..), InpObj (..), OutBody (..), OutBodyIface (..), TokenHeaderTable, TokenHeaderList, ValueTable, toToken, InpBody)
import Network.HTTP.Semantics qualified as HTTP
import Network.HTTP.Semantics.Client.Internal qualified as Client (Request (..), Response (..))
import Network.HTTP.Semantics.Server.Internal qualified as Server (Request (..), Response (..))
import Network.HTTP.Types (Header)

import Network.GRPC.Util.Imports
import Network.GRPC.Util.BufferedSocket

-------------------------------------------------------------------------------
-- Message
-------------------------------------------------------------------------------

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
            trailers <- readIORef trailersRef
            last' <- trailers Nothing
            case last' of
                HTTP.Trailers t -> do
                    putStrLn $ "sending TRAILERS: " ++ show t
                    sendHeaders bsock t

                HTTP.NextTrailersMaker _next ->
                    putStrLn "ERROR: wrong trailersmaker"

            -- TODO

  where
    iface :: IORef HTTP.TrailersMaker -> OutBodyIface
    iface trailersRef = OutBodyIface
        { outBodyUnmask    = id
        , outBodyPush      = writeBuilderTrailers bsock trailersRef
        , outBodyPushFinal = \b -> do
            -- debug
            putStrLn "outBodyPushFinal"

            -- send last builder-chunk
            writeBuilderTrailers bsock trailersRef b
            sendWord32be bsock 0

        , outBodyCancel    = \_ -> fail "cancel"
        , outBodyFlush     = return ()
        }

sendHeaders :: BufferedSocket -> [Header] -> IO ()
sendHeaders bsock headers = writeBuilder bsock headersBuilder
  where
    headers' :: [(ByteString, ByteString)]
    headers' = map (first CI.original) headers

    headersBuilder :: BSB.Builder
    headersBuilder = Binary.Put.execPut (Binary.put headers')


-- | write builder as single frame.
writeBuilder :: BufferedSocket -> BSB.Builder -> IO ()
writeBuilder bsock builder
    | len <= 0  = return ()
    | otherwise = do
        putStrLn $ "writeBuilder len: " ++ show len
        sendWord32be bsock (fromIntegral len) -- TODO: add size check
        sendLBS      bsock lbs
  where
    lbs = BSB.toLazyByteString builder
    len = LBS.length lbs

writeBuilderTrailers :: BufferedSocket -> IORef HTTP.TrailersMaker -> BSB.Builder -> IO ()
writeBuilderTrailers bsock trailersRef builder
    | null chunks = return ()
    | otherwise = do
        t <- readIORef trailersRef
        loop t chunks
  where
    lbs = BSB.toLazyByteString builder
    len = LBS.length lbs
    chunks = LBS.toChunks lbs

    loop :: HTTP.TrailersMaker -> [ByteString] -> IO ()
    loop t [] = do
        -- at the end write trailersmaker to the reference
        writeIORef trailersRef t
    loop t (c:cs) = do
        -- debug
        putStrLn $ "sending chunk: " ++ show c

        -- send chunk
        sendWord32be bsock (fromIntegral (BS.length c))
        sendBS bsock c

        -- next trailersmaker
        t' <- t (Just c) >>= \case
            HTTP.NextTrailersMaker next -> do
                putStrLn "writeBuilderTrailers got next trailersmaker"
                return next
            res@(HTTP.Trailers ts) -> do
                putStrLn $ "writeBuilderTrailers: got trailers " ++ show ts
                return (\_ -> return res)

        -- loop with rest of the chunks
        loop t' cs

-------------------------------------------------------------------------------
-- Read object
-------------------------------------------------------------------------------

readInpObj :: BufferedSocket -> IO InpObj
readInpObj bsock = do
    headers' <- readHeaders bsock
    putStrLn $ "read headers: " ++ show headers'

    -- ioref for trailers
    -- TODO: actually read trailers.
    trailersRef <- newIORef (Just (mkTokenHeaderTable headers')) -- TODO: for trailers we just copy headers for now.

    -- read the length of first body chunk.
    bodyLen <- recvWord32be bsock
    lenRef <- newIORef bodyLen

    -- read body
    let body :: InpBody -- IO (ByteString, Bool)
        body
            | bodyLen == 0 = do
                fail "read trailers"
                return (mempty, True)
            | otherwise = do
                -- read the chunk size
                bodyLen' <- readIORef lenRef
                lbs <- recvLBS bsock (fromIntegral bodyLen')
                putStrLn $ "body: " ++ show lbs

                -- read the size of the next chunk
                nextLen <- recvWord32be bsock
                if nextLen == 0
                then do
                    trailers <- readHeaders bsock
                    putStrLn $ "read trailers " ++ show trailers
                    writeIORef trailersRef (Just (mkTokenHeaderTable trailers))
                    return (LBS.toStrict lbs, True)
                else do
                    writeIORef lenRef nextLen
                    return (LBS.toStrict lbs, False)

    return InpObj
        { inpObjHeaders  = mkTokenHeaderTable headers'
        , inpObjBodySize = Nothing
        , inpObjTrailers = trailersRef
        , inpObjBody     = body
        }

readHeaders :: BufferedSocket -> IO [(ByteString, ByteString)]
readHeaders bsock = do
    len <- recvWord32be bsock
    putStrLn $ "readInptObj len " ++ show len
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
