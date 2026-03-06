{-# LANGUAGE PatternSynonyms #-}
module Network.GRPC.TCP (
    -- * Server
    readRequest,
    writeResponse,
    -- * Client
    readResponse,
    writeRequest,
) where

import Data.Coerce (coerce)
import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Network.HTTP.Semantics.Server.Internal qualified as Server (Request (..), Response (..))
import Network.HTTP.Semantics.Client.Internal qualified as Client (Request (..), Response (..))
import Network.HTTP.Semantics (OutObj (..), InpObj (..), OutBody (..), OutBodyIface (..))
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Builder qualified as BSB
import Data.Binary.Put qualified as Binary.Put
import Data.CaseInsensitive qualified as CI

import Network.GRPC.Util.Imports
import Network.GRPC.Util.BufferedSocket

{-
readMessage :: Socket -> IO LBS.ByteString
readMessage _ = fail "TODO"

_getRequest :: Get Request
_getRequest = error "TODO"

decodeRequest :: LBS.ByteString -> IO Request
decodeRequest _ = fail "TODO"
-}

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
writeOutObj bsock OutObj { outObjHeaders = headers, outObjBody = body, outObjTrailers = _trailers } = do
    writeBuilder bsock headersBuilder
    case body of
        OutBodyNone                 -> fail "none"
        OutBodyStreaming _body'     -> fail "streaming"
        OutBodyBuilder _builder     -> fail "builder"
        OutBodyFile _filespec       -> fail "OutBodyFile" -- TODO: do we need to support this?
        OutBodyStreamingIface body' -> body' iface

  where
    headers' :: [(ByteString, ByteString)]
    headers' = map (first CI.original) headers

    headersBuilder :: BSB.Builder
    headersBuilder = Binary.Put.execPut (Binary.put headers')

    iface :: OutBodyIface
    iface = OutBodyIface
        { outBodyUnmask    = id
        , outBodyPush      = writeBuilder bsock
        , outBodyPushFinal = \b -> writeBuilder bsock b >> sendWord32be bsock 0
        , outBodyCancel    = \_ -> fail "cancel"
        , outBodyFlush     = return ()
        }

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

-------------------------------------------------------------------------------
-- Read object
-------------------------------------------------------------------------------

readInpObj :: BufferedSocket -> IO InpObj
readInpObj bsock = do
    len <- recvWord32be bsock
    putStrLn $ "readInptObj len " ++ show len
    lbs <- recvLBS bsock (fromIntegral len)

    -- decode headers from lbs
    let headers' :: [(ByteString, ByteString)]
        headers' = Binary.decode lbs -- TODO: don't use decode
    print headers'

    -- read body
    fail "readInpObj: TODO read body"
