{-# LANGUAGE PatternSynonyms #-}
module Network.GRPC.TCP (
    -- * Server
    readRequest,
    writeResponse,
    -- * Client
    readResponse,
    writeRequest,
) where

import Network.HTTP.Semantics.Server.Internal qualified as Server (Request (..), Response (..))
import Network.HTTP.Semantics.Client.Internal qualified as Client (Request (..), Response (..))
import Network.Socket (Socket)
import Network.GRPC.Util.BufferedSocket

import Data.Binary ()

{-
import Data.Binary (Get)
import Data.ByteString.Lazy qualified as LBS
-}

{-
readMessage :: Socket -> IO LBS.ByteString
readMessage _ = fail "TODO"

_getRequest :: Get Request
_getRequest = error "TODO"

decodeRequest :: LBS.ByteString -> IO Request
decodeRequest _ = fail "TODO"
-}

readRequest :: BufferedSocket -> IO Server.Request
readRequest _ = fail "TODO: readRequest"

writeResponse :: BufferedSocket -> Server.Response -> IO ()
writeResponse _ _ = fail "TODO: writeResponse"

-- client
readResponse :: BufferedSocket -> IO Client.Response
readResponse _ = fail "TODO: readResponse"

writeRequest :: BufferedSocket -> Client.Request -> IO ()
writeRequest _ _ = fail "TODO: writeRequest"
