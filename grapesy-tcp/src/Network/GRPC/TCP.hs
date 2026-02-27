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

readRequest :: Socket -> IO Server.Request
readRequest _ = fail "TODO: readRequest"

writeResponse :: Socket -> Server.Response -> IO ()
writeResponse _ _ = fail "TODO: writeResponse"

-- client
readResponse :: Socket -> IO Client.Response
readResponse _ = fail "TODO: readResponse"

writeRequest :: Socket -> Client.Request -> IO ()
writeRequest _ _ = fail "TODO: writeRequest"
