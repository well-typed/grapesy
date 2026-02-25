module Network.GRPC.TCP (
    readMessage,
    decodeRequest,
    writeResponse,
) where

import Network.HTTP.Semantics.Server.Internal (Request (..), Response (..))
import Network.Socket (Socket)
import Data.Binary (Get)
import Data.ByteString.Lazy qualified as LBS

readMessage :: Socket -> IO LBS.ByteString
readMessage _ = fail "TODO"

_getRequest :: Get Request
_getRequest = error "TODO"

decodeRequest :: LBS.ByteString -> IO Request
decodeRequest _ = fail "TODO"

writeResponse :: Socket -> Response -> IO ()
writeResponse _ _ = fail "TODO"
