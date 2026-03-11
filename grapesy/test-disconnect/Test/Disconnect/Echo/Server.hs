module Test.Disconnect.Echo.Server (
    handleEcho
  ) where

import Data.Proxy
import Data.Word

import Network.GRPC.Common
import Network.GRPC.Server        qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary

import Test.Disconnect.Echo.RPC

-- | Echos any input
handleEcho :: Proxy (Echo rpc) -> Server.Call (Echo rpc) -> IO ()
handleEcho _ call =
    loop
  where
    loop :: IO ()
    loop = do
        inp <- Server.Binary.recvInput @Word64 call
        case inp of
          StreamElem n   -> Server.Binary.sendNextOutput  call n >> loop
          FinalElem  n _ -> Server.Binary.sendFinalOutput call (n, NoMetadata)
          NoMoreElems  _ -> Server.sendTrailers           call NoMetadata

