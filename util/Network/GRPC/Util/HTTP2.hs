module Network.GRPC.Util.HTTP2 (
    -- * General auxiliary
    fromHeaderTable
    -- * Configuration
  , allocConfigWithTimeout
  ) where

import Data.Bifunctor
import Network.HPACK (BufferSize)
import Network.HPACK qualified as HPACK
import Network.HPACK.Token qualified as HPACK
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as Server
import Network.Socket
import System.TimeManager qualified as TimeoutManager

{-------------------------------------------------------------------------------
  General auxiliary
-------------------------------------------------------------------------------}

fromHeaderTable :: HPACK.TokenHeaderTable -> [HTTP.Header]
fromHeaderTable = map (first HPACK.tokenKey) . fst

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Adaptation of 'allocSimpleConfig' that allows to specify a timeout
allocConfigWithTimeout :: Socket -> BufferSize -> Int -> IO Server.Config
allocConfigWithTimeout sock confBufferSize timeout = do
    -- Since 'allocSimpleConfig' calls some functions that are not exported,
    -- we use it as-is, and then throw away the 'TimeManager' it created and
    -- override it with our own. We can only do this because none of the other
    -- values in this record depend on the time manager.
    config <- Server.allocSimpleConfig sock confBufferSize
    timeoutManager <- TimeoutManager.initialize $ timeout * 1_000_000
    return config{Server.confTimeoutManager = timeoutManager}
