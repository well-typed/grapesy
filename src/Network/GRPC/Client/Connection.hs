{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Client.Connection (
    Connection(..)
  , withConnection
  ) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Network.HPACK qualified as HPACK
import Network.HTTP2.Client qualified as Client
import Network.Run.TCP (runTCPClient)

import Network.GRPC.Client.Connection.Params
import Network.GRPC.Client.Connection.Meta (ConnMeta)
import Network.GRPC.Client.Connection.Meta qualified as ConnMeta
import Network.GRPC.Spec.HTTP2.Connection (Scheme(..), Authority(..))
import Network.GRPC.Spec.HTTP2.Connection qualified as Connection

{-------------------------------------------------------------------------------
  Connection API
-------------------------------------------------------------------------------}

-- | Open connection
--
-- This type is kept abstract (opaque) in the user facing API.
data Connection = Connection {
      -- | Connection parameters
      connParams :: ConnParams

      -- | Information about the open connection
    , connMeta :: TVar ConnMeta

      -- | Send request
      --
      -- This is the function we get from @http2@.
    , connSend :: forall a.
           Client.Request
        -> (Client.Response -> IO a)
        -> IO a
    }

{-------------------------------------------------------------------------------
  Open a new connection
-------------------------------------------------------------------------------}

withConnection :: forall a. ConnParams -> (Connection -> IO a) -> IO a
withConnection connParams k = do
    connMeta <- newTVarIO $ ConnMeta.init
    runTCPClient (       authorityHost $ connAuthority connParams)
                 (show . authorityPort $ connAuthority connParams)
               $ \sock ->
      bracket
          (Client.allocSimpleConfig sock bufferSize)
          Client.freeSimpleConfig $ \conf ->
        Client.run clientConfig conf $ \connSend ->
          k Connection{connParams, connMeta, connSend}
  where
    -- See docs of 'confBufferSize', but importantly: "this value is announced
    -- via SETTINGS_MAX_FRAME_SIZE to the peer."
    --
    -- Value of 4kB is taken from the example code.
    bufferSize :: HPACK.BufferSize
    bufferSize = 4096

    clientConfig :: Client.ClientConfig
    clientConfig = Client.ClientConfig {
          scheme = case connScheme connParams of
                     Http  -> "http"
                     Https -> "https"

          -- The example code does not include the port number here; the RFC
          -- lists it as optional
          -- <https://www.rfc-editor.org/rfc/rfc3986#section-3.2>
        , authority = Connection.authority $ connAuthority connParams

          -- Docs describe this as "How many pushed responses are contained in
          -- the cache". I don't think I really know what this means. Value of
          -- 20 is from the example code.
        , cacheLimit = 20
        }

