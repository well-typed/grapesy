-- Note: atm this module is used only by Server.Run
module Network.GRPC.Util.HTTP2 (
    -- * Configuration
  withConfigForInsecure,
  withConfigForSecure,
    -- * Settings
  mkServerConfig,
  mkTlsSettings,
  ) where

import Network.GRPC.Util.Imports

import Data.ByteString qualified as Strict (ByteString)
import Foreign (mallocBytes, free)
import Network.HPACK (BufferSize)
import Network.HTTP2.Server qualified as Server
import Network.HTTP2.TLS.Server qualified as Server.TLS
import Network.Socket (Socket, SockAddr)
import Network.Socket qualified as Socket
import Network.Socket.BufferPool qualified as Recv
import Network.Socket.ByteString qualified as Socket
import System.TimeManager qualified as Time (Manager)

import Network.GRPC.Common.HTTP2Settings
import Network.GRPC.Util.TimeManager (disableTimeout)

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Create config to be used with @http2@ (without TLS)
--
-- We do not use @allocSimpleConfig@ from @http2:Network.HTTP2.Server@, but
-- instead create a config that is very similar to the config created by
-- 'allocConfigForSecure'.
withConfigForInsecure ::
     Time.Manager
  -> Socket
  -> (Server.Config -> IO a)
  -> IO a
withConfigForInsecure mgr sock k = do
    -- @recv@ does not provide a way to deallocate a buffer pool, and
    -- @http2-tls@ (in @freeServerConfig@) does not attempt to deallocate it.
    -- We follow suit here.
    pool   <- Recv.newBufferPool readBufferLowerLimit readBufferSize
    mysa   <- Socket.getSocketName sock
    peersa <- Socket.getPeerName sock
    withConfig
      mgr
      (Socket.sendAll sock)
      (Recv.receive sock pool)
      mysa
      peersa
      k
  where
    -- Use the defaults from @http2-tls@
    readBufferLowerLimit, readBufferSize :: Int
    readBufferLowerLimit = Server.TLS.settingsReadBufferLowerLimit Server.TLS.defaultSettings
    readBufferSize       = Server.TLS.settingsReadBufferSize       Server.TLS.defaultSettings

-- | Create config to be used with @http2-tls@ (with TLS)
--
-- This is adapted from @allocConfigForServer@ in
-- @http2-tls:Network.HTTP2.TLS.Config@.
withConfigForSecure ::
     Time.Manager
  -> Server.TLS.IOBackend
  -> (Server.Config -> IO a)
  -> IO a
withConfigForSecure mgr backend =
    withConfig
      mgr
      (Server.TLS.send         backend)
      (Server.TLS.recv         backend)
      (Server.TLS.mySockAddr   backend)
      (Server.TLS.peerSockAddr backend)

-- | Internal generalization
withConfig ::
     Time.Manager
  -> (Strict.ByteString -> IO ())
  -> Recv.Recv
  -> SockAddr
  -> SockAddr
  -> (Server.Config -> IO a)
  -> IO a
withConfig mgr send recv mysa peersa k =
    bracket (mallocBytes writeBufferSize) free $ \buf -> do
      recvN <- Recv.makeRecvN mempty recv
      k Server.Config {
          confWriteBuffer       = buf
        , confBufferSize        = writeBufferSize
        , confSendAll           = send
        , confReadN             = recvN
        , confPositionReadMaker = Server.defaultPositionReadMaker
        , confTimeoutManager    = mgr
        , confMySockAddr        = mysa
        , confPeerSockAddr      = peersa
        }
  where
    -- This is the default value for @settingsSendBufferSize@ in @http2-tls@
    -- and the default value given in the documentation in @http2@.
    writeBufferSize :: BufferSize
    writeBufferSize = 4096

{-------------------------------------------------------------------------------
  Settings

  NOTE: If we want to override 'HTTP2.TLS.settingsReadBufferLowerLimit' or
  'HTTP2.TLS.settingsReadBufferSize', we should also modify
  'allocConfigForInsecure'.
-------------------------------------------------------------------------------}

mkServerConfig :: HTTP2Settings -> Server.ServerConfig
mkServerConfig http2Settings =
    Server.defaultServerConfig {
        Server.connectionWindowSize = fromIntegral $
          http2ConnectionWindowSize http2Settings
      , Server.settings =
          Server.defaultSettings {
              Server.initialWindowSize = fromIntegral $
                http2StreamWindowSize http2Settings
            , Server.maxConcurrentStreams = Just . fromIntegral $
                http2MaxConcurrentStreams http2Settings
            , Server.pingRateLimit =
                case http2OverridePingRateLimit http2Settings of
                  Nothing    -> Server.pingRateLimit Server.defaultSettings
                  Just limit -> limit
            , Server.emptyFrameRateLimit =
                case http2OverrideEmptyFrameRateLimit http2Settings of
                  Nothing    -> Server.emptyFrameRateLimit Server.defaultSettings
                  Just limit -> limit
            , Server.settingsRateLimit =
                case http2OverrideSettingsRateLimit http2Settings of
                  Nothing    -> Server.settingsRateLimit Server.defaultSettings
                  Just limit -> limit
            , Server.rstRateLimit =
                case http2OverrideRstRateLimit http2Settings of
                  Nothing    -> Server.rstRateLimit Server.defaultSettings
                  Just limit -> limit
            }
      }

-- | Settings for secure server (with TLS)
--
-- NOTE: This overlaps with the values in 'mkServerConfig', and I /think/ we
-- don't actually need this, because we don't use @runWithSocket@ from
-- @http2-tls@ (but rather @runTLSWithSocket@. However, we set them here anyway
-- for completeness and in case @http2-tls@ decides to use them elsewhere.
mkTlsSettings ::
     HTTP2Settings
  -> (String -> IO ())  -- ^ Key logger
  -> Server.TLS.Settings
mkTlsSettings http2Settings keyLogger =
    Server.TLS.defaultSettings {
        Server.TLS.settingsKeyLogger =
          keyLogger
      , Server.TLS.settingsTimeout =
          disableTimeout
      , Server.TLS.settingsConnectionWindowSize = fromIntegral $
          http2ConnectionWindowSize http2Settings
      , Server.TLS.settingsStreamWindowSize = fromIntegral $
          http2StreamWindowSize http2Settings
      , Server.TLS.settingsConcurrentStreams = fromIntegral $
          http2MaxConcurrentStreams http2Settings
      , Server.TLS.settingsPingRateLimit =
          case http2OverridePingRateLimit http2Settings of
            Nothing    -> Server.pingRateLimit Server.defaultSettings
            Just limit -> limit
      , Server.TLS.settingsEmptyFrameRateLimit =
          case http2OverrideEmptyFrameRateLimit http2Settings of
            Nothing    -> Server.emptyFrameRateLimit Server.defaultSettings
            Just limit -> limit
      , Server.TLS.settingsSettingsRateLimit =
          case http2OverrideSettingsRateLimit http2Settings of
            Nothing    -> Server.settingsRateLimit Server.defaultSettings
            Just limit -> limit
      , Server.TLS.settingsRstRateLimit =
          case http2OverrideRstRateLimit http2Settings of
            Nothing    -> Server.rstRateLimit Server.defaultSettings
            Just limit -> limit
      }
