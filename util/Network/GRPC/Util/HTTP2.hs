{-# LANGUAGE CPP #-}

#include "MachDeps.h"

module Network.GRPC.Util.HTTP2 (
    -- * General auxiliary
    fromHeaderTable
    -- * Configuration
  , withConfigForInsecure
  , withConfigForSecure
    -- * Settings
  , mkServerConfig
  , mkTlsSettings
    -- * Timeouts
  , withTimeManager
  ) where

import Control.Exception
import Data.Bifunctor
import Data.ByteString qualified as Strict (ByteString)
import Data.Maybe (fromMaybe)
import Foreign (mallocBytes, free)
import Network.HPACK (BufferSize)
import Network.HPACK qualified as HPACK
import Network.HPACK.Token qualified as HPACK
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as Server
import Network.HTTP2.TLS.Server qualified as Server.TLS
import Network.Socket (Socket, SockAddr)
import Network.Socket qualified as Socket
import Network.Socket.BufferPool (Recv)
import Network.Socket.BufferPool qualified as Recv
import Network.Socket.ByteString qualified as Socket
import System.TimeManager qualified as Time (Manager)
import System.TimeManager qualified as TimeManager

import Network.GRPC.Common.HTTP2Settings

{-------------------------------------------------------------------------------
  General auxiliary
-------------------------------------------------------------------------------}

fromHeaderTable :: HPACK.TokenHeaderTable -> [HTTP.Header]
fromHeaderTable = map (first HPACK.tokenKey) . fst

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
    def :: Server.TLS.Settings
    def = Server.TLS.defaultSettings

    -- Use the defaults from @http2-tls@
    readBufferLowerLimit, readBufferSize :: Int
    readBufferLowerLimit = Server.TLS.settingsReadBufferLowerLimit def
    readBufferSize       = Server.TLS.settingsReadBufferSize       def

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
  -> Recv
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

mkServerConfig ::
     HTTP2Settings
  -> Maybe Word  -- ^ Override number of workers
  -> Server.ServerConfig
mkServerConfig http2Settings numberOfWorkers =
    Server.defaultServerConfig {
        Server.numberOfWorkers =
          fromMaybe
            (Server.numberOfWorkers Server.defaultServerConfig)
            (fromIntegral <$> numberOfWorkers)
      , Server.connectionWindowSize = fromIntegral $
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
  -> Maybe Word         -- ^ Override number of workers
  -> (String -> IO ())  -- ^ Key logger
  -> Server.TLS.Settings
mkTlsSettings http2Settings numberOfWorkers keyLogger =
    Server.TLS.defaultSettings {
        Server.TLS.settingsKeyLogger =
          keyLogger
      , Server.TLS.settingsTimeout =
          disableTimeout
      , Server.TLS.settingsNumberOfWorkers =
          fromMaybe
            (Server.TLS.settingsNumberOfWorkers Server.TLS.defaultSettings)
            (fromIntegral <$> numberOfWorkers)
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

{-------------------------------------------------------------------------------
  Timeouts
-------------------------------------------------------------------------------}

-- | Allocate time manager (without any actual timeouts)
--
-- The @http2@ ecosystem relies on a time manager for timeouts; we don't use
-- those timeouts (see disableTimeout), but must still provide a time manager
-- for insecure connections. For secure connections the time manager is
-- allocated in 'Network.Run.Timeout.runTCPServerWithSocket' from @network-run@.
-- In that package it allocates a single manager for the entire server (it
-- allocates the manager before calling accept), so we should do the same in the
-- insecure case for better consistency between the two setups; this also avoids
-- the possibility of leaking managers.
withTimeManager :: (Time.Manager -> IO a) -> IO a
withTimeManager = TimeManager.withManager (disableTimeout * 1_000_000)

-- | Work around the fact that we cannot disable timeouts in http2/http2-tls
--
-- TODO: <https://github.com/well-typed/grapesy/issues/123>
-- We need a proper solution for this.
disableTimeout :: Int
disableTimeout =
#if (WORD_SIZE_IN_BITS == 64)
    -- Set a really high timeout to effectively disable timeouts (100 years)
    --
    -- NOTE: We cannot use 'maxBound' here, because this value is multiplied
    -- by @1_000_000@ in 'Network.Run.TCP.Timeout.runTCPServerWithSocket'
    -- (in @network-run@).
    100 * 365 * 24 * 60 * 60
#else
#warning "Timeout for RPC messages is set to 30 minutes on 32-bit systems."
#warning "See https://github.com/kazu-yamamoto/http2/issues/112"
    -- Unfortunately, the same trick does not work on 32-bit systems, where we
    -- simply don't have enough range. The maximum timeout we can support here
    -- is roughly 35 mins. We set it to 30 minutes exactly, to at least provide
    -- a clue if the timeout does hit (1_800_000_000 < 2_147_483_647).
    30 * 60
#endif

