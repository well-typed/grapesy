module Network.GRPC.Util.TimeManager (
    -- * Timeouts
  TimeManager,
  withTimeManager,
  disableTimeout,
) where

import System.TimeManager qualified as TimeManager

{-------------------------------------------------------------------------------
  Timeouts
-------------------------------------------------------------------------------}

type TimeManager = TimeManager.Manager

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
withTimeManager :: (TimeManager -> IO a) -> IO a
withTimeManager = TimeManager.withManager (disableTimeout * 1_000_000)

-- | Disable timeouts in http2/http2-tls
--
-- A value of 0 (or lower) disables timeouts as of @time-manager-0.2.2@.
disableTimeout :: Int
disableTimeout = 0
