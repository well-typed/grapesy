-- | Settings and parameters pertaining to HTTP\/2
--
-- Intended for unqualified import.

module Network.GRPC.Common.HTTP2Settings
  ( HTTP2Settings(..)
  , defaultHTTP2Settings
  ) where

import Data.Default
import Data.Word

-- | HTTP\/2 settings
data HTTP2Settings = HTTP2Settings {
      -- | Maximum number of concurrent active streams
      --
      -- <https://datatracker.ietf.org/doc/html/rfc7540#section-5.1.2>
      http2MaxConcurrentStreams :: Word32

      -- | Window size for streams
      --
      -- <https://datatracker.ietf.org/doc/html/rfc7540#section-6.9.2>
    , http2StreamWindowSize :: Word32

      -- | Connection window size
      --
      -- This value is broadcast via a @WINDOW_UDPATE@ frame at the beginning of
      -- a new connection.
      --
      -- If the consumed window space of all streams exceeds this value, the
      -- sender will stop sending data. Therefore, if this value is less than
      -- @'http2MaxConcurrentStreams' * 'http2StreamWindowSize'@, there is risk
      -- of a control flow deadlock, since the connection window space may be
      -- used up by streams that we are not yet processing before we have
      -- received all data on the streams that we /are/ processing. To reduce
      -- this risk, increase
      -- 'Network.GRPC.Server.Run.serverOverrideNumberOfWorkers' for the server.
      -- See <https://github.com/kazu-yamamoto/network-control/pull/4> for more
      -- information.
    , http2ConnectionWindowSize :: Word32

      -- | Ping rate limit
      --
      -- This setting is specific to the [@http2@
      -- package's](https://hackage.haskell.org/package/http2) implementation of
      -- the HTTP\/2 specification. In particular, the library imposes a ping
      -- rate limit as a security measure against
      -- [CVE-2019-9512](https://www.cve.org/CVERecord?id=CVE-2019-9512). By
      -- default (as of version 5.1.2) it sets this limit at 10 pings/second. If
      -- you find yourself being disconnected from a gRPC peer because that peer
      -- is sending too many pings (you will see an
      -- [EnhanceYourCalm](https://hackage.haskell.org/package/http2-5.1.2/docs/Network-HTTP2-Client.html#t:ErrorCode)
      -- exception, corresponding to the
      -- [ENHANCE_YOUR_CALM](https://www.rfc-editor.org/rfc/rfc9113#ErrorCodes)
      -- HTTP\/2 error code), you may wish to increase this limit. If you are
      -- connecting to a peer that you trust, you can set this limit to
      -- 'maxBound' (effectively turning off protecting against ping flooding).
    , http2OverridePingRateLimit :: Maybe Int
    }
  deriving (Show)

-- | Default HTTP\/2 settings
--
-- [Section 6.5.2 of the HTTP\/2
-- specification](https://datatracker.ietf.org/doc/html/rfc7540#section-6.5.2)
-- recommends that the @SETTINGS_MAX_CONCURRENT_STREAMS@ parameter be no smaller
-- than 100 "so as not to unnecessarily limit parallelism", so we default to
-- 128.
--
-- The default initial stream window size (corresponding to the
-- @SETTINGS_INITIAL_WINDOW_SIZE@ HTTP\/2 parameter) is 64KB.
--
-- The default connection window size is 128 * 64KB to avoid the control flow
-- deadlock discussed at 'http2ConnectionWindowSize'.
--
-- The ping rate limit imposed by the [@http2@
-- package](https://hackage.haskell.org/package/http2) is overridden to 100
-- PINGs/sec.
defaultHTTP2Settings :: HTTP2Settings
defaultHTTP2Settings = HTTP2Settings {
      http2MaxConcurrentStreams  = defMaxConcurrentStreams
    , http2StreamWindowSize      = defInitialStreamWindowSize
    , http2ConnectionWindowSize  = defMaxConcurrentStreams * defInitialStreamWindowSize
    , http2OverridePingRateLimit = Just 100
    }
  where
    defMaxConcurrentStreams    = 128
    defInitialStreamWindowSize = 1024 * 64

instance Default HTTP2Settings where
  def = defaultHTTP2Settings
