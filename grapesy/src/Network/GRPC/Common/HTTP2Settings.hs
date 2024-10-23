-- | Settings and parameters pertaining to HTTP\/2
--
-- Intended for unqualified import.

module Network.GRPC.Common.HTTP2Settings
  ( HTTP2Settings(..)
  , defaultHTTP2Settings
  ) where

import Data.Default.Class
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

      -- | Enable @TCP_NODELAY@
      --
      -- Send out TCP segments as soon as possible, even if there is only a
      -- small amount of data.
      --
      -- When @TCP_NODELAY@ is /NOT/ set, the TCP implementation will wait to
      -- send a TCP segment to the receiving peer until either (1) there is
      -- enough data to fill a certain minimum segment size or (2) we receive an
      -- ACK from the receiving peer for data we sent previously. This adds a
      -- network roundtrip delay to every RPC message we want to send (to
      -- receive the ACK). If the peer uses TCP delayed acknowledgement, which
      -- will typically be the case, then this delay will increase further
      -- still; default for delayed acknowledgement is 40ms, thus resulting in a
      -- theoretical maximum of 25 RPCs/sec.
      --
      -- We therefore enable TCP_NODELAY by default, so that data is sent to the
      -- peer as soon as we have an entire gRPC message serialized and ready to
      -- send (we send the data to the TCP layer only once an entire message is
      -- written, or the @http2@ write buffer is full).
      --
      -- Turning this off /could/ improve throughput, as fewer TCP segments will
      -- be needed, but you probably only want to do this if you send very few
      -- very large RPC messages. In gRPC this is anyway discouraged, because
      -- gRPC messages do not support incremental (de)serialization; if you need
      -- to send large amounts of data, it is preferable to split these into
      -- many, smaller, gRPC messages; this also gives the application the
      -- possibility of reporting on data transmission progress.
      --
      -- TL;DR: leave this at the default unless you know what you are doing.
    , http2TcpNoDelay :: Bool

      -- | Set @SO_LINGER@ to a value of 0
      --
      -- Instead of following the normal shutdown sequence to close the TCP
      -- connection, this will just send a @RST@ packet and immediately discard
      -- the connection, freeing the local port.
      --
      -- This should /not/ be enabled in the vast majority of cases. It is only
      -- useful in specific scenarios, such as stress testing, where resource
      -- (e.g. port) exhaustion is a greater concern than protocol adherence.
      -- Even in such scenarios scenarios, it probably only makes sense to
      -- enable this option on the client since they will be using a new
      -- ephemeral port for each connection (unlike the server).
      --
      -- TL;DR: leave this at the default unless you know what you are doing.
    , http2TcpAbortiveClose :: Bool

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
      -- 'maxBound' (effectively turning off protection against ping flooding).
    , http2OverridePingRateLimit :: Maybe Int

      -- | Empty DATA frame rate limit
      --
      -- This setting is specific to the [@http2@
      -- package's](https://hackage.haskell.org/package/http2) implementation of
      -- the HTTP\/2 specification. In particular, the library imposes a rate
      -- limit for empty DATA frames as a security measure against
      -- [CVE-2019-9518](https://www.cve.org/CVERecord?id=CVE-2019-9518). By
      -- default, it sets this limit at 4 frames/second. If you find yourself
      -- being disconnected from a gRPC peer because that peer is sending too
      -- many empty DATA frames (you will see an
      -- [EnhanceYourCalm](https://hackage.haskell.org/package/http2-5.1.2/docs/Network-HTTP2-Client.html#t:ErrorCode)
      -- exception, corresponding to the
      -- [ENHANCE_YOUR_CALM](https://www.rfc-editor.org/rfc/rfc9113#ErrorCodes)
      -- HTTP\/2 error code), you may wish to increase this limit. If you are
      -- connecting to a peer that you trust, you can set this limit to
      -- 'maxBound' (effectively turning off protection against empty DATA frame
      -- flooding).
    , http2OverrideEmptyFrameRateLimit :: Maybe Int

      -- | SETTINGS frame rate limit
      --
      -- This setting is specific to the [@http2@
      -- package's](https://hackage.haskell.org/package/http2) implementation of
      -- the HTTP\/2 specification. In particular, the library imposes a rate
      -- limit for SETTINGS frames as a security measure against
      -- [CVE-2019-9515](https://www.cve.org/CVERecord?id=CVE-2019-9515). By
      -- default, it sets this limit at 4 frames/second. If you find yourself
      -- being disconnected from a gRPC peer because that peer is sending too
      -- many SETTINGS frames (you will see an
      -- [EnhanceYourCalm](https://hackage.haskell.org/package/http2-5.1.2/docs/Network-HTTP2-Client.html#t:ErrorCode)
      -- exception, corresponding to the
      -- [ENHANCE_YOUR_CALM](https://www.rfc-editor.org/rfc/rfc9113#ErrorCodes)
      -- HTTP\/2 error code), you may wish to increase this limit. If you are
      -- connecting to a peer that you trust, you can set this limit to
      -- 'maxBound' (effectively turning off protection against SETTINGS frame
      -- flooding).
    , http2OverrideSettingsRateLimit :: Maybe Int

      -- | Reset (RST) frame rate limit
      --
      -- This setting is specific to the [@http2@
      -- package's](https://hackage.haskell.org/package/http2) implementation of
      -- the HTTP\/2 specification. In particular, the library imposes a rate
      -- limit for RST frames as a security measure against
      -- [CVE-2023-44487](https://www.cve.org/CVERecord?id=CVE-2023-44487). By
      -- default, it sets this limit at 4 frames/second. If you find yourself
      -- being disconnected from a gRPC peer because that peer is sending too
      -- many empty RST frames (you will see an
      -- [EnhanceYourCalm](https://hackage.haskell.org/package/http2-5.1.2/docs/Network-HTTP2-Client.html#t:ErrorCode)
      -- exception, corresponding to the
      -- [ENHANCE_YOUR_CALM](https://www.rfc-editor.org/rfc/rfc9113#ErrorCodes)
      -- HTTP\/2 error code), you may wish to increase this limit. If you are
      -- connecting to a peer that you trust, you can set this limit to
      -- 'maxBound' (effectively turning off protection against RST frame
      -- flooding).
    , http2OverrideRstRateLimit :: Maybe Int
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
      http2MaxConcurrentStreams        = defMaxConcurrentStreams
    , http2StreamWindowSize            = defInitialStreamWindowSize
    , http2ConnectionWindowSize        = defInitialConnectionWindowSize
    , http2TcpAbortiveClose            = False
    , http2TcpNoDelay                  = True
    , http2OverridePingRateLimit       = Just 100
    , http2OverrideEmptyFrameRateLimit = Nothing
    , http2OverrideSettingsRateLimit   = Nothing
    , http2OverrideRstRateLimit        = Nothing
    }
  where
    defMaxConcurrentStreams        = 128
    defInitialStreamWindowSize     = 256 * 1024        -- 256KiB
    defInitialConnectionWindowSize = 2   * 1024 * 1024 -- 2MiB

instance Default HTTP2Settings where
  def = defaultHTTP2Settings
