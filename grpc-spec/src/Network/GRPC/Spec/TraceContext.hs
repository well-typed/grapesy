-- | Trace context
--
-- See documentation of t'TraceContext'.
module Network.GRPC.Spec.TraceContext (
    -- * Definition
    TraceContext(..)
  , TraceId(..)
  , SpanId(..)
  , TraceOptions(..)
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Base16 qualified as BS.Strict.Base16
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.Default
import Data.String
import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Trace context
--
-- Representation of the \"trace context\" in OpenTelemetry, corresponding
-- directly to the W3C @traceparent@ header.
--
-- References:
--
-- * <https://www.w3.org/TR/trace-context/#traceparent-header>
--   W3C spec
--
-- * <https://github.com/census-instrumentation/opencensus-specs/blob/master/encodings/BinaryEncoding.md>
--   Binary format used for the @grpc-trace-bin@ header
--
-- * <https://github.com/open-telemetry/opentelemetry-specification/issues/639>
--   Current status of the binary encoding.
--
-- Relation to Haskell OpenTelemetry implementations:
--
-- * The Haskell @opentelemetry@ package calls this a @SpanContext@, but
--    provides no binary @PropagationFormat@, and does not support
--    t'TraceOptions'.
--
--   <https://hackage.haskell.org/package/opentelemetry>
--
-- * The Haskell @hs-opentelemetry@ ecosystem defines @SpanContext@, which is
--   the combination of the W3C @traceparent@ header (our t'TraceContext') and
--   the W3C @tracestate@ header (which we do not support). It too does not
--   support the @grpc-trace-bin@ binary format.
--
--   <https://github.com/iand675/hs-opentelemetry>
--   <https://hackage.haskell.org/package/hs-opentelemetry-propagator-w3c>
data TraceContext = TraceContext {
      traceContextTraceId  :: Maybe TraceId
    , traceContextSpanId   :: Maybe SpanId
    , traceContextOptions  :: Maybe TraceOptions
    }
  deriving stock (Show, Eq, Generic)

instance Default TraceContext where
  def = TraceContext {
        traceContextTraceId = Nothing
      , traceContextSpanId  = Nothing
      , traceContextOptions = Nothing
      }

-- | Trace ID
--
-- The ID of the whole trace forest. Must be a 16-byte string.
newtype TraceId = TraceId {
      getTraceId :: Strict.ByteString
    }
  deriving stock (Eq, Generic)

-- | Span ID
--
-- ID of the caller span (parent). Must be an 8-byte string.
newtype SpanId = SpanId {
      getSpanId :: Strict.ByteString
    }
  deriving stock (Eq, Generic)

-- | Tracing options
--
-- The flags are recommendations given by the caller rather than strict rules to
-- follow for 3 reasons:
--
-- * Trust and abuse.
-- * Bug in caller
-- * Different load between caller service and callee service might force callee
--   to down sample.
data TraceOptions = TraceOptions {
      -- | Sampled
      --
      -- When set, denotes that the caller may have recorded trace data. When
      -- unset, the caller did not record trace data out-of-band.
      traceOptionsSampled :: Bool
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Show instances for IDs

  We follow the W3C spec and show these as base16 strings.
-------------------------------------------------------------------------------}

instance Show TraceId where
  show (TraceId tid) =
      show . BS.Strict.Char8.unpack $
        BS.Strict.Base16.encode tid

instance IsString TraceId where
  fromString str =
      case BS.Strict.Base16.decode (BS.Strict.Char8.pack str) of
        Left  err -> error err
        Right tid -> TraceId tid

instance Show SpanId where
  show (SpanId tid) =
      show . BS.Strict.Char8.unpack $
        BS.Strict.Base16.encode tid

instance IsString SpanId where
  fromString str =
      case BS.Strict.Base16.decode (BS.Strict.Char8.pack str) of
        Left  err -> error err
        Right tid -> SpanId tid

