-- | Trace context
--
-- See documentation of 'TraceContext'.
module Network.GRPC.Spec.TraceContext (
    -- * Definition
    TraceContext(..)
  , TraceId(..)
  , SpanId(..)
  , TraceOptions(..)
    -- ** Serialization
  , buildTraceContext
  , parseTraceContext
  ) where

import Control.Applicative (many)
import Control.Monad.Except
import Data.Binary (Binary(..))
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Get
import Data.Binary.Put qualified as Put
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Base16 qualified as BS.Strict.Base16
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.Default
import Data.Maybe (maybeToList)
import Data.String
import Data.Word

import Network.GRPC.Spec.CustomMetadata

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
--    'TraceOptions'.
--
--   <https://hackage.haskell.org/package/opentelemetry>
--
-- * The Haskell @hs-opentelemetry@ ecosystem defines @SpanContext@, which is
--   the combination of the W3C @traceparent@ header (our 'TraceContext') and
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
  deriving stock (Show, Eq)

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
  deriving stock (Eq)

-- | Span ID
--
-- ID of the caller span (parent). Must be an 8-byte string.
newtype SpanId = SpanId {
      getSpanId :: Strict.ByteString
    }
  deriving stock (Eq)

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
  deriving stock (Show, Eq)

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

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

buildTraceContext :: TraceContext -> BinaryValue
buildTraceContext = BinaryValue . BS.Lazy.toStrict . Binary.encode

parseTraceContext :: MonadError String m => BinaryValue -> m TraceContext
parseTraceContext (BinaryValue bs) =
    case Binary.decodeOrFail (BS.Lazy.fromStrict bs) of
      Right (_, _, ctxt) -> return ctxt
      Left  (_, _, err)  -> throwError err

{-------------------------------------------------------------------------------
  Internal auxiliary: parsing

  <https://github.com/census-instrumentation/opencensus-specs/blob/master/encodings/BinaryEncoding.md>
-------------------------------------------------------------------------------}

instance Binary TraceId where
  put = Put.putByteString . getTraceId
  get = TraceId <$> Get.getByteString 16

instance Binary SpanId where
  put = Put.putByteString . getSpanId
  get = SpanId <$> Get.getByteString 8

instance Binary TraceOptions where
  put = Put.putWord8 . traceOptionsToWord8
  get = traceOptionsFromWord8 =<< Get.getWord8

instance Binary Field where
  put (FieldTraceId tid)  = Put.putWord8 0 <> put tid
  put (FieldSpanId  sid)  = Put.putWord8 1 <> put sid
  put (FieldOptions opts) = Put.putWord8 2 <> put opts

  get = do
      fieldId <- Get.getWord8
      case fieldId of
        0 -> FieldTraceId <$> get
        1 -> FieldSpanId  <$> get
        2 -> FieldOptions <$> get
        _ -> fail $ "Invalid fieldId " ++ show fieldId

instance Binary TraceContext where
  put ctxt = mconcat [
        Put.putWord8 0 -- Version 0
      , foldMap put (traceContextToFields ctxt)
      ]

  get = do
      version <- Get.getWord8
      case version of
        0 -> traceContextFromFields =<< many get
        _ -> fail $ "Invalid version " ++ show version

{-------------------------------------------------------------------------------
  Internal: fields
-------------------------------------------------------------------------------}

data Field =
    FieldTraceId TraceId
  | FieldSpanId SpanId
  | FieldOptions TraceOptions

traceContextToFields :: TraceContext -> [Field]
traceContextToFields (TraceContext tid sid opts) = concat [
      FieldTraceId <$> maybeToList tid
    , FieldSpanId  <$> maybeToList sid
    , FieldOptions <$> maybeToList opts
    ]

traceContextFromFields :: forall m. MonadFail m => [Field] -> m TraceContext
traceContextFromFields = flip go def
  where
    go :: [Field] -> TraceContext -> m TraceContext
    go []     acc = return acc
    go (f:fs) acc =
        case f of
          FieldTraceId tid ->
            case traceContextTraceId acc of
              Nothing -> go fs $ acc{traceContextTraceId = Just tid}
              Just _  -> fail "Multiple TraceId fields"
          FieldSpanId sid ->
            case traceContextSpanId acc of
              Nothing -> go fs $ acc{traceContextSpanId = Just sid}
              Just _  -> fail "Multiple SpanId fields"
          FieldOptions opts ->
            case traceContextOptions acc of
              Nothing -> go fs $ acc{traceContextOptions = Just opts}
              Just _  -> fail "Multiple TraceOptions fields"

{-------------------------------------------------------------------------------
  Internal: dealing with 'TraceOptions'

  We take advantage of the fact that currently only a single option is defined.
  Once we have more than one, this code will be a bit more complicated.
-------------------------------------------------------------------------------}

traceOptionsToWord8 :: TraceOptions -> Word8
traceOptionsToWord8 (TraceOptions False) = 0
traceOptionsToWord8 (TraceOptions True)  = 1

traceOptionsFromWord8 :: MonadFail m => Word8 -> m TraceOptions
traceOptionsFromWord8 0 = return $ TraceOptions False
traceOptionsFromWord8 1 = return $ TraceOptions True
traceOptionsFromWord8 n = fail $ "Invalid TraceOptions " ++ show n
