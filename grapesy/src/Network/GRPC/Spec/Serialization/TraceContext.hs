{-# OPTIONS_GHC -Wno-orphans #-}

module Network.GRPC.Spec.Serialization.TraceContext (
    buildTraceContext
  , parseTraceContext
  ) where

import Control.Applicative (many)
import Control.Monad.Except
import Data.Binary (Binary(..))
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Get
import Data.Binary.Put qualified as Put
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.Default.Class
import Data.Maybe (maybeToList)
import Data.Word

import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

buildTraceContext :: TraceContext -> Strict.ByteString
buildTraceContext = BS.Lazy.toStrict . Binary.encode

parseTraceContext :: MonadError String m => Strict.ByteString -> m TraceContext
parseTraceContext bs =
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
