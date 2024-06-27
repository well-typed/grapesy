{-# LANGUAGE OverloadedStrings #-}

-- | Functionality shared between requests and responses
--
-- The following headers are used both in requests and in responses:
--
-- * @Content-Type@
-- * @Message-Encoding@
-- * @Message-Accept-Encoding@
-- * @Custom-Metadata@ (see "Network.GRPC.Spec.CustomMetadata")
--
-- We also define 'MessageType' here, which is currently only used for request
-- headers, but at least morally speaking seems to apply the response just the
-- same.
--
-- Intended for unqualified import.
module Network.GRPC.Spec.Headers.Common (
    -- * Content type
    ContentType(..)
  , buildContentType
  , parseContentType
    -- * Message type
  , MessageType(..)
  , buildMessageType
  , parseMessageType
    -- * Message encoding
  , buildMessageEncoding
  , buildMessageAcceptEncoding
  , parseMessageEncoding
  , parseMessageAcceptEncoding
    -- * Utilities
  , trim
  ) where

import Control.Monad
import Control.Monad.Except
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.Default
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec.Compression
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Unknown
import Network.GRPC.Util.ByteString

{-------------------------------------------------------------------------------
  > Content-Type →
  >   "content-type"
  >   "application/grpc"
  >   [("+proto" / "+json" / {custom})]
-------------------------------------------------------------------------------}

-- | Content type
data ContentType =
    -- | The default content type for this RPC
    --
    -- This is given by 'rpcContentType', and is typically
    -- @application/grpc+format@, where @format@ is @proto@, @json@, .. (see
    -- also 'defaultRpcContentType').
    ContentTypeDefault

    -- | Override the content type
    --
    -- Depending on the choice of override, this may or may not be conform spec.
    -- See <https://datatracker.ietf.org/doc/html/rfc2045#section-5> for a spec
    -- of the Content-Type header; the gRPC spec however disallows most of what
    -- is technically allowed by this RPC.
  | ContentTypeOverride Strict.ByteString
  deriving stock (Show, Eq, Generic)

instance Default ContentType where
  def = ContentTypeDefault

buildContentType ::
     IsRPC rpc
  => Proxy rpc
  -> ContentType
  -> HTTP.Header
buildContentType proxy contentType = (
      "content-type"
    , case contentType of
       ContentTypeDefault    -> defaultContentType
       ContentTypeOverride x -> x
    )
  where
    defaultContentType :: Strict.ByteString
    defaultContentType = rpcContentType proxy

parseContentType :: forall m rpc.
     (MonadError String m, IsRPC rpc)
  => Proxy rpc
  -> HTTP.Header
  -> m ContentType
parseContentType proxy (name, hdr) = do
    if hdr == rpcContentType proxy then
      return ContentTypeDefault
    else do
      -- Headers must be ASCII, justifying the use of BS.Strict.C8.
      -- See <https://www.rfc-editor.org/rfc/rfc7230#section-3.2.4>.
      -- The gRPC spec does not allow for quoted strings.
      withoutPrefix <-
        case BS.Strict.C8.stripPrefix "application/grpc" hdr of
          Nothing -> err "missing \"application/grpc\" prefix"
          Just remainder -> return remainder

      -- The gRPC spec does not allow for any parameters.
      when (';' `BS.Strict.C8.elem` withoutPrefix) $
        err "unexpected parameter"

      -- Check format
      --
      -- The only @format@ we should allow is @serializationFormat proxy@.
      -- However, some non-conforming proxies use formats such as
      -- @application/grpc+octet-stream@. We therefore ignore @format@ here.
      if BS.Strict.C8.null withoutPrefix then
        -- Accept "application/grpc"
        return $ ContentTypeOverride hdr
      else
        case BS.Strict.C8.stripPrefix "+" withoutPrefix of
          Just _format ->
            -- Accept "application/grpc+<format>"
            return $ ContentTypeOverride hdr
          Nothing ->
            err "invalid subtype"
  where
    err :: String -> m a
    err reason =
       throwError $ concat [
           "Unexpected value \""
         , BS.Strict.C8.unpack hdr
         , "\" for header "
         , show name
         , ": "
         , reason
         , ". Expected \""
         , BS.Strict.C8.unpack $
             rpcContentType (Proxy @(UnknownRpc Nothing Nothing))
         , "\" or \""
         , BS.Strict.C8.unpack $
             rpcContentType proxy
         , "\", with \""
         , "application/grpc+{other_format}"
         , "\" also accepted."
         ]

{-------------------------------------------------------------------------------
  > Message-Type → "grpc-message-type" {type name for message schema}
-------------------------------------------------------------------------------}

-- | Message type
data MessageType =
    -- | Default message type for this RPC
    --
    -- This is given by 'rpcMessageType'. For the specific case Protobuf this
    -- is the fully qualified proto message name (and we currently omit the
    -- @grpc-message-type@ header altogether for JSON).
    MessageTypeDefault

    -- | Override the message type
  | MessageTypeOverride Strict.ByteString
  deriving stock (Show, Eq, Generic)

instance Default MessageType where
  def = MessageTypeDefault

buildMessageType ::
     IsRPC rpc
  => Proxy rpc
  -> MessageType
  -> Maybe HTTP.Header
buildMessageType proxy messageType =
    case messageType of
      MessageTypeDefault    -> mkHeader <$> defaultMessageType
      MessageTypeOverride x -> Just $ mkHeader x
  where
    defaultMessageType :: Maybe Strict.ByteString
    defaultMessageType = rpcMessageType proxy

    mkHeader :: Strict.ByteString -> HTTP.Header
    mkHeader = ("grpc-message-type",)

-- | Parse message type
--
-- We do not need the @grpc-message-type@ header in order to know the message
-- type, because the /path/ determines the service and method, and that in turn
-- determines the message type. Therefore, if the value is not what we expect,
-- we merely record this fact ('MessageTypeOverride') but don't otherwise do
-- anything differently.
parseMessageType :: forall rpc.
     IsRPC rpc
  => Proxy rpc
  -> HTTP.Header
  -> MessageType
parseMessageType proxy (_name, given) =
    case rpcMessageType proxy of
      Nothing ->
        -- We expected no message type at all, but did get one
        MessageTypeOverride given
      Just expected ->
        if expected == given
          then MessageTypeDefault
          else MessageTypeOverride given

{-------------------------------------------------------------------------------
  > Message-Encoding → "grpc-encoding" Content-Coding
  > Content-Coding → "identity" / "gzip" / "deflate" / "snappy" / {custom}
-------------------------------------------------------------------------------}

buildMessageEncoding :: CompressionId -> HTTP.Header
buildMessageEncoding compr = (
      "grpc-encoding"
    , serializeCompressionId compr
    )

parseMessageEncoding ::
     MonadError String m
  => HTTP.Header
  -> m CompressionId
parseMessageEncoding (_name, value) =
    return $ deserializeCompressionId value

{-------------------------------------------------------------------------------
  > Message-Accept-Encoding →
  >   "grpc-accept-encoding" Content-Coding *("," Content-Coding)
-------------------------------------------------------------------------------}

buildMessageAcceptEncoding :: NonEmpty CompressionId -> HTTP.Header
buildMessageAcceptEncoding compr = (
      "grpc-accept-encoding"
    , mconcat . intersperse "," . map serializeCompressionId $ toList compr
    )

parseMessageAcceptEncoding ::
     MonadError String m
  => HTTP.Header
  -> m (NonEmpty CompressionId)
parseMessageAcceptEncoding (_name, value) =
      expectAtLeastOne
    . map (deserializeCompressionId . strip)
    . BS.Strict.splitWith (== ascii ',')
    $ value

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

expectAtLeastOne :: forall m a.
     (MonadError String m, Typeable a)
  => [a] -> m (NonEmpty a)
expectAtLeastOne (x : xs) = return (x :| xs)
expectAtLeastOne []       = throwError $ concat [
                                "Expected at least one "
                              , show (typeRep (Proxy @a))
                              ]
{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

-- | Trim leading or trailing whitespace
--
-- We only allow for space and tab, based on
-- <https://www.rfc-editor.org/rfc/rfc9110.html#name-whitespace>.
trim :: Strict.ByteString -> Strict.ByteString
trim = ltrim . rtrim
  where
    ltrim, rtrim :: Strict.ByteString -> Strict.ByteString
    ltrim = BS.Strict.dropWhile    isSpace
    rtrim = BS.Strict.dropWhileEnd isSpace

    isSpace :: Word8 -> Bool
    isSpace x = x == 32 || x == 9
