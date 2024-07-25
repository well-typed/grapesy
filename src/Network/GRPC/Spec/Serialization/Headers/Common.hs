{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.Serialization.Headers.Common (
    -- * Content type
    buildContentType
  , parseContentType
    -- * Message type
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
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy
import Data.Word
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec
import Network.GRPC.Util.ByteString

{-------------------------------------------------------------------------------
  > Content-Type →
  >   "content-type"
  >   "application/grpc"
  >   [("+proto" / "+json" / {custom})]
-------------------------------------------------------------------------------}

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

-- | Parse @content-type@ header
--
-- The gRPC spec mandates different behaviour here for requests and responses:
-- when parsing a request (i.e., on the server), the spec requires that the
-- server responds with @415 Unsupported Media Type@. When parsing a response,
-- however (i.e., on hte client), the spec mandates that we synthesize a
-- gRPC exception. We therefore take a function as parameter to construct the
-- actual error.
parseContentType :: forall m rpc.
     (MonadError (InvalidHeaders GrpcException) m, IsRPC rpc)
  => Proxy rpc
  -> (String -> InvalidHeaders GrpcException)
  -> HTTP.Header
  -> m ContentType
parseContentType proxy invalid (_name, value) = do
    if value == rpcContentType proxy then
      return ContentTypeDefault
    else do
      -- Headers must be ASCII, justifying the use of BS.Strict.C8.
      -- See <https://www.rfc-editor.org/rfc/rfc7230#section-3.2.4>.
      -- The gRPC spec does not allow for quoted strings.
      withoutPrefix <-
        case BS.Strict.C8.stripPrefix "application/grpc" value of
          Nothing        -> err "Missing \"application/grpc\" prefix."
          Just remainder -> return remainder

      -- The gRPC spec does not allow for any parameters.
      when (';' `BS.Strict.C8.elem` withoutPrefix) $
        err "Unexpected parameter."

      -- Check format
      --
      -- The only @format@ we should allow is @serializationFormat proxy@.
      -- However, some non-conforming proxies use formats such as
      -- @application/grpc+octet-stream@. We therefore ignore @format@ here.
      if BS.Strict.C8.null withoutPrefix then
        -- Accept "application/grpc"
        return $ ContentTypeOverride value
      else
        case BS.Strict.C8.stripPrefix "+" withoutPrefix of
          Just _format ->
            -- Accept "application/grpc+<format>"
            return $ ContentTypeOverride value
          Nothing ->
            err "Invalid subtype."
  where
    err :: String -> m a
    err reason = throwError . invalid . concat $ [
          reason
        , " Expected \""
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
     MonadError (InvalidHeaders GrpcException) m
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

parseMessageAcceptEncoding :: forall m.
     MonadError (InvalidHeaders GrpcException) m
  => HTTP.Header
  -> m (NonEmpty CompressionId)
parseMessageAcceptEncoding hdr@(_name, value) =
      atLeastOne
    . map (deserializeCompressionId . strip)
    . BS.Strict.splitWith (== ascii ',')
    $ value
  where
    atLeastOne :: forall a. [a] -> m (NonEmpty a)
    atLeastOne (x : xs) = return (x :| xs)
    atLeastOne []       = throwError $ invalidHeader Nothing hdr $
                            "Expected at least one compresion ID"

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
