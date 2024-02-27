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
-- Intended for unqualified import.
module Network.GRPC.Spec.Common (
    -- * Content type
    ContentType(..)
  , buildContentType
  , parseContentType
    -- * Message encoding
  , buildMessageEncoding
  , buildMessageAcceptEncoding
  , parseMessageEncoding
  , parseMessageAcceptEncoding
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
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec.Compression
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Unknown
import Network.GRPC.Util.ByteString
import Network.GRPC.Util.Partial

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
    -- This corresponds to @application/grpc+format@, where @format@ is given
    -- by 'serializationFormat'
    ContentTypeDefault

    -- | Override the content type
    --
    -- Depending on the choice of override, this may or may not be conform spec.
    -- See <https://datatracker.ietf.org/doc/html/rfc2045#section-5> for a spec
    -- of the Content-Type header; the gRPC spec however disallows most of what
    -- is technically allowed by this RPC.
  | ContentTypeOverride Strict.ByteString
  deriving stock (Show, Eq)

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