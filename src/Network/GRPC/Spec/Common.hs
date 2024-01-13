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
    -- * Construction
    buildContentType
  , buildMessageEncoding
  , buildMessageAcceptEncoding
    -- * Parsing
  , parseContentType
  , parseMessageEncoding
  , parseMessageAcceptEncoding
  ) where

import Control.Monad
import Control.Monad.Except
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.Foldable (toList)
import Data.List (intersperse, stripPrefix)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Proxy
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec.Compression
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.ByteString
import Network.GRPC.Util.Partial

{-------------------------------------------------------------------------------
  > Content-Type →
  >   "content-type"
  >   "application/grpc"
  >   [("+proto" / "+json" / {custom})]
-------------------------------------------------------------------------------}

buildContentType ::
     IsRPC rpc
  => Proxy rpc
  -> Maybe Strict.ByteString -- ^ Optional override
  -> HTTP.Header
buildContentType proxy mOverride = (
      "content-type"
    , fromMaybe defaultContentType mOverride
    )
  where
    defaultContentType :: Strict.ByteString
    defaultContentType = "application/grpc+" <> serializationFormat proxy

parseContentType :: forall m rpc.
     (MonadError String m, IsRPC rpc)
  => Proxy rpc
  -> HTTP.Header
  -> m ()
parseContentType proxy (name, hdr) = do
    -- See <https://datatracker.ietf.org/doc/html/rfc2045#section-5> for a spec
    -- of the Content-Type header; the gRPC spec however disallows most of what
    -- is technically allowed by this RPC.

    -- The gRPC spec does not allow for quoted strings.
    withoutPrefix <-
      case stripPrefix "application/grpc" hdrAscii of
        Nothing -> err "missing \"application/grpc\" prefix"
        Just remainder -> return remainder

    -- The gRPC spec does not allow for any parameters.
    when (';' `elem` withoutPrefix) $
      err "unexpected parameter"

    -- Check format
    --
    -- The only @format@ we should allow is @serializationFormat proxy@.
    -- However, some non-conforming proxies use formats such as
    -- @application/grpc+octet-stream@. We therefore ignore @format@ here.
    case withoutPrefix of
      []          -> return () -- Accept "application/grpc"
      '+':_format -> return () -- Accept "application/grpc+<format>"
      _otherwise  -> err "invalid subtype"
  where
    -- ASCII justified by <https://www.rfc-editor.org/rfc/rfc7230#section-3.2.4>
    hdrAscii :: String
    hdrAscii = BS.Strict.C8.unpack hdr

    err :: String -> m a
    err reason =
       throwError $ concat [
           "Unexpected value "
         , show hdrAscii
         , " for header "
         , show name
         , ": "
         , reason
         , ". Expected "
         , show ("application/grpc" :: String)
         , " or "
         , show $
                "application/grpc+"
             ++ BS.Strict.C8.unpack (serializationFormat proxy)
         , ", with "
         , show ("application/grpc+{other_format}" :: String)
         , " also accepted."
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