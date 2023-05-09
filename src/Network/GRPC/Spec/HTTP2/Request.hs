{-# LANGUAGE OverloadedStrings #-}

-- | Construct HTTP2 requests
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.HTTP2.Request qualified as Request
module Network.GRPC.Spec.HTTP2.Request (
    buildHeaders
  ) where

import Data.ByteString.Base64 qualified as BS.Strict.B64
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.CaseInsensitive qualified as CI
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Version
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Compression (Compression(..))
import Network.GRPC.Spec
import Network.GRPC.Spec.Compression qualified as Compression
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.RPC

import PackageInfo_grapesy qualified as PackageInfo

{-------------------------------------------------------------------------------
  Headers
-------------------------------------------------------------------------------}

-- | Request headers
--
-- > Request-Headers →
-- >   Call-Definition
-- >   *Custom-Metadata
buildHeaders :: IsRPC rpc => AllCallParams -> rpc -> [HTTP.Header]
buildHeaders callParams@AllCallParams{perCallParams} rpc = concat [
      callDefinition callParams rpc
    , map customMetadata $ callCustomMetadata perCallParams
    ]
  where
    customMetadata :: CustomMetadata -> HTTP.Header
    customMetadata (BinaryHeader name value) = (
          CI.mk $ getHeaderName name <> "-bin"
        , BS.Strict.B64.encode value
        )
    customMetadata (AsciiHeader name value) = (
          CI.mk $ getHeaderName name
        , getAsciiValue value
        )

-- | Call definition
--
-- > Call-Definition →
-- >   Method
-- >   Scheme
-- >   Path
-- >   TE
-- >   [Authority]
-- >   [Timeout]
-- >   Content-Type
-- >   [Message-Type]
-- >   [Message-Encoding]
-- >   [Message-Accept-Encoding]
-- >   [User-Agent]
--
-- However, the spec additionally mandates that
--
--   HTTP2 requires that reserved headers, ones starting with ":" appear
--   before all other headers. Additionally implementations should send
--   Timeout immediately after the reserved headers and they should send the
--   Call-Definition headers before sending Custom-Metadata.
--
-- (Relevant part of the HTTP2 spec:
-- <https://www.rfc-editor.org/rfc/rfc7540#section-8.1.2.1>.) This means
-- @TE@ should come /after/ @Authority@ (if using). However, we will not include
-- the reserved headers here /at all/, as they are automatically added by
-- `http2`.
callDefinition :: IsRPC rpc => AllCallParams -> rpc -> [HTTP.Header]
callDefinition = \callParams@AllCallParams{perCallParams} rpc -> catMaybes [
      hdrTimeout <$> callTimeout perCallParams
    , Just $ hdrTe
    , Just $ hdrContentType rpc
    , Just $ hdrMessageType rpc
    , Just $ hdrMessageEncoding (callCompression callParams)
    , Just $ hdrMessageAcceptEncoding (callAcceptCompression callParams)
    , Just $ hdrUserAgent
    ]
  where
    -- > Timeout      → "grpc-timeout" TimeoutValue TimeoutUnit
    -- > TimeoutValue → {positive integer as ASCII string of at most 8 digits}
    -- > TimeoutUnit  → Hour / Minute / Second / Millisecond / Microsecond / Nanosecond
    -- > Hour         → "H"
    -- > Minute       → "M"
    -- > Second       → "S"
    -- > Millisecond  → "m"
    -- > Microsecond  → "u"
    -- > Nanosecond   → "n"
    hdrTimeout :: Timeout -> HTTP.Header
    hdrTimeout (Timeout unit val) = (
          "grpc-timeout"
        , mconcat [
              BS.Strict.C8.pack $ show $ getTimeoutValue val
            , " "
            , case unit of
                Hour        -> "H"
                Minute      -> "M"
                Second      -> "S"
                Millisecond -> "m"
                Microsecond -> "u"
                Nanosecond  -> "n"
            ]
        )

    -- > TE → "te" "trailers" # Used to detect incompatible proxies
    hdrTe :: HTTP.Header
    hdrTe  = ("te", "trailers")

    -- > Content-Type →
    -- >   "content-type"
    -- >   "application/grpc"
    -- >   [("+proto" / "+json" / {custom})]
    hdrContentType :: IsRPC rpc => rpc -> HTTP.Header
    hdrContentType rpc = (
          "content-type"
        , "application/grpc+" <> serializationFormat rpc
        )

    -- > Message-Type → "grpc-message-type" {type name for message schema}
    hdrMessageType :: IsRPC rpc => rpc -> HTTP.Header
    hdrMessageType rpc = ("grpc-message-type", messageType rpc)

    -- > Message-Encoding → "grpc-encoding" Content-Coding
    -- > Content-Coding → "identity" / "gzip" / "deflate" / "snappy" / {custom}
    hdrMessageEncoding :: Compression -> HTTP.Header
    hdrMessageEncoding compr = (
          "grpc-encoding"
        , Compression.serializeId (compressionId compr)
        )

    -- > Message-Accept-Encoding →
    -- >   "grpc-accept-encoding" Content-Coding *("," Content-Coding)
    hdrMessageAcceptEncoding :: NonEmpty Compression -> HTTP.Header
    hdrMessageAcceptEncoding compr = (
          "grpc-accept-encoding"
        , mconcat . intersperse "," $
            map (Compression.serializeId . compressionId) (toList compr)
        )

    -- > User-Agent → "user-agent" {structured user-agent string}
    --
    -- The spec says:
    --
    --   While the protocol does not require a user-agent to function it is
    --   recommended that clients provide a structured user-agent string that
    --   provides a basic description of the calling library, version & platform
    --   to facilitate issue diagnosis in heterogeneous environments. The
    --   following structure is recommended to library developers
    --
    -- > User-Agent →
    -- >   "grpc-"
    -- >   Language
    -- >   ?("-" Variant)
    -- >   "/"
    -- >   Version
    -- >   ?( " ("  *(AdditionalProperty ";") ")" )
    hdrUserAgent :: HTTP.Header
    hdrUserAgent = (
          "user-agent"
        , mconcat [
              "grpc-"
            , "haskell"
            , "-" <> BS.Strict.C8.pack (PackageInfo.name)
            , "/"
            , mconcat . intersperse "." $
                map (BS.Strict.C8.pack . show) $
                  versionBranch PackageInfo.version
            ]
        )
