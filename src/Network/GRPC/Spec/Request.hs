{-# LANGUAGE OverloadedStrings #-}

-- | Construct HTTP2 requests
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Request qualified as Request
module Network.GRPC.Spec.Request (
    buildHeaders
  ) where

import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Version
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec
import Network.GRPC.Spec.Common
import Network.GRPC.Spec.Compression (compressionId)
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
import Network.GRPC.Spec.RPC

import PackageInfo_grapesy qualified as PackageInfo

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Request headers
--
-- > Request-Headers →
-- >   Call-Definition
-- >   *Custom-Metadata
buildHeaders :: IsRPC rpc => RequestHeaders -> rpc -> [HTTP.Header]
buildHeaders callParams@RequestHeaders{requestParams} rpc = concat [
      callDefinition callParams rpc
    , map buildCustomMetadata $ callMetadata requestParams
    ]

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
callDefinition :: IsRPC rpc => RequestHeaders -> rpc -> [HTTP.Header]
callDefinition = \callParams@RequestHeaders{requestParams} rpc -> catMaybes [
      hdrTimeout <$> callTimeout requestParams
    , Just $ buildTe
    , Just $ buildContentType rpc
    , Just $ buildMessageType rpc
    , Just $ buildMessageEncoding (compressionId $ requestCompression callParams)
    , Just $ buildMessageAcceptEncoding (requestAcceptCompression callParams)
    , Just $ buildUserAgent
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
    buildTe :: HTTP.Header
    buildTe  = ("te", "trailers")

    -- > Message-Type → "grpc-message-type" {type name for message schema}
    buildMessageType :: IsRPC rpc => rpc -> HTTP.Header
    buildMessageType rpc = (
          "grpc-message-type"
        , PercentEncoding.encode $ messageType rpc
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
    buildUserAgent :: HTTP.Header
    buildUserAgent = (
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
