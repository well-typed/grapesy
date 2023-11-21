{-# LANGUAGE OverloadedStrings #-}

-- | Construct HTTP2 requests
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Request qualified as Req
module Network.GRPC.Spec.Request (
    -- * Inputs (message sent to the peer)
    RequestHeaders(..)
  , IsFinal(..)
  , NoMetadata(..)
    -- * Serialization
  , buildRequestHeaders
  , parseRequestHeaders
  ) where

import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.SOP
import Data.Version
import Generics.SOP qualified as SOP
import GHC.Generics qualified as GHC
import Network.HTTP.Types qualified as HTTP
import Text.Show.Pretty

import Network.GRPC.Spec.Common
import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.Timeout
import Network.GRPC.Util.Partial

import Paths_grapesy qualified as Grapesy

{-------------------------------------------------------------------------------
  Inputs (message sent to the peer)
-------------------------------------------------------------------------------}

-- | Full set of call parameters required to construct the RPC call
--
-- This is constructed internally; it is not part of the public API.
data RequestHeaders = RequestHeaders {
      -- | Timeout
      requestTimeout :: Maybe Timeout

      -- | Custom metadata
    , requestMetadata :: [CustomMetadata]

      -- | Compression used for outgoing messages
    , requestCompression :: Maybe CompressionId

      -- | Accepted compression algorithms for incoming messages
      --
      -- @Maybe (NonEmpty ..)@ is perhaps a bit strange (why not just @[]@), but
      -- it emphasizes the specification: /if/ the header is present, it must be
      -- a non-empty list.
    , requestAcceptCompression :: Maybe (NonEmpty CompressionId)
    }
  deriving stock (Show, Eq)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Mark a input sent as final
data IsFinal = Final | NotFinal
  deriving stock (Show, Eq)

-- | gRPC does not support request trailers (only response trailers)
data NoMetadata = NoMetadata
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Request headers
--
-- > Request-Headers →
-- >   Call-Definition
-- >   *Custom-Metadata
buildRequestHeaders :: IsRPC rpc => Proxy rpc -> RequestHeaders -> [HTTP.Header]
buildRequestHeaders proxy callParams@RequestHeaders{requestMetadata} = concat [
      callDefinition proxy callParams
    , map buildCustomMetadata requestMetadata
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
callDefinition :: IsRPC rpc => Proxy rpc -> RequestHeaders -> [HTTP.Header]
callDefinition proxy = \hdrs -> catMaybes [
      hdrTimeout <$> requestTimeout hdrs
    , Just $ buildTe
    , Just $ buildContentType proxy
    , Just $ buildMessageType
    , buildMessageEncoding       <$> requestCompression       hdrs
    , buildMessageAcceptEncoding <$> requestAcceptCompression hdrs
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
    buildMessageType :: HTTP.Header
    buildMessageType = (
          "grpc-message-type"
        , PercentEncoding.encode $ messageType proxy
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
              "grpc-haskell-grapesy/"
            , mconcat . intersperse "." $
                map (BS.Strict.C8.pack . show) $
                  versionBranch Grapesy.version
            ]
        )

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

-- | Parse request headers
parseRequestHeaders ::
     IsRPC rpc
  => Proxy rpc
  -> [HTTP.Header] -> Either String RequestHeaders
parseRequestHeaders proxy =
      runPartialParser uninitRequestHeaders
    . mapM_ parseHeader
  where
    parseHeader ::
         HTTP.Header
      -> PartialParser RequestHeaders (Either String) ()
    parseHeader hdr@(name, _value)
      | name == "user-agent"
      = return () -- TODO

      | name == "grpc-timeout"
      = return () -- TODO

      | name == "grpc-encoding"
      = update updCompression $ \_ ->
          Just <$> parseMessageEncoding hdr

      | name == "grpc-accept-encoding"
      = update updAcceptCompression $ \_ ->
          Just <$> parseMessageAcceptEncoding hdr

      -- /If/ the @te@ header is present we check its value, but we do not
      -- insist that it is present (technically this is not conform spec).
      | name == "te"
      = expectHeaderValue hdr ["trailers"]

      -- Dealing with 'content-type' and 'grpc-message-type' is a bit subtle.
      -- In /principle/ the headers should /tell/ us what kind of RPC we are
      -- dealing with (Protobuf or otherwise) and what its message type is.
      -- However, we assume that, for a given server, the path (parsed
      -- previously) determines these values, and so we merely need to check
      -- that they match the values we expect.
      --
      -- TODO: For content types that aren't application/grpc (with or without
      -- a subtype), we should throw HTTP 415 Unsupported Media Type response
      --
      -- TODO: We should throw an error if these headers are not present.
      | name == "content-type"
      = parseContentType proxy hdr
      | name == "grpc-message-type"
      = expectHeaderValue hdr [PercentEncoding.encode (messageType proxy)]

      -- Everything else we parse as custom metadata
      | otherwise
      = parseCustomMetadata hdr >>= update updCustom . fmap . (:)

    -- All of these headers are optional
    uninitRequestHeaders :: Partial (Either String) RequestHeaders
    uninitRequestHeaders =
           Right (Nothing :: Maybe Timeout)
        :* Right ([]      :: [CustomMetadata])
        :* Right (Nothing :: Maybe CompressionId)
        :* Right (Nothing :: Maybe (NonEmpty CompressionId))
        :* Nil

    (    _updTimeout
      :* updCustom
      :* updCompression
      :* updAcceptCompression
      :* Nil ) = partialUpdates (Proxy @RequestHeaders)

