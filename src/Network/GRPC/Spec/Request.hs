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

import Control.Monad.Except
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.SOP
import Data.Version
import Generics.SOP qualified as SOP
import GHC.Generics qualified as GHC
import GHC.Stack
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec.Common
import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.Timeout
import Network.GRPC.Spec.TraceContext
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
    , requestMetadata :: Map HeaderName HeaderValue

      -- | Compression used for outgoing messages
    , requestCompression :: Maybe CompressionId

      -- | Accepted compression algorithms for incoming messages
      --
      -- @Maybe (NonEmpty ..)@ is perhaps a bit strange (why not just @[]@), but
      -- it emphasizes the specification: /if/ the header is present, it must be
      -- a non-empty list.
    , requestAcceptCompression :: Maybe (NonEmpty CompressionId)

      -- | Optionally, override the content-type
      --
      -- Set to 'Nothing' to omit the content-type header altogether.
    , requestContentType :: Maybe ContentType

      -- | Trace context (for OpenTelemetry)
    , requestTraceContext :: Maybe TraceContext
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

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Request headers
--
-- > Request-Headers →
-- >   Call-Definition
-- >   *Custom-Metadata
buildRequestHeaders ::
     (IsRPC rpc, HasCallStack)
  => Proxy rpc -> RequestHeaders -> [HTTP.Header]
buildRequestHeaders proxy callParams@RequestHeaders{requestMetadata} = concat [
      callDefinition proxy callParams
    , map buildCustomMetadata $ Map.toList requestMetadata
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
callDefinition ::
     (IsRPC rpc, HasCallStack)
  => Proxy rpc -> RequestHeaders -> [HTTP.Header]
callDefinition proxy = \hdrs -> catMaybes [
      hdrTimeout <$> requestTimeout hdrs
    , Just $ buildTe
    , buildContentType proxy <$> requestContentType hdrs
    , Just $ buildMessageType
    , buildMessageEncoding <$> requestCompression hdrs
    , buildMessageAcceptEncoding <$> requestAcceptCompression hdrs
    , Just $ buildUserAgent
    , buildGrpcTraceBin <$> requestTraceContext hdrs
    ]
  where
    hdrTimeout :: Timeout -> HTTP.Header
    hdrTimeout t = ("grpc-timeout", buildTimeout t)

    -- > TE → "te" "trailers" # Used to detect incompatible proxies
    buildTe :: HTTP.Header
    buildTe  = ("te", "trailers")

    -- > Message-Type → "grpc-message-type" {type name for message schema}
    buildMessageType :: HTTP.Header
    buildMessageType = (
          "grpc-message-type"
        , PercentEncoding.encode $ rpcMessageType proxy
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

    buildGrpcTraceBin :: TraceContext -> HTTP.Header
    buildGrpcTraceBin ctxt = (
          "grpc-trace-bin"
        , buildBinaryValue $ buildTraceContext ctxt
        )

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

-- | Parse request headers
parseRequestHeaders :: forall rpc m.
     (IsRPC rpc, MonadError (HTTP.Status, Strict.ByteString) m)
  => Proxy rpc
  -> [HTTP.Header] -> m RequestHeaders
parseRequestHeaders proxy =
      runPartialParser uninitRequestHeaders
    . mapM_ parseHeader
  where
    parseHeader :: HTTP.Header -> PartialParser RequestHeaders m ()
    parseHeader hdr@(name, value)
      | name == "user-agent"
      = return () -- TODO

      | name == "grpc-timeout"
      = update updTimeout $ \_ ->
          fmap Just . httpError HTTP.badRequest400 $
            parseTimeout value

      | name == "grpc-encoding"
      = update updCompression $ \_ ->
          fmap Just . httpError HTTP.badRequest400 $
            parseMessageEncoding hdr

      | name == "grpc-accept-encoding"
      = update updAcceptCompression $ \_ ->
          fmap Just . httpError HTTP.badRequest400 $
            parseMessageAcceptEncoding hdr

      | name == "grpc-trace-bin"
      = update updTraceContext $ \_ ->
          fmap Just . httpError HTTP.badRequest400 $
            parseBinaryValue value >>= parseTraceContext

      -- /If/ the @te@ header is present we check its value, but we do not
      -- insist that it is present (technically this is not conform spec).
      | name == "te"
      = httpError HTTP.badRequest400 $
          expectHeaderValue hdr ["trailers"]

      -- Dealing with 'content-type' and 'grpc-message-type' is a bit subtle.
      -- In /principle/ the headers should /tell/ us what kind of RPC we are
      -- dealing with (Protobuf or otherwise) and what its message type is.
      -- However, we assume that, for a given server, the path (parsed
      -- previously) determines these values, and so we merely need to check
      -- that they match the values we expect.
      --
      -- If these headers are absent, we simply assume that they implicitly
      -- have the values we expect.
      | name == "content-type"
      = update updContentType $ \_ ->
          fmap Just . httpError HTTP.unsupportedMediaType415 $
            parseContentType proxy hdr

      | name == "grpc-message-type"
      = httpError HTTP.badRequest400 $
          expectHeaderValue hdr [PercentEncoding.encode (rpcMessageType proxy)]

      -- Everything else we parse as custom metadata
      | otherwise
      = httpError HTTP.badRequest400 (parseCustomMetadata hdr) >>=
        update updCustom . fmap . uncurry Map.insert

    -- All of these headers are optional
    uninitRequestHeaders :: Partial m RequestHeaders
    uninitRequestHeaders =
           return (Nothing   :: Maybe Timeout)
        :* return (Map.empty :: Map HeaderName HeaderValue)
        :* return (Nothing   :: Maybe CompressionId)
        :* return (Nothing   :: Maybe (NonEmpty CompressionId))
        :* return (Nothing   :: Maybe ContentType)
        :* return (Nothing   :: Maybe TraceContext)
        :* Nil

    (    updTimeout
      :* updCustom
      :* updCompression
      :* updAcceptCompression
      :* updContentType
      :* updTraceContext
      :* Nil ) = partialUpdates (Proxy @RequestHeaders)

    httpError ::
         MonadError (HTTP.Status, Strict.ByteString) m'
      => HTTP.Status -> Either String a -> m' a
    httpError code (Left err) = throwError (code, BS.Strict.UTF8.fromString err)
    httpError _    (Right a)  = return a
