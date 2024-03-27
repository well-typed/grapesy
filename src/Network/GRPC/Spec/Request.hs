{-# LANGUAGE OverloadedStrings #-}

-- | Construct HTTP2 requests
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Request qualified as Req
module Network.GRPC.Spec.Request (
    -- * Inputs (message sent to the peer)
    RequestHeaders_(..)
  , RequestHeaders
  , IsFinal(..)
  , NoMetadata(..)
    -- * Serialization
  , buildRequestHeaders
  , parseRequestHeaders
  ) where

import Control.Monad
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.State (State, execState, modify)
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8
import Data.Functor (($>))
import Data.List (intersperse, intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Version
import GHC.Stack
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec.Common
import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.CustomMetadata.Map
import Network.GRPC.Spec.CustomMetadata.Raw
import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.Timeout
import Network.GRPC.Spec.TraceContext
import Network.GRPC.Util.HKD (HKD, Undecorated, DecoratedWith)
import Network.GRPC.Util.HKD qualified as HKD

import Paths_grapesy qualified as Grapesy

{-------------------------------------------------------------------------------
  Inputs (message sent to the peer)
-------------------------------------------------------------------------------}

-- | Full set of call parameters required to construct the RPC call
--
-- This is constructed internally; it is not part of the public API.
data RequestHeaders_ f = RequestHeaders {
      -- | Timeout
      requestTimeout :: HKD f (Maybe Timeout)

      -- | Custom metadata
    , requestMetadata :: HKD f CustomMetadataMap

      -- | Compression used for outgoing messages
    , requestCompression :: HKD f (Maybe CompressionId)

      -- | Accepted compression algorithms for incoming messages
      --
      -- @Maybe (NonEmpty ..)@ is perhaps a bit strange (why not just @[]@), but
      -- it emphasizes the specification: /if/ the header is present, it must be
      -- a non-empty list.
    , requestAcceptCompression :: HKD f (Maybe (NonEmpty CompressionId))

      -- | Optionally, override the content-type
      --
      -- Set to 'Nothing' to omit the content-type header altogether.
      --
      -- See also discussion of 'requestMessageType'.
    , requestContentType :: HKD f (Maybe ContentType)

      -- | Should we include the @Message-Type@ header?
      --
      -- To be conform to the gRPC spec, the @Message-Type@ /should/ be
      -- included, but @grapesy@ will not insist that the header is present for
      -- incoming requests. We do not need the header in order to know the
      -- message type, because the /path/ determines the service and method, and
      -- that in turn determines the message type. If it /is/ present, however,
      -- we verify that it has the valeu we expect.
    , requestMessageType :: HKD f Bool

      -- | Should we include the @te: trailers@ header?
      --
      -- To be conform to the gRPC spec, the @te@ header should be included, but
      -- @grapesy@ does not insist that the header is present for incoming
      -- requests. However, /if/ it is present, we /do/ verify that it has the
      -- right value; this prevents values getting lost without notice.
    , requestIncludeTE :: HKD f Bool

      -- | Trace context (for OpenTelemetry)
    , requestTraceContext :: HKD f (Maybe TraceContext)
    }

type RequestHeaders = RequestHeaders_ Undecorated

deriving stock instance Show RequestHeaders
deriving stock instance Eq   RequestHeaders

instance HKD.Traversable RequestHeaders_ where
  sequence x =
      RequestHeaders
        <$> requestTimeout           x
        <*> requestMetadata          x
        <*> requestCompression       x
        <*> requestAcceptCompression x
        <*> requestContentType       x
        <*> requestMessageType       x
        <*> requestIncludeTE         x
        <*> requestTraceContext      x

-- | Mark a input sent as final
data IsFinal = Final | NotFinal
  deriving stock (Show, Eq)

-- | gRPC does not support request trailers (only response trailers)
data NoMetadata = NoMetadata
  deriving stock (Show, Eq)

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
    , map buildCustomMetadata $ customMetadataMapToList requestMetadata
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
    , guard (requestIncludeTE hdrs) $> buildTe
    , buildContentType proxy <$> requestContentType hdrs
    , guard (requestMessageType hdrs) $> buildMessageType
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
  => Proxy rpc -> [HTTP.Header] -> m RequestHeaders
parseRequestHeaders proxy =
      HKD.sequence
    . flip execState uninitRequestHeaders
    . mapM_ parseHeader
  where
    parseHeader :: HTTP.Header -> State (RequestHeaders_ (DecoratedWith m)) ()
    parseHeader hdr@(name, value)
      | name == "user-agent"
      = return () -- TODO

      | name == "grpc-timeout"
      = modify $ \x -> x {
            requestTimeout = fmap Just $
              httpError HTTP.badRequest400 $
                parseTimeout value
          }

      | name == "grpc-encoding"
      = modify $ \x -> x {
            requestCompression = fmap Just $
               httpError HTTP.badRequest400 $
                 parseMessageEncoding hdr
          }

      | name == "grpc-accept-encoding"
      = modify $ \x -> x {
            requestAcceptCompression = fmap Just $
               httpError HTTP.badRequest400 $
                 parseMessageAcceptEncoding hdr
          }

      | name == "grpc-trace-bin"
      = modify $ \x -> x {
            requestTraceContext = fmap Just $
              httpError HTTP.badRequest400 $
                parseBinaryValue value >>= parseTraceContext
          }

      | name == "content-type"
      = modify $ \x -> x {
            requestContentType = fmap Just $
              httpError HTTP.unsupportedMediaType415 $
                parseContentType proxy hdr
          }

      | name == "grpc-message-type"
      = modify $ \x -> x {
            requestMessageType = do
              let expected = PercentEncoding.encode (rpcMessageType proxy)
              httpError HTTP.badRequest400 $
                expectHeaderValue hdr [expected]
              return True
          }

      | name == "te"
      = modify $ \x -> x {
            requestIncludeTE = do
              httpError HTTP.badRequest400 $
                expectHeaderValue hdr ["trailers"]
              return True
          }

      | otherwise
      = modify $ \x -> x {
            requestMetadata = do
              md <- httpError HTTP.badRequest400 $ parseCustomMetadata hdr
              customMetadataMapInsert md <$> requestMetadata x
          }

    uninitRequestHeaders :: RequestHeaders_ (DecoratedWith m)
    uninitRequestHeaders = RequestHeaders {
          requestTimeout           = return Nothing
        , requestMetadata          = return mempty
        , requestCompression       = return Nothing
        , requestAcceptCompression = return Nothing
        , requestContentType       = return Nothing
        , requestMessageType       = return False
        , requestIncludeTE         = return False
        , requestTraceContext      = return Nothing
        }

    httpError ::
         MonadError (HTTP.Status, Strict.ByteString) m'
      => HTTP.Status -> Either String a -> m' a
    httpError code (Left err) = throwError (code, BS.Strict.UTF8.fromString err)
    httpError _    (Right a)  = return a

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

expectHeaderValue ::
     MonadError String m
  => HTTP.Header -> [Strict.ByteString] -> m ()
expectHeaderValue (name, actual) expected =
    unless (actual `elem` expected) $ throwError $ concat [
        "Unexpected value \""
      , BS.Strict.C8.unpack actual
      , "\" for header"
      , show name -- Show instance adds quotes
      , ". Expected "
      , intercalate " or " $
          map (\e -> "\"" ++ BS.Strict.C8.unpack e ++ "\"") expected
      , "."
      ]

