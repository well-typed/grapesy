{-# LANGUAGE OverloadedStrings #-}

-- | Construct HTTP2 requests
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Request qualified as Req
module Network.GRPC.Spec.Headers.Request (
    -- * Inputs (message sent to the peer)
    RequestHeaders_(..)
  , RequestHeaders
  , RequestHeaders'
  , InvalidRequestHeaders
    -- * Serialization
  , buildRequestHeaders
  , parseRequestHeaders
  , parseRequestHeaders'
  ) where

import Control.Monad
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.State (State, execState, modify)
import Data.Bifunctor
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.Functor (($>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Proxy
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)

import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.CustomMetadata.Map
import Network.GRPC.Spec.CustomMetadata.Raw
import Network.GRPC.Spec.Headers.Common
import Network.GRPC.Spec.Headers.Invalid
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.Timeout
import Network.GRPC.Spec.TraceContext
import Network.GRPC.Util.HKD (HKD, Undecorated, DecoratedWith)
import Network.GRPC.Util.HKD qualified as HKD

{-------------------------------------------------------------------------------
  Inputs (message sent to the peer)
-------------------------------------------------------------------------------}

-- | Full set of call parameters required to construct the RPC call
--
-- This is constructed internally; it is not part of the public API.
data RequestHeaders_ f = RequestHeaders {
      -- | Timeout
      requestTimeout :: HKD f (Maybe Timeout)

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
      -- Set to 'Nothing' to omit the message-type header altogether.
      --
      -- We do not need the header in order to know the message type, because
      -- the /path/ determines the service and method, and that in turn
      -- determines the message type. If it /is/ present, however, we verify
      -- that it has the valeu we expect.
    , requestMessageType :: HKD f (Maybe MessageType)

      -- | User agent
    , requestUserAgent :: HKD f (Maybe Strict.ByteString)

      -- | Should we include the @te: trailers@ header?
      --
      -- The @TE@ header is part of the HTTP specification;
      -- see also <https://datatracker.ietf.org/doc/html/rfc7230#section-4.3>.
      -- It indicates that we are willing to accept a chunked encoding for the
      -- response body, and that we expect trailers to be present after the
      -- response body.
      --
      -- To be conform to the gRPC spec, the @te@ header should be included, but
      -- @grapesy@ does not insist that the header is present for incoming
      -- requests. However, /if/ it is present, we /do/ verify that it has the
      -- right value; this prevents values getting lost without notice.
    , requestIncludeTE :: HKD f Bool

      -- | Trace context (for OpenTelemetry)
    , requestTraceContext :: HKD f (Maybe TraceContext)

      -- | Previous RPC attempts
      --
      -- This is part of automatic retries.
      -- See <https://github.com/grpc/proposal/blob/master/A6-client-retries.md>.
    , requestPreviousRpcAttempts :: HKD f (Maybe Int)

      -- | Custom metadata
      --
      -- Any header we do not otherwise explicitly support we attempt to parse
      -- as custom metadata. Headers for which this fails end up in
      -- 'requestUnrecognized'; reasons for this include the use of reserved
      -- header names (starting with @grpc-@), invalid binary encodings, etc.
    , requestMetadata :: CustomMetadataMap

      -- | Unrecognized headers
    , requestUnrecognized :: HKD f ()
    }
  deriving anyclass (HKD.Coerce)

-- | Request headers (without allowing for invalid headers)
--
-- NOTE: The HKD type
--
-- > RequestHeaders_ Undecorated
--
-- means that each field of type @HKD f a@ is simply of type @a@ (that is,
-- undecorated).
type RequestHeaders = RequestHeaders_ Undecorated

-- | Request headers allowing for invalid headers
--
-- NOTE: The HKD type
--
-- > RequestHeaders_ (DecoratedWith (Either InvalidRequestHeaders))
--
-- means that each field of type @HKD f a@ is of type
--
-- > Either InvalidRequestHeaders a
--
-- (i.e., either valid or invalid).
type RequestHeaders' =
       RequestHeaders_ (DecoratedWith (Either InvalidRequestHeaders))

deriving stock instance Show    RequestHeaders
deriving stock instance Eq      RequestHeaders
deriving stock instance Generic RequestHeaders

deriving stock instance Show RequestHeaders'
deriving stock instance Eq   RequestHeaders'
-- We do not derive Generic for RequestHeaders', as doing so makes ghc confused
-- about the instance for RequestHeaders for some reason.

instance HKD.Traversable RequestHeaders_ where
  traverse f x =
      RequestHeaders
        <$> (f    $ requestTimeout             x)
        <*> (f    $ requestCompression         x)
        <*> (f    $ requestAcceptCompression   x)
        <*> (f    $ requestContentType         x)
        <*> (f    $ requestMessageType         x)
        <*> (f    $ requestUserAgent           x)
        <*> (f    $ requestIncludeTE           x)
        <*> (f    $ requestTraceContext        x)
        <*> (f    $ requestPreviousRpcAttempts x)
        <*> (pure $ requestMetadata            x)
        <*> (f    $ requestUnrecognized        x)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Request headers
--
-- > Request-Headers →
-- >   Call-Definition
-- >   *Custom-Metadata
buildRequestHeaders ::
     IsRPC rpc
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
callDefinition :: forall rpc.
     IsRPC rpc
  => Proxy rpc -> RequestHeaders -> [HTTP.Header]
callDefinition proxy = \hdrs -> catMaybes [
      hdrTimeout <$> requestTimeout hdrs
    , guard (requestIncludeTE hdrs) $> buildTe
    , buildContentType proxy <$> requestContentType hdrs
    , join $ buildMessageType proxy <$> requestMessageType hdrs
    , buildMessageEncoding <$> requestCompression hdrs
    , buildMessageAcceptEncoding <$> requestAcceptCompression hdrs
    , buildUserAgent <$> requestUserAgent hdrs
    , buildGrpcTraceBin <$> requestTraceContext hdrs
    , buildPreviousRpcAttempts <$> requestPreviousRpcAttempts hdrs
    ]
  where
    hdrTimeout :: Timeout -> HTTP.Header
    hdrTimeout t = ("grpc-timeout", buildTimeout t)

    -- > TE → "te" "trailers" # Used to detect incompatible proxies
    buildTe :: HTTP.Header
    buildTe  = ("te", "trailers")

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
    buildUserAgent :: Strict.ByteString -> HTTP.Header
    buildUserAgent userAgent = (
          "user-agent"
        , userAgent
        )

    buildGrpcTraceBin :: TraceContext -> HTTP.Header
    buildGrpcTraceBin ctxt = (
          "grpc-trace-bin"
        , buildBinaryValue $ buildTraceContext ctxt
        )

    buildPreviousRpcAttempts :: Int -> HTTP.Header
    buildPreviousRpcAttempts n = (
          "grpc-previous-rpc-attempts"
        , BS.Strict.C8.pack $ show n
        )

{-------------------------------------------------------------------------------
  Invalid headers
-------------------------------------------------------------------------------}

-- | Invalid request headers
--
-- For certain types of failures the gRPC spec mandates a specific HTTP status.
type InvalidRequestHeaders = (HTTP.Status, InvalidHeaders)

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseRequestHeaders :: forall rpc m.
     (IsRPC rpc, MonadError InvalidRequestHeaders m)
  => Proxy rpc
  -> [HTTP.Header] -> m RequestHeaders
parseRequestHeaders proxy = HKD.sequenceThrow . parseRequestHeaders' proxy

-- | Parse request headers
--
-- This can report invalid headers on a per-header basis; see also
-- 'parseRequestHeaders'.
parseRequestHeaders' :: forall rpc.
     IsRPC rpc
  => Proxy rpc
  -> [HTTP.Header] -> RequestHeaders'
parseRequestHeaders' proxy =
      flip execState uninitRequestHeaders
    . mapM_ (parseHeader . second trim)
  where
    parseHeader :: HTTP.Header -> State RequestHeaders' ()
    parseHeader hdr@(name, value)
      | name == "user-agent"
      = modify $ \x -> x {
           requestUserAgent = return (Just value)
          }

      | name == "grpc-timeout"
      = modify $ \x -> x {
            requestTimeout = fmap Just $
              httpError hdr HTTP.badRequest400 $
                parseTimeout value
          }

      | name == "grpc-encoding"
      = modify $ \x -> x {
            requestCompression = fmap Just $
               first (HTTP.badRequest400,) $
                 parseMessageEncoding hdr
          }

      | name == "grpc-accept-encoding"
      = modify $ \x -> x {
            requestAcceptCompression = fmap Just $
               first (HTTP.badRequest400,) $
                 parseMessageAcceptEncoding hdr
          }

      | name == "grpc-trace-bin"
      = modify $ \x -> x {
            requestTraceContext = fmap Just $
              httpError hdr HTTP.badRequest400 $
                parseBinaryValue value >>= parseTraceContext
          }

      | name == "content-type"
      = modify $ \x -> x {
            requestContentType = fmap Just $
              first (HTTP.unsupportedMediaType415,) $
                parseContentType proxy hdr
          }

      | name == "grpc-message-type"
      = modify $ \x -> x {
            requestMessageType = return . Just $
              parseMessageType proxy hdr
          }

      | name == "te"
      = modify $ \x -> x {
            requestIncludeTE = do
              first (HTTP.badRequest400,) $
                expectHeaderValue hdr ["trailers"]
              return True
          }

      | name == "grpc-previous-rpc-attempts"
      = modify $ \x -> x {
            requestPreviousRpcAttempts = do
              httpError hdr HTTP.badRequest400 $
                maybe
                  (Left $ "grpc-previous-rpc-attempts: invalid " ++ show value)
                  (Right . Just)
                  (readMaybe $ BS.Strict.C8.unpack value)
          }

      | otherwise
      = modify $ \x ->
          case parseCustomMetadata hdr of
            Left invalid -> x {
                requestUnrecognized = Left $
                  case requestUnrecognized x of
                    Left (status, invalid') -> (status, invalid <> invalid')
                    Right ()                -> (HTTP.badRequest400, invalid)
              }
            Right md -> x {
                requestMetadata = customMetadataMapInsert md $ requestMetadata x
              }

    uninitRequestHeaders :: RequestHeaders'
    uninitRequestHeaders = RequestHeaders {
          requestTimeout             = return Nothing
        , requestCompression         = return Nothing
        , requestAcceptCompression   = return Nothing
        , requestContentType         = return Nothing
        , requestIncludeTE           = return False
        , requestUserAgent           = return Nothing
        , requestTraceContext        = return Nothing
        , requestPreviousRpcAttempts = return Nothing
        , requestMessageType         =
            -- If the default is that this header should be absent, then /start/
            -- with 'MessageTypeDefault'; if it happens to present, parse it as
            -- an override.
            case rpcMessageType proxy of
              Nothing -> return $ Just MessageTypeDefault
              Just _  -> return $ Nothing
        , requestMetadata            = mempty
        , requestUnrecognized        = return ()
        }

    httpError ::
         MonadError InvalidRequestHeaders m'
      => HTTP.Header -> HTTP.Status -> Either String a -> m' a
    httpError _   _      (Right a)  = return a
    httpError hdr status (Left err) = throwError (status, invalidHeader hdr err)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

expectHeaderValue ::
     MonadError InvalidHeaders m
  => HTTP.Header -> [Strict.ByteString] -> m ()
expectHeaderValue hdr@(_name, actual) expected =
    unless (actual `elem` expected) $
      throwError $ invalidHeader hdr err
  where
    err :: String
    err = concat [
          "Expected "
        , intercalate " or " $
            map (\e -> "\"" ++ BS.Strict.C8.unpack e ++ "\"") expected
        , "."
        ]
