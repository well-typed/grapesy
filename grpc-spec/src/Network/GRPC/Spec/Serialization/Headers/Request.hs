{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.Serialization.Headers.Request (
    buildRequestHeaders
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
import Data.Maybe (catMaybes)
import Data.Proxy
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)

import Network.GRPC.Spec
import Network.GRPC.Spec.Serialization.CustomMetadata
import Network.GRPC.Spec.Serialization.Headers.Common
import Network.GRPC.Spec.Serialization.Timeout
import Network.GRPC.Spec.Serialization.TraceContext
import Network.GRPC.Spec.Util.HKD qualified as HKD

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
    , buildContentType . Just . chooseContentType proxy <$>
        requestContentType hdrs
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
  Parsing
-------------------------------------------------------------------------------}

parseRequestHeaders :: forall rpc m.
     (IsRPC rpc, MonadError (InvalidHeaders GrpcException) m)
  => Proxy rpc
  -> [HTTP.Header] -> m RequestHeaders
parseRequestHeaders proxy = HKD.sequenceChecked . parseRequestHeaders' proxy

-- | Parse request headers
--
-- This can report invalid headers on a per-header basis; see also
-- 'parseRequestHeaders'.
parseRequestHeaders' :: forall rpc.
     IsRPC rpc
  => Proxy rpc
  -> [HTTP.Header] -> RequestHeaders' GrpcException
parseRequestHeaders' proxy =
      flip execState uninitRequestHeaders
    . mapM_ (parseHeader . second trim)
  where
    parseHeader :: HTTP.Header -> State (RequestHeaders' GrpcException) ()
    parseHeader hdr@(name, value)
      | name == "user-agent"
      = modify $ \x -> x {
           requestUserAgent = return (Just value)
          }

      | name == "grpc-timeout"
      = modify $ \x -> x {
            requestTimeout = fmap Just $
              httpError hdr $
                parseTimeout value
          }

      | name == "grpc-encoding"
      = modify $ \x -> x {
            requestCompression = fmap Just $
               parseMessageEncoding hdr
          }

      | name == "grpc-accept-encoding"
      = modify $ \x -> x {
            requestAcceptCompression = fmap Just $
               parseMessageAcceptEncoding hdr
          }

      | name == "grpc-trace-bin"
      = modify $ \x -> x {
            requestTraceContext = fmap Just $
              httpError hdr $
                parseBinaryValue value >>= parseTraceContext
          }

      | name == "content-type"
      = modify $ \x -> x {
            requestContentType = fmap Just $
              parseContentType' proxy hdr
          }

      | name == "grpc-message-type"
      = modify $ \x -> x {
            requestMessageType = return . Just $
              parseMessageType proxy hdr
          }

      | name == "te"
      = modify $ \x -> x {
            requestIncludeTE = do
              expectHeaderValue hdr ["trailers"]
              return True
          }

      | name == "grpc-previous-rpc-attempts"
      = modify $ \x -> x {
            requestPreviousRpcAttempts = do
              httpError hdr $
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
                    Left invalid' -> invalid <> invalid'
                    Right ()      -> invalid
              }
            Right md -> x {
                requestMetadata = customMetadataMapInsert md $ requestMetadata x
              }

    uninitRequestHeaders :: RequestHeaders' GrpcException
    uninitRequestHeaders = RequestHeaders {
          requestTimeout             = return Nothing
        , requestCompression         = return Nothing
        , requestAcceptCompression   = return Nothing
        , requestIncludeTE           = return False
        , requestUserAgent           = return Nothing
        , requestTraceContext        = return Nothing
        , requestPreviousRpcAttempts = return Nothing
        , requestMetadata            = mempty
        , requestUnrecognized        = return ()

        -- Special cases

        , requestContentType =
            throwError $ missingHeader invalidContentType "content-type"
        , requestMessageType =
            -- If the default is that this header should be absent, then /start/
            -- with 'MessageTypeDefault'; if it happens to present, parse it as
            -- an override.
            case rpcMessageType proxy of
              Nothing -> return $ Just MessageTypeDefault
              Just _  -> return $ Nothing
        }

    httpError ::
         MonadError (InvalidHeaders GrpcException) m'
      => HTTP.Header -> Either String a -> m' a
    httpError _   (Right a)  = return a
    httpError hdr (Left err) = throwError $ invalidHeader Nothing hdr err

{-------------------------------------------------------------------------------
  Content type

  See 'parseContentType' for discussion.
-------------------------------------------------------------------------------}

parseContentType' ::
     IsRPC rpc
  => Proxy rpc
  -> HTTP.Header
  -> Either (InvalidHeaders GrpcException) ContentType
parseContentType' proxy hdr =
    parseContentType
      proxy
      (invalidHeader invalidContentType hdr)
      hdr

invalidContentType :: Maybe HTTP.Status
invalidContentType = Just HTTP.unsupportedMediaType415

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

expectHeaderValue ::
     MonadError (InvalidHeaders GrpcException) m
  => HTTP.Header -> [Strict.ByteString] -> m ()
expectHeaderValue hdr@(_name, actual) expected =
    unless (actual `elem` expected) $
      throwError $ invalidHeader Nothing hdr err
  where
    err :: String
    err = concat [
          "Expected "
        , intercalate " or " $
            map (\e -> "\"" ++ BS.Strict.C8.unpack e ++ "\"") expected
        , "."
        ]
