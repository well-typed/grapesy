{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Deal with HTTP2 responses
--
-- Intended for unqualified import.
module Network.GRPC.Spec.Serialization.Headers.Response (
    -- * ResponseHeaders
    buildResponseHeaders
  , parseResponseHeaders
  , parseResponseHeaders'
    -- * ProperTrailers
  , buildProperTrailers
  , parseProperTrailers
  , parseProperTrailers'
    -- * TrailersOnly
  , buildTrailersOnly
  , parseTrailersOnly
  , parseTrailersOnly'
    -- * Classify server response
  , classifyServerResponse
    -- * Pushback
  , buildPushback
  , parsePushback
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.CaseInsensitive qualified as CI
import Data.Maybe (isJust)
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)

#if !MIN_VERSION_text(2,0,0)
import Data.Text.Encoding.Error qualified as Text
#endif

import Network.GRPC.Spec
import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
import Network.GRPC.Spec.Serialization.CustomMetadata
import Network.GRPC.Spec.Serialization.Headers.Common
import Network.GRPC.Spec.Util.HKD qualified as HKD
import Network.GRPC.Spec.Util.Protobuf qualified as Protobuf

{-------------------------------------------------------------------------------
  Classify server response
-------------------------------------------------------------------------------}

-- | Classify server response
--
-- gRPC servers are supposed to respond with HTTP status @200 OK@ no matter
-- whether the call was successful or not; if not successful, the information
-- about the failure should be reported using @grpc-status@ and related headers
-- (@grpc-message@, @grpc-status-details-bin@).
--
-- The gRPC spec mandates that if we get a non-200 status from a broken
-- deployment, we synthesize a gRPC exception with an appropriate status and
-- status message. The spec itself does not provide any guidance on what such an
-- appropriate status would look like, but the official gRPC repo does provide a
-- partial mapping between HTTP status codes and gRPC status codes at
-- <https://github.com/grpc/grpc/blob/master/doc/http-grpc-status-mapping.md>.
-- This is the mapping we implement here.
classifyServerResponse :: forall rpc.
     IsRPC rpc
  => Proxy rpc
  -> HTTP.Status           -- ^ HTTP status
  -> [HTTP.Header]         -- ^ Headers
  -> Maybe Lazy.ByteString -- ^ Response body, if known (used for errors only)
  -> Either (TrailersOnly' GrpcException) (ResponseHeaders' GrpcException)
classifyServerResponse rpc status headers mBody
  -- The "HTTP to gRPC Status Code Mapping" is explicit:
  --
  -- > (..) to be used only for clients that received a response that did not
  -- > include grpc-status. If grpc-status was provided, it must be used.
  --
  -- Therefore if @grpc-status@ is present, we ignore the HTTP status.
  | hasGrpcStatus headers
  = Left $ parseTrailersOnly' rpc headers

  | 200 <- statusCode
  = Right $ parseResponseHeaders' rpc headers

  | otherwise
  = Left $
      case statusCode of
        400 -> synthesize GrpcInternal         -- Bad request
        401 -> synthesize GrpcUnauthenticated  -- Unauthorized
        403 -> synthesize GrpcPermissionDenied -- Forbidden
        404 -> synthesize GrpcUnimplemented    -- Not found
        429 -> synthesize GrpcUnavailable      -- Too many requests
        502 -> synthesize GrpcUnavailable      -- Bad gateway
        503 -> synthesize GrpcUnavailable      -- Service unavailable
        504 -> synthesize GrpcUnavailable      -- Gateway timeout
        _   -> synthesize GrpcUnknown
  where
    HTTP.Status{statusCode, statusMessage} = status

    -- The @grpc-status@ header not present, and HTTP status not @200 OK@.
    -- We classify the response as an error response (hence 'TrailersOnly''):
    --
    -- * We set 'properTrailersGrpcStatus' based on the HTTP status.
    -- * We leave 'properTrailersGrpcMessage' alone if @grpc-message@ present
    --   and valid, and replace it with a default message otherwise.
    --
    -- The resulting 'TrailersOnly'' cannot contain any parse errors
    -- (only @grpc-status@ is required, and only @grpc-message@ can fail).
    synthesize :: GrpcError -> TrailersOnly' GrpcException
    synthesize err = parsed {
          trailersOnlyContentType =
            -- We tried to parse the headers that were there, but there will
            -- almost certainly not be a content-type header present in the
            -- case of a non-200 HTTP status. We don't want to synthesize /that/
            -- error, so we override it.
            case trailersOnlyContentType parsed of
              Left  _err   -> Right Nothing
              Right mCType -> Right mCType
        , trailersOnlyProper = parsedTrailers {
              properTrailersGrpcStatus = Right $
                GrpcError err
            , properTrailersGrpcMessage = Right $
                case properTrailersGrpcMessage parsedTrailers of
                  Right (Just msg) -> Just msg
                  _otherwise       -> Just defaultMsg
            }
        }
      where
        parsed :: TrailersOnly' GrpcException
        parsed = parseTrailersOnly' rpc headers

        parsedTrailers :: ProperTrailers'
        parsedTrailers = trailersOnlyProper parsed

        defaultMsg :: Text
        defaultMsg = mconcat [
              "Unexpected HTTP status code "
            , Text.pack (show statusCode)
            , if not (BS.Strict.null statusMessage)
                then " (" <> decodeUtf8Lenient statusMessage <> ")"
                else mempty
            , case mBody of
                Just body | not (BS.Lazy.null body) -> mconcat [
                    "\nResponse body:\n"
                  , decodeUtf8Lenient (BS.Lazy.toStrict body)
                  ]
                _otherwise ->
                  mempty
            ]

-- | Is the @grpc-status@ header set?
--
-- We use this as a proxy to determine if we are in the Trailers-Only case.
--
-- It might be tempting to use the HTTP @Content-Length@ header instead, but
-- this is doubly wrong:
--
-- * There might be servers who use the Trailers-Only case but do not set the
--   @Content-Length@ header (although such a server would not conform to the
--   HTTP spec: "An origin server SHOULD send a @Content-Length@ header field
--   when the content size is known prior to sending the complete header
--   section"; see
--   <https://www.rfc-editor.org/rfc/rfc9110.html#name-content-length>).
-- * Conversely, there might be servers or proxies who /do/ set @Content-Length@
--   header even when it's /not/ the Trailers-Only case (e.g., see
--   <https://github.com/grpc/grpc-web/issues/1101> or
--   <https://github.com/envoyproxy/envoy/issues/5554>).
--
-- We therefore check for the presence of the @grpc-status@ header instead.
hasGrpcStatus :: [HTTP.Header] -> Bool
hasGrpcStatus = isJust . lookup "grpc-status"

{-------------------------------------------------------------------------------
  > Response-Headers →
  >   HTTP-Status
  >   [Message-Encoding]
  >   [Message-Accept-Encoding]
  >   Content-Type
  >   *Custom-Metadata

  We do not deal with @HTTP-Status@ here; @http2@ deals this separately.
-------------------------------------------------------------------------------}

-- | Build response headers
buildResponseHeaders :: forall rpc.
     SupportsServerRpc rpc
  => Proxy rpc -> ResponseHeaders -> [HTTP.Header]
buildResponseHeaders proxy
             ResponseHeaders{ responseCompression
                            , responseAcceptCompression
                            , responseMetadata
                            , responseContentType
                            } = concat [
      [ buildContentType $ Just (chooseContentType proxy x)
      | Just x <- [responseContentType]
      ]
    , [ buildMessageEncoding x
      | Just x <- [responseCompression]
      ]
    , [ buildMessageAcceptEncoding x
      | Just x <- [responseAcceptCompression]
      ]
    , [ buildTrailer proxy ]
    , [ buildCustomMetadata x
      | x <- customMetadataMapToList responseMetadata
      ]
    ]

-- | Parse response headers
parseResponseHeaders :: forall rpc m.
     (IsRPC rpc, MonadError (InvalidHeaders GrpcException) m)
  => Proxy rpc -> [HTTP.Header] -> m ResponseHeaders
parseResponseHeaders proxy = HKD.sequenceChecked . parseResponseHeaders' proxy

-- | Generalization of 'parseResponseHeaders' that does not throw errors
--
-- See also 'Network.GRPC.Spec.parseRequestHeaders' versus
-- ''Network.GRPC.Spec.parseRequestHeaders'' for a similar pair of functions.
parseResponseHeaders' :: forall rpc.
     IsRPC rpc
  => Proxy rpc -> [HTTP.Header] -> ResponseHeaders' GrpcException
parseResponseHeaders' proxy =
      flip execState uninitResponseHeaders
    . mapM_ (parseHeader . second trim)
  where
    -- HTTP2 header names are always lowercase, and must be ASCII.
    -- <https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.2>
    parseHeader :: HTTP.Header -> State (ResponseHeaders' GrpcException) ()
    parseHeader hdr@(name, _value)
      | name == "content-type"
      = modify $ \x -> x {
            responseContentType = Just <$> parseContentType' proxy hdr
          }

      | name == "grpc-encoding"
      = modify $ \x -> x {
            responseCompression = Just <$> parseMessageEncoding hdr
          }

      | name == "grpc-accept-encoding"
      = modify $ \x -> x {
            responseAcceptCompression = Just <$> parseMessageAcceptEncoding hdr
          }

      | name == "trailer"
      = return () -- ignore the HTTP trailer header

      | otherwise
      = modify $ \x ->
          case parseCustomMetadata hdr of
            Left invalid -> x{
                responseUnrecognized = Left $ mconcat [
                    invalid
                  , otherInvalid $ responseUnrecognized x
                  ]
              }
            Right md -> x{
                responseMetadata =
                  customMetadataMapInsert md $ responseMetadata x
              }

    uninitResponseHeaders :: ResponseHeaders' GrpcException
    uninitResponseHeaders = ResponseHeaders {
          responseCompression       = return Nothing
        , responseAcceptCompression = return Nothing
        , responseMetadata          = mempty
        , responseUnrecognized      = return ()

          -- special cases

        , responseContentType =
            throwError $
              invalidContentType
                "Missing content-type header"
                (MissingHeader Nothing "content-type")
        }

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
      (\err -> invalidContentType err (InvalidHeader Nothing hdr err))
      hdr

invalidContentType ::
     String
  -> InvalidHeader HandledSynthesized
  -> InvalidHeaders GrpcException
invalidContentType err = invalidHeaderSynthesize GrpcException {
      grpcError         = GrpcUnknown
    , grpcErrorMessage  = Just $ Text.pack err
    , grpcErrorMetadata = []
    }

{-------------------------------------------------------------------------------
  > Trailers → Status [Status-Message] *Custom-Metadata
-------------------------------------------------------------------------------}

-- | Construct the HTTP @Trailer@ header
--
-- This lists all headers that /might/ be present in the trailers.
--
-- See
--
-- * <https://datatracker.ietf.org/doc/html/rfc7230#section-4.4>
-- * <https://www.rfc-editor.org/rfc/rfc9110#name-processing-trailer-fields>
buildTrailer :: forall rpc. SupportsServerRpc rpc => Proxy rpc -> HTTP.Header
buildTrailer _ = (
      "Trailer"
    , BS.Strict.intercalate ", " allPotentialTrailers
    )
  where
    allPotentialTrailers :: [Strict.ByteString]
    allPotentialTrailers = concat [
          reservedTrailers
        , map (CI.original . buildHeaderName) $
            metadataHeaderNames (Proxy @(ResponseTrailingMetadata rpc))
        ]

    -- These cannot be 'HeaderName' (which disallow reserved names)
    --
    -- This list must match the names used by 'buildProperTrailers'
    -- and recognized by 'parseProperTrailers'.
    reservedTrailers :: [Strict.ByteString]
    reservedTrailers = [
          "grpc-status"
        , "grpc-message"
        , "grpc-retry-pushback-ms"
        , "endpoint-load-metrics-bin"
        ]

-- | Build trailers (see 'buildTrailersOnly' for the Trailers-Only case)
buildProperTrailers :: ProperTrailers -> [HTTP.Header]
buildProperTrailers ProperTrailers{
                        properTrailersGrpcStatus
                      , properTrailersGrpcMessage
                      , properTrailersMetadata
                      , properTrailersPushback
                      , properTrailersOrcaLoadReport
                      } = concat [
    -- NOTE: If we add additional (reserved) headers here, we also need to add
    -- them to 'buildTrailer'.
      [ ( "grpc-status"
        , BS.Strict.C8.pack $ show $ fromGrpcStatus properTrailersGrpcStatus
        )
      ]
    , [ ("grpc-message", PercentEncoding.encode x)
      | Just x <- [properTrailersGrpcMessage]
      ]
    , [ ( "grpc-retry-pushback-ms"
        , buildPushback x
        )
      | Just x <- [properTrailersPushback]
      ]
    , [ ( "endpoint-load-metrics-bin"
        , buildBinaryValue $ Protobuf.buildStrict x
        )
      | Just x <- [properTrailersOrcaLoadReport]
      ]
    , [ buildCustomMetadata x
      | x <- customMetadataMapToList properTrailersMetadata
      ]
    ]

-- | Build trailers for the Trailers-Only case
buildTrailersOnly ::
     (ContentType -> Maybe BS.Strict.C8.ByteString)
     -- ^ Interpret 'ContentType'
     --
     -- Under normal circumstances this should be @Just .@ 'chooseContentType'.
     -- In some cases, however, the content-type might not be known. For
     -- example, when a request comes in for an unknown method, the gRPC server
     -- is supposed to respond with a @Trailers-Only@ message, with an
     -- @UNIMPLEMENTED@ error code. Frustratingly, @Trailers-Only@ requires a
     -- @Content-Type@ header, /even though there is no content/. This
     -- @Content-Type@ header normally indicates the serialization format (e.g.,
     -- @application/grpc+proto@), but this format depends on the specific
     -- method, which was not found!
     --
     -- To resolve this catch-22, this function is allowed to return @Nothing@,
     -- in which case the @Content-Type@ we will use @application/grpc@, with no
     -- format specifier. Fortunately, this is allowed by the spec.
  -> TrailersOnly -> [HTTP.Header]
buildTrailersOnly f TrailersOnly{
                            trailersOnlyContentType
                          , trailersOnlyProper
                          } = concat [
      [ buildContentType $ f x
      | Just x <- [trailersOnlyContentType]
      ]
    , buildProperTrailers trailersOnlyProper
    ]

-- | Parse response trailers
--
-- The gRPC spec defines:
--
-- > Trailers      → Status [Status-Message] *Custom-Metadata
-- > Trailers-Only → HTTP-Status Content-Type Trailers
--
-- This means that Trailers-Only is a superset of the Trailers; we make use of
-- this here, and error out if we get an unexpected @Content-Type@ override.
parseProperTrailers :: forall rpc m.
     (IsRPC rpc, MonadError (InvalidHeaders GrpcException) m)
  => Proxy rpc -> [HTTP.Header] -> m ProperTrailers
parseProperTrailers proxy = HKD.sequenceChecked . parseProperTrailers' proxy

-- | Generalization of 'parseProperTrailers' that does not throw errors.
--
-- See also 'Network.GRPC.Spec.parseRequestHeaders' versus
-- ''Network.GRPC.Spec.parseRequestHeaders'' for a similar pair of functions.
-- See t'ProperTrailers'' for a discussion of why 'ProperTrailers'' is not
-- parameterized (unlike t'ResponseHeaders'' and
-- t'Network.GRPC.Spec.RequestHeaders'').
parseProperTrailers' :: forall rpc.
     IsRPC rpc
  => Proxy rpc -> [HTTP.Header] -> ProperTrailers'
parseProperTrailers' proxy hdrs =
    case trailersOnlyToProperTrailers trailersOnly of
      (properTrailers, Right Nothing) ->
         properTrailers
      (properTrailers, Right (Just _ct)) ->
         properTrailers {
             properTrailersUnrecognized = Left $ mconcat [
                 unexpectedHeader "content-type"
               , otherInvalid $ properTrailersUnrecognized properTrailers
               ]
           }
      (properTrailers, Left invalid) ->
         case dropSynthesized invalid of
           -- The content-type header is "invalid" because it's missing.
           -- In our case, this actually means everything is as it should be.
           InvalidHeaders [MissingHeader{}] ->
             properTrailers
           -- The @content-type@ header is present, /and/ invalid!
           _otherwise ->
             properTrailers {
               properTrailersUnrecognized = Left $ mconcat [
                   unexpectedHeader "content-type"
                 , invalid
                 , otherInvalid $ properTrailersUnrecognized properTrailers
                 ]
         }
  where
    trailersOnly :: TrailersOnly' GrpcException
    trailersOnly = parseTrailersOnly' proxy hdrs

-- | Parse t'TrailersOnly'
parseTrailersOnly :: forall m rpc.
     (IsRPC rpc, MonadError (InvalidHeaders GrpcException) m)
  => Proxy rpc -> [HTTP.Header] -> m TrailersOnly
parseTrailersOnly proxy = HKD.sequenceChecked . parseTrailersOnly' proxy

-- | Generalization of 'parseTrailersOnly' does that not throw errors.
--
-- See also 'Network.GRPC.Spec.parseRequestHeaders' versus
-- ''Network.GRPC.Spec.parseRequestHeaders'' for a similar pair of functions.
parseTrailersOnly' :: forall rpc.
     IsRPC rpc
  => Proxy rpc -> [HTTP.Header] -> TrailersOnly' GrpcException
parseTrailersOnly' proxy =
      flip execState uninitTrailersOnly
    . mapM_ (parseHeader . second trim)
  where
    parseHeader :: HTTP.Header -> State (TrailersOnly' GrpcException) ()
    parseHeader hdr@(name, value)
      | name == "content-type"
      = modify $ \x -> x {
            trailersOnlyContentType = Just <$> parseContentType' proxy hdr
          }

      | name == "grpc-status"
      = modify $ liftProperTrailers $ \x -> x{
            properTrailersGrpcStatus = throwInvalidHeader hdr $
              case toGrpcStatus =<< readMaybe (BS.Strict.C8.unpack value) of
                Nothing -> throwError $ "Invalid status: " ++ show value
                Just v  -> return v
          }

      | name == "grpc-message"
      = modify $ liftProperTrailers $ \x -> x{
            properTrailersGrpcMessage = throwInvalidHeader hdr $
              case PercentEncoding.decode value of
                Left  err -> throwError $ show err
                Right msg -> return (Just msg)
          }

      | name == "grpc-retry-pushback-ms"
      = modify $ liftProperTrailers $ \x -> x{
            properTrailersPushback =
              Just <$> parsePushback value
          }

      | name == "endpoint-load-metrics-bin"
      = modify $ liftProperTrailers $ \x -> x{
            properTrailersOrcaLoadReport = throwInvalidHeader hdr $ do
              value' <- parseBinaryValue value
              case Protobuf.parseStrict value' of
                Left  err    -> throwError err
                Right report -> return $ Just report
          }

      | otherwise
      = modify $ liftProperTrailers $ \x ->
          case parseCustomMetadata hdr of
            Left invalid -> x{
                properTrailersUnrecognized = Left $ mconcat [
                    invalid
                  , otherInvalid $ properTrailersUnrecognized x
                  ]
              }
            Right md -> x{
                properTrailersMetadata =
                  customMetadataMapInsert md $ properTrailersMetadata x
              }

    uninitTrailersOnly :: TrailersOnly' GrpcException
    uninitTrailersOnly = TrailersOnly {
          trailersOnlyContentType =
            throwError $
              invalidContentType
                "Missing content-type header"
                (MissingHeader Nothing "content-type")
        , trailersOnlyProper =
            simpleProperTrailers
              (throwError $ missingHeader Nothing "grpc-status")
              (return Nothing)
              mempty
        }

    liftProperTrailers ::
         (ProperTrailers_ f -> ProperTrailers_ f)
      -> TrailersOnly_ f -> TrailersOnly_ f
    liftProperTrailers f trailersOnly = trailersOnly{
          trailersOnlyProper = f (trailersOnlyProper trailersOnly)
        }

{-------------------------------------------------------------------------------
  Pushback
-------------------------------------------------------------------------------}

-- | Serialize t'Pushback'
buildPushback :: Pushback -> Strict.ByteString
buildPushback (RetryAfter n) = BS.Strict.C8.pack $ show n
buildPushback DoNotRetry     = "-1"

-- | Parse t'Pushback'
--
-- Parsing a pushback cannot fail; the spec mandates:
--
-- > If the value for pushback is negative or unparseble, then it will be seen
-- > as the server asking the client not to retry at all.
--
-- We therefore only require @Monad m@, not @MonadError m@ (having the @Monad@
-- constraint at all keeps the type signature consistent with other parsing
-- functions).
parsePushback :: Monad m => Strict.ByteString -> m Pushback
parsePushback bs =
    case readMaybe (BS.Strict.C8.unpack bs) of
      Just (n :: Int) ->
        -- The @Read@ instance for @Word@ /does/ allow for signs
        -- <https://gitlab.haskell.org/ghc/ghc/-/issues/24216>
        return $ if n < 0 then DoNotRetry else RetryAfter (fromIntegral n)
      Nothing ->
        return DoNotRetry

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

otherInvalid :: Either (InvalidHeaders e) () -> InvalidHeaders e
otherInvalid = either id (\() -> mempty)

decodeUtf8Lenient :: BS.Strict.C8.ByteString -> Text
#if MIN_VERSION_text(2,0,0)
decodeUtf8Lenient = Text.decodeUtf8Lenient
#else
decodeUtf8Lenient = Text.decodeUtf8With Text.lenientDecode
#endif
