{-# LANGUAGE OverloadedStrings #-}

-- | Deal with HTTP2 responses
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Response qualified as Resp
module Network.GRPC.Spec.Headers.Response (
    -- * Headers
    ResponseHeaders_(..)
  , ResponseHeaders
  , ResponseHeaders'
  , ProperTrailers_(..)
  , ProperTrailers
  , ProperTrailers'
  , TrailersOnly_(..)
  , TrailersOnly
  , TrailersOnly'
  , Pushback(..)
  , simpleProperTrailers
  , trailersOnlyToProperTrailers
  , properTrailersToTrailersOnly
    -- * gRPC termination
  , GrpcException(..)
  , throwGrpcError
  , GrpcNormalTermination(..)
  , grpcClassifyTermination
  , grpcExceptionToTrailers
    -- * Serialization
    -- ** Construction
  , buildResponseHeaders
  , buildProperTrailers
  , buildTrailersOnly
  , buildPushback
    -- ** Parsing
  , parseResponseHeaders
  , parseResponseHeaders'
  , parseProperTrailers
  , parseProperTrailers'
  , parseTrailersOnly
  , parseTrailersOnly'
  , parsePushback
  ) where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.CaseInsensitive qualified as CI
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)

import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.CustomMetadata.Map
import Network.GRPC.Spec.CustomMetadata.Raw
import Network.GRPC.Spec.CustomMetadata.Typed
import Network.GRPC.Spec.Headers.Common
import Network.GRPC.Spec.Headers.Invalid
import Network.GRPC.Spec.OrcaLoadReport
import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.Status
import Network.GRPC.Util.HKD (HKD, Undecorated, DecoratedWith)
import Network.GRPC.Util.HKD qualified as HKD
import Network.GRPC.Util.Protobuf qualified as Protobuf

{-------------------------------------------------------------------------------
  Outputs (messages received from the peer)
-------------------------------------------------------------------------------}

-- | Response headers
data ResponseHeaders_ f = ResponseHeaders {
      -- | Compression used for outbound messages
      responseCompression :: HKD f (Maybe CompressionId)

      -- | Compression accepted for inbound messages
    , responseAcceptCompression :: HKD f (Maybe (NonEmpty CompressionId))

      -- | Content-type
      --
      -- Set to 'Nothing' to omit the content-type header altogether.
    , responseContentType :: HKD f (Maybe ContentType)

      -- | Initial response metadata
      --
      -- The response can include additional metadata in the trailers; see
      -- 'properTrailersMetadata'.
    , responseMetadata :: CustomMetadataMap

      -- | Unrecognized headers
    , responseUnrecognized :: HKD f ()
    }

-- | Response headers (without allowing for invalid headers)
--
-- See 'RequestHeaders' for an explanation of @Undecorated@.
type ResponseHeaders = ResponseHeaders_ Undecorated

-- | Response headers allowing for invalid headers
--
-- See 'RequestHeaders'' for an explanation of @DecoratedWith@.
type ResponseHeaders' = ResponseHeaders_ (DecoratedWith (Either InvalidHeaders))

deriving stock instance Show    ResponseHeaders
deriving stock instance Eq      ResponseHeaders
deriving stock instance Generic ResponseHeaders

deriving stock instance Show ResponseHeaders'
deriving stock instance Eq   ResponseHeaders'

instance HKD.Traversable ResponseHeaders_ where
  sequence x =
      ResponseHeaders
        <$> responseCompression       x
        <*> responseAcceptCompression x
        <*> responseContentType       x
        <*> pure (responseMetadata    x)
        <*> responseUnrecognized      x

-- | Information sent by the peer after the final output
--
-- Response trailers are a
-- [HTTP2 concept](https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.3):
-- they are HTTP headers that are sent /after/ the content body. For example,
-- imagine the server is streaming a file that it's reading from disk; it could
-- use trailers to give the client an MD5 checksum when streaming is complete.
data ProperTrailers_ f = ProperTrailers {
      -- | gPRC status
      properTrailersGrpcStatus :: HKD f GrpcStatus

      -- | Additional status message
      --
      -- NOTE: in @grapesy@ this message is only used in error cases.
    , properTrailersGrpcMessage :: HKD f (Maybe Text)

      -- | Server pushback
      --
      -- This is part of automatic retries.
      -- See <https://github.com/grpc/proposal/blob/master/A6-client-retries.md>.
    , properTrailersPushback :: HKD f (Maybe Pushback)

      -- | ORCA load report
      --
      -- See <https://github.com/grpc/proposal/blob/master/A51-custom-backend-metrics.md>
    , properTrailersOrcaLoadReport :: HKD f (Maybe OrcaLoadReport)

      -- | Trailing metadata
      --
      -- See also 'responseMetadata' for the initial metadata.
    , properTrailersMetadata :: CustomMetadataMap

      -- | Unrecognized trailers
    , properTrailersUnrecognized :: HKD f ()
    }

-- | Default constructor for 'ProperTrailers'
simpleProperTrailers :: forall f.
     HKD.ValidDecoration Applicative f
  => HKD f GrpcStatus
  -> HKD f (Maybe Text)
  -> CustomMetadataMap
  -> ProperTrailers_ f
simpleProperTrailers status msg metadata = ProperTrailers {
      properTrailersGrpcStatus     = status
    , properTrailersGrpcMessage    = msg
    , properTrailersPushback       = HKD.pure (Proxy @f) (Nothing :: Maybe Pushback)
    , properTrailersOrcaLoadReport = HKD.pure (Proxy @f) (Nothing :: Maybe OrcaLoadReport)
    , properTrailersMetadata       = metadata
    , properTrailersUnrecognized   = HKD.pure (Proxy @f) ()
    }

-- | Trailers sent after the response (without allowing for invalid trailers)
type ProperTrailers = ProperTrailers_ Undecorated

-- | Trailers sent after the response, allowing for invalid trailers
type ProperTrailers' = ProperTrailers_ (DecoratedWith (Either InvalidHeaders))

deriving stock instance Show    ProperTrailers
deriving stock instance Eq      ProperTrailers
deriving stock instance Generic ProperTrailers

deriving stock instance Show ProperTrailers'
deriving stock instance Eq   ProperTrailers'

instance HKD.Traversable ProperTrailers_ where
  sequence x =
      ProperTrailers
        <$> properTrailersGrpcStatus     x
        <*> properTrailersGrpcMessage    x
        <*> properTrailersPushback       x
        <*> properTrailersOrcaLoadReport x
        <*> pure (properTrailersMetadata x)
        <*> properTrailersUnrecognized   x

-- | Trailers sent in the gRPC Trailers-Only case
--
-- We deal with the HTTP status elsewhere.
data TrailersOnly_ f = TrailersOnly {
      -- | Content type
      --
      -- Set to 'Nothing' to omit the content-type altogether.
      trailersOnlyContentType :: HKD f (Maybe ContentType)

      -- | All regular trailers can also appear in the Trailers-Only case
    , trailersOnlyProper :: ProperTrailers_ f
    }

-- | Trailers for the Trailers-Only case (without allowing for invalid trailers)
type TrailersOnly = TrailersOnly_ Undecorated

-- | Trailers for the Trailers-Only case, allowing for invalid headers
type TrailersOnly' = TrailersOnly_ (DecoratedWith (Either InvalidHeaders))

deriving stock instance Show    TrailersOnly
deriving stock instance Eq      TrailersOnly
deriving stock instance Generic TrailersOnly

deriving stock instance Show TrailersOnly'
deriving stock instance Eq   TrailersOnly'

instance HKD.Traversable TrailersOnly_ where
  sequence x =
      TrailersOnly
        <$> trailersOnlyContentType x
        <*> HKD.sequence (trailersOnlyProper x)

-- | 'ProperTrailers' is a subset of 'TrailersOnly'
properTrailersToTrailersOnly ::
     (ProperTrailers_ f, HKD f (Maybe ContentType))
  -> TrailersOnly_ f
properTrailersToTrailersOnly (proper, ct) = TrailersOnly {
      trailersOnlyProper      = proper
    , trailersOnlyContentType = ct
    }

-- | 'TrailersOnly' is a superset of 'ProperTrailers'
trailersOnlyToProperTrailers ::
      TrailersOnly_ f
   -> (ProperTrailers_ f, HKD f (Maybe ContentType))
trailersOnlyToProperTrailers TrailersOnly{
                                 trailersOnlyProper
                               , trailersOnlyContentType
                               } = (
      trailersOnlyProper
    , trailersOnlyContentType
    )

{-------------------------------------------------------------------------------
  Pushback
-------------------------------------------------------------------------------}

-- | Pushback
--
-- The server adds this header to push back against client retries. We do not
-- yet support automatic retries
-- (<https://github.com/well-typed/grapesy/issues/104>), but do /we/ parse this
-- header so that /if/ the server includes it, we do not throw a parser error.
--
-- See also <https://github.com/grpc/proposal/blob/master/A6-client-retries.md>
data Pushback =
    RetryAfter Word
  | DoNotRetry
  deriving (Show, Eq, Generic)

buildPushback :: Pushback -> Strict.ByteString
buildPushback (RetryAfter n) = BS.Strict.C8.pack $ show n
buildPushback DoNotRetry     = "-1"

-- | Parse 'Pushback'
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
  gRPC exceptions
-------------------------------------------------------------------------------}

-- | Server indicated a gRPC error
--
-- For the common case where you just want to set 'grpcError', you can use
-- 'throwGrpcError'.
data GrpcException = GrpcException {
      grpcError          :: GrpcError
    , grpcErrorMessage   :: Maybe Text
    , grpcErrorMetadata  :: [CustomMetadata]
    }
  deriving stock (Show)
  deriving anyclass (Exception)

throwGrpcError :: GrpcError -> IO a
throwGrpcError grpcError = throwIO $ GrpcException {
      grpcError
    , grpcErrorMessage  = Nothing
    , grpcErrorMetadata = []
    }

-- | Server indicated normal termination
--
-- This is only an exception if the client tries to send any further messages.
data GrpcNormalTermination = GrpcNormalTermination {
      grpcTerminatedMetadata :: [CustomMetadata]
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Check if trailers correspond to an exceptional response
--
-- The gRPC spec states that
--
-- > Trailers-Only is permitted for calls that produce an immediate error
--
-- However, in practice gRPC servers can also respond with @Trailers-Only@ in
-- non-error cases, simply indicating that the server considers the
-- conversation over. To distinguish, we look at 'trailerGrpcStatus'.
grpcClassifyTermination ::
     ProperTrailers'
  -> Either GrpcException GrpcNormalTermination
grpcClassifyTermination ProperTrailers {
                              properTrailersGrpcStatus
                            , properTrailersGrpcMessage
                            , properTrailersMetadata
                            } =
    case properTrailersGrpcStatus of
      Right GrpcOk -> Right GrpcNormalTermination {
          grpcTerminatedMetadata = customMetadataMapToList properTrailersMetadata
        }
      Right (GrpcError err) -> Left GrpcException{
          grpcError         = err
        , grpcErrorMessage  = case properTrailersGrpcMessage of
                                Right msg -> msg
                                Left  _   -> Just "Invalid grpc-message"
        , grpcErrorMetadata = customMetadataMapToList properTrailersMetadata
        }
      Left _invalidStatus -> Left GrpcException {
          grpcError         = GrpcUnknown
        , grpcErrorMessage  = Just "Invalid grpc-status"
        , grpcErrorMetadata = customMetadataMapToList properTrailersMetadata
        }

-- | Translate gRPC exception to response trailers
grpcExceptionToTrailers ::  GrpcException -> ProperTrailers
grpcExceptionToTrailers GrpcException{
                            grpcError
                          , grpcErrorMessage
                          , grpcErrorMetadata
                          } =
    simpleProperTrailers
      (GrpcError grpcError)
      grpcErrorMessage
      (customMetadataMapFromList grpcErrorMetadata)

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
      [ buildContentType proxy x
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
     (IsRPC rpc, MonadError InvalidHeaders m)
  => Proxy rpc -> [HTTP.Header] -> m ResponseHeaders
parseResponseHeaders proxy = HKD.sequenceThrow . parseResponseHeaders' proxy

parseResponseHeaders' :: forall rpc.
     IsRPC rpc
  => Proxy rpc -> [HTTP.Header] -> ResponseHeaders'
parseResponseHeaders' proxy =
      flip execState uninitResponseHeaders
    . mapM_ (parseHeader . second trim)
  where
    -- HTTP2 header names are always lowercase, and must be ASCII.
    -- <https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.2>
    parseHeader :: HTTP.Header -> State ResponseHeaders' ()
    parseHeader hdr@(name, _value)
      | name == "content-type"
      = modify $ \x -> x {
            responseContentType = Just <$> parseContentType proxy hdr
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

    uninitResponseHeaders :: ResponseHeaders'
    uninitResponseHeaders = ResponseHeaders {
          responseCompression       = return Nothing
        , responseAcceptCompression = return Nothing
        , responseContentType       = return Nothing
        , responseMetadata          = mempty
        , responseUnrecognized      = return ()
        }

{-------------------------------------------------------------------------------
  > Trailers → Status [Status-Message] *Custom-Metadata
-------------------------------------------------------------------------------}

-- | Construct the HTTP 'Trailer' header
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
--
-- NOTE: If we add additional (reserved) headers here, we also need to add them
-- to 'buildTrailer'.
buildProperTrailers :: ProperTrailers -> [HTTP.Header]
buildProperTrailers ProperTrailers{
                        properTrailersGrpcStatus
                      , properTrailersGrpcMessage
                      , properTrailersMetadata
                      , properTrailersPushback
                      , properTrailersOrcaLoadReport
                      } = concat [
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
buildTrailersOnly :: IsRPC rpc => Proxy rpc -> TrailersOnly -> [HTTP.Header]
buildTrailersOnly proxy TrailersOnly{
                            trailersOnlyContentType
                          , trailersOnlyProper
                          } = concat [
      [ buildContentType proxy x
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
     (IsRPC rpc, MonadError InvalidHeaders m)
  => Proxy rpc -> [HTTP.Header] -> m ProperTrailers
parseProperTrailers proxy = HKD.sequenceThrow . parseProperTrailers' proxy

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
         -- The @content-type@ header is present, /and/ invalid!
         properTrailers {
           properTrailersUnrecognized = Left $ mconcat [
               unexpectedHeader "content-type"
             , invalid
             , otherInvalid $ properTrailersUnrecognized properTrailers
             ]
         }
  where
    trailersOnly :: TrailersOnly'
    trailersOnly = parseTrailersOnly' proxy hdrs

parseTrailersOnly :: forall m rpc.
     (IsRPC rpc, MonadError InvalidHeaders m)
  => Proxy rpc -> [HTTP.Header] -> m TrailersOnly
parseTrailersOnly proxy = HKD.sequenceThrow . parseTrailersOnly' proxy

parseTrailersOnly' :: forall rpc.
     IsRPC rpc
  => Proxy rpc -> [HTTP.Header] -> TrailersOnly'
parseTrailersOnly' proxy =
      flip execState uninitTrailersOnly
    . mapM_ (parseHeader . second trim)
  where
    parseHeader :: HTTP.Header -> State TrailersOnly' ()
    parseHeader hdr@(name, value)
      | name == "content-type"
      = modify $ \x -> x {
            trailersOnlyContentType = Just <$> parseContentType proxy hdr
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

    uninitTrailersOnly :: TrailersOnly'
    uninitTrailersOnly = TrailersOnly {
          trailersOnlyContentType = return Nothing
        , trailersOnlyProper      = simpleProperTrailers
                                      (throwError $ missingHeader "grpc-status")
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
  Internal auxiliary
-------------------------------------------------------------------------------}

otherInvalid :: Either InvalidHeaders () -> InvalidHeaders
otherInvalid = either id (\() -> mempty)

