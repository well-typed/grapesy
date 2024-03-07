{-# LANGUAGE OverloadedStrings #-}

-- | Deal with HTTP2 responses
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Response qualified as Resp
module Network.GRPC.Spec.Response (
    -- * Headers
    ResponseHeaders_(..)
  , ResponseHeaders
  , ProperTrailers_(..)
  , ProperTrailers
  , TrailersOnly_(..)
  , TrailersOnly
  , Pushback(..)
  , trailersOnlyToProperTrailers
  , properTrailersToTrailersOnly
    -- * gRPC exceptions
  , GrpcException(..)
  , grpcExceptionFromTrailers
  , grpcExceptionToTrailers
    -- * Serialization
    -- ** Construction
  , buildResponseHeaders
  , buildProperTrailers
  , buildTrailersOnly
  , buildPushback
    -- ** Parsing
  , parseResponseHeaders
  , parseProperTrailers
  , parseTrailersOnly
  , parsePushback
  ) where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString qualified as Strict
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.Text (Text)
import GHC.Stack
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)

import Network.GRPC.Spec.Common
import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.Status
import Network.GRPC.Util.HKD (HKD, Undecorated, DecoratedWith)
import Network.GRPC.Util.HKD qualified as HKD

{-------------------------------------------------------------------------------
  Outputs (messages received from the peer)
-------------------------------------------------------------------------------}

-- | Response headers
data ResponseHeaders_ f = ResponseHeaders {
      -- | Compression used for outbound messages
      responseCompression :: HKD f (Maybe CompressionId)

      -- | Compression accepted for inbound messages
    , responseAcceptCompression :: HKD f (Maybe (NonEmpty CompressionId))

      -- | Initial response metadata
      --
      -- The response can include additional metadata in the trailers; see
      -- 'properTrailersMetadata'.
    , responseMetadata :: HKD f (Map HeaderName HeaderValue)

      -- | Content-type
      --
      -- Set to 'Nothing' to omit the content-type header altogether.
    , responseContentType :: HKD f (Maybe ContentType)
    }

type ResponseHeaders = ResponseHeaders_ Undecorated

deriving stock instance Show ResponseHeaders
deriving stock instance Eq   ResponseHeaders

instance HKD.Traversable ResponseHeaders_ where
  sequence x =
      ResponseHeaders
        <$> responseCompression       x
        <*> responseAcceptCompression x
        <*> responseMetadata          x
        <*> responseContentType       x

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

      -- | Trailing metadata
      --
      -- See also 'responseMetadata' for the initial metadata.
    , properTrailersMetadata :: HKD f (Map HeaderName HeaderValue)

      -- | Server pushback
    , properTrailersPushback :: HKD f (Maybe Pushback)
    }

type ProperTrailers = ProperTrailers_ Undecorated

deriving stock instance Show ProperTrailers
deriving stock instance Eq   ProperTrailers

instance HKD.Traversable ProperTrailers_ where
  sequence x =
      ProperTrailers
        <$> properTrailersGrpcStatus  x
        <*> properTrailersGrpcMessage x
        <*> properTrailersMetadata    x
        <*> properTrailersPushback    x

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

type TrailersOnly = TrailersOnly_ Undecorated

deriving stock instance Show TrailersOnly
deriving stock instance Eq   TrailersOnly

instance HKD.Traversable TrailersOnly_ where
  sequence x =
      TrailersOnly
        <$> trailersOnlyContentType x
        <*> HKD.sequence (trailersOnlyProper x)

-- | 'ProperTrailers' is a subset of 'TrailersOnly'
properTrailersToTrailersOnly ::
     (ProperTrailers, Maybe ContentType)
  -> TrailersOnly
properTrailersToTrailersOnly (proper, ct) = TrailersOnly {
      trailersOnlyProper      = proper
    , trailersOnlyContentType = ct
    }

-- | 'TrailersOnly' is a superset of 'ProperTrailers'
trailersOnlyToProperTrailers ::
      TrailersOnly
   -> (ProperTrailers, Maybe ContentType)
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
  deriving (Show, Eq)

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
data GrpcException = GrpcException {
      grpcError         :: GrpcError
    , grpcErrorMessage  :: Maybe Text
    , grpcErrorMetadata :: [CustomMetadata]
    }
  deriving stock (Show, Eq)
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
grpcExceptionFromTrailers ::
     ProperTrailers
  -> Either (Maybe Text, [CustomMetadata]) GrpcException
grpcExceptionFromTrailers ProperTrailers {
                              properTrailersGrpcStatus
                            , properTrailersGrpcMessage
                            , properTrailersMetadata
                            } =
    case properTrailersGrpcStatus of
      GrpcOk -> Left (
          properTrailersGrpcMessage
        , Map.toList properTrailersMetadata
        )
      GrpcError err -> Right GrpcException{
          grpcError         = err
        , grpcErrorMessage  = properTrailersGrpcMessage
        , grpcErrorMetadata = Map.toList properTrailersMetadata
        }

-- | Translate gRPC exception to response trailers
grpcExceptionToTrailers ::  GrpcException -> ProperTrailers
grpcExceptionToTrailers err = ProperTrailers{
      properTrailersGrpcStatus  = GrpcError (grpcError err)
    , properTrailersGrpcMessage = grpcErrorMessage err
    , properTrailersMetadata    = Map.fromList $ grpcErrorMetadata  err
    , properTrailersPushback    = Nothing
    }

{-------------------------------------------------------------------------------
  > Response-Headers →
  >   HTTP-Status
  >   [Message-Encoding]
  >   [Message-Accept-Encoding]
  >   Content-Type
  >   *Custom-Metadata

  We do not deal with @HTTP-Status@ here; @http2@ deals this separately.

  TODO: We should attempt to be more lenient in our parsing here, and throw
  fewer errors. Perhaps have an @Invalid@ constructor or something, so that we
  can mark incoming headers that were not valid, but still give them to the
  user, but then throw an error if we try to /send/ those.

  TODO: Related to the above, the spec says: "Implementations MUST accept padded
  and un-padded values and should emit un-padded values." We don't currently do
  this for incoming headers.
-------------------------------------------------------------------------------}

-- | Build response headers
buildResponseHeaders ::
     IsRPC rpc
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
    , [ buildCustomMetadata x
      | x <- Map.toList responseMetadata
      ]
    ]

-- | Parse response headers
parseResponseHeaders :: forall rpc m.
     (IsRPC rpc, MonadError String m)
  => Proxy rpc -> [HTTP.Header] -> m ResponseHeaders
parseResponseHeaders proxy =
      HKD.sequence
    . flip execState uninitResponseHeaders
    . mapM_ parseHeader
  where
    -- HTTP2 header names are always lowercase, and must be ASCII.
    -- <https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.2>
    parseHeader :: HTTP.Header -> State (ResponseHeaders_ (DecoratedWith m)) ()
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

      | otherwise
      = modify $ \x -> x {
            responseMetadata = do
              (parsedName, parsedValue) <- parseCustomMetadata hdr
              Map.insert parsedName parsedValue <$> responseMetadata x
          }

    uninitResponseHeaders :: ResponseHeaders_ (DecoratedWith m)
    uninitResponseHeaders = ResponseHeaders {
          responseCompression       = return Nothing
        , responseAcceptCompression = return Nothing
        , responseMetadata          = return Map.empty
        , responseContentType       = return Nothing
        }

{-------------------------------------------------------------------------------
  > Trailers → Status [Status-Message] *Custom-Metadata
-------------------------------------------------------------------------------}

-- | Build trailers (see 'buildTrailersOnly' for the Trailers-Only case)
buildProperTrailers :: ProperTrailers -> [HTTP.Header]
buildProperTrailers ProperTrailers{
                        properTrailersGrpcStatus
                      , properTrailersGrpcMessage
                      , properTrailersMetadata
                      , properTrailersPushback
                      } = concat [
      [ ( "grpc-status"
        , BS.Strict.C8.pack $ show $ fromGrpcStatus properTrailersGrpcStatus
        )
      ]
    , [ ("grpc-message", PercentEncoding.encode x)
      | Just x <- [properTrailersGrpcMessage]
      ]
    , [ buildCustomMetadata x
      | x <- Map.toList properTrailersMetadata
      ]
    , [ ( "grpc-retry-pushback-ms"
        , buildPushback x
        )
      | Just x <- [properTrailersPushback]
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
     (IsRPC rpc, MonadError String m, HasCallStack)
  => Proxy rpc -> [HTTP.Header] -> m ProperTrailers
parseProperTrailers proxy hdrs = do
    trailersOnly <- parseTrailersOnly proxy hdrs
    case trailersOnlyToProperTrailers trailersOnly of
      (properTrailers, Nothing) ->
        return properTrailers
      (_, Just ct) ->
        throwError $ concat [
            "parseProperTrailers: unexpected "
          , show ct
          , " at "
          , prettyCallStack callStack
          ]

-- | Parse trailers in the gRPC @Trailers-Only@ case
parseTrailersOnly :: forall m rpc.
     (IsRPC rpc, MonadError String m)
  => Proxy rpc -> [HTTP.Header] -> m TrailersOnly
parseTrailersOnly proxy =
      HKD.sequence
    . flip execState uninitTrailersOnly
    . mapM_ parseHeader
  where
    parseHeader :: HTTP.Header -> State (TrailersOnly_ (DecoratedWith m)) ()
    parseHeader hdr@(name, value)
      | name == "content-type"
      = modify $ \x -> x {
            trailersOnlyContentType = Just <$> parseContentType proxy hdr
          }

      | name == "grpc-status"
      = modify $ liftProperTrailers $ \x -> x{
            properTrailersGrpcStatus =
              case toGrpcStatus =<< readMaybe (BS.Strict.C8.unpack value) of
                Nothing -> throwError $ "Invalid status: " ++ show value
                Just v  -> return v
          }

      | name == "grpc-message"
      = modify $ liftProperTrailers $ \x -> x{
            properTrailersGrpcMessage =
              case PercentEncoding.decode value of
                Left  err -> throwError $ show err
                Right msg -> return (Just msg)
          }

      | name == "grpc-retry-pushback-ms"
      = modify $ liftProperTrailers $ \x -> x{
            properTrailersPushback =
              Just <$> parsePushback value
          }

      | otherwise
      = modify $ liftProperTrailers $ \x -> x{
            properTrailersMetadata = do
              (parsedName, parsedValue) <- parseCustomMetadata hdr
              Map.insert parsedName parsedValue <$> properTrailersMetadata x
          }

    uninitTrailersOnly :: TrailersOnly_ (DecoratedWith m)
    uninitTrailersOnly = TrailersOnly {
          trailersOnlyContentType = return Nothing
        , trailersOnlyProper      = ProperTrailers {
              properTrailersGrpcStatus  = throwError "missing: grpc-status"
            , properTrailersGrpcMessage = return Nothing
            , properTrailersMetadata    = return Map.empty
            , properTrailersPushback    = return Nothing
            }
        }

    liftProperTrailers ::
         (ProperTrailers_ f -> ProperTrailers_ f)
      -> TrailersOnly_ f -> TrailersOnly_ f
    liftProperTrailers f trailersOnly = trailersOnly{
          trailersOnlyProper = f (trailersOnlyProper trailersOnly)
        }