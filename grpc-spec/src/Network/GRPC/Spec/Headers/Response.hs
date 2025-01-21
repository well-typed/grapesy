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
    -- * Termination
  , GrpcNormalTermination(..)
  , grpcClassifyTermination
  , grpcExceptionToTrailers
  ) where

import Control.Exception
import Control.Monad.Except (throwError)
import Data.ByteString qualified as Strict (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Text (Text)
import GHC.Generics (Generic)

import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.CustomMetadata.Map
import Network.GRPC.Spec.CustomMetadata.Raw
import Network.GRPC.Spec.Headers.Common
import Network.GRPC.Spec.Headers.Invalid
import Network.GRPC.Spec.OrcaLoadReport
import Network.GRPC.Spec.Status
import Network.GRPC.Spec.Util.HKD (HKD, Undecorated, Checked)
import Network.GRPC.Spec.Util.HKD qualified as HKD

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
  deriving anyclass (HKD.Coerce)

-- | Response headers (without allowing for invalid headers)
--
-- See t'Network.GRPC.Spec.RequestHeaders' for an explanation of 'Undecorated'.
type ResponseHeaders = ResponseHeaders_ Undecorated

-- | Response headers allowing for invalid headers
--
-- See t'Network.GRPC.Spec.RequestHeaders'' for an explanation of 'Checked' and
-- the purpose of @e@.
type ResponseHeaders' e =  ResponseHeaders_ (Checked (InvalidHeaders e))

deriving stock instance Show    ResponseHeaders
deriving stock instance Eq      ResponseHeaders
deriving stock instance Generic ResponseHeaders

deriving stock instance Show e => Show (ResponseHeaders_ (Checked e))
deriving stock instance Eq   e => Eq   (ResponseHeaders_ (Checked e))

instance HKD.Traversable ResponseHeaders_ where
  traverse f x =
      ResponseHeaders
        <$> (f    $ responseCompression       x)
        <*> (f    $ responseAcceptCompression x)
        <*> (f    $ responseContentType       x)
        <*> (pure $ responseMetadata          x)
        <*> (f    $ responseUnrecognized      x)

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
    , properTrailersGrpcMessage :: HKD f (Maybe Text)

      -- | Status details
      --
      -- This can be used to provide additional details about the RPC error;
      -- as this is a binary field, it can be used for structured data.
      --
      -- The spec imposes some additional restrictions on this field:
      --
      -- * @Status-Details@ is allowed only if @Status@ is not OK.
      -- * When using Protobuf this contains a @google.rpc.Status@ message.
      -- * If it contains a status code (as in the case of a @google.rpc.Status@
      --   message), it MUST NOT contradict the Status header.
      --
      -- The spec additionally mandates that consumers MUST verify that third
      -- requirement; however, it is impossible to verify this unless a specific
      -- format for the status details is known.
    , properTrailersStatusDetails :: HKD f (Maybe Strict.ByteString)

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
  deriving anyclass (HKD.Coerce)

-- | Default constructor for t'ProperTrailers'
simpleProperTrailers :: forall f.
     HKD.ValidDecoration Applicative f
  => HKD f GrpcStatus
  -> HKD f (Maybe Text)
  -> CustomMetadataMap
  -> ProperTrailers_ f
simpleProperTrailers status msg metadata = ProperTrailers {
      properTrailersGrpcStatus     = status
    , properTrailersGrpcMessage    = msg
    , properTrailersStatusDetails  = HKD.pure (Proxy @f) (Nothing :: Maybe Strict.ByteString)
    , properTrailersPushback       = HKD.pure (Proxy @f) (Nothing :: Maybe Pushback)
    , properTrailersOrcaLoadReport = HKD.pure (Proxy @f) (Nothing :: Maybe OrcaLoadReport)
    , properTrailersMetadata       = metadata
    , properTrailersUnrecognized   = HKD.pure (Proxy @f) ()
    }

-- | Trailers sent after the response (without allowing for invalid trailers)
type ProperTrailers = ProperTrailers_ Undecorated

-- | Trailers sent after the response, allowing for invalid trailers
--
-- We do not parameterize this over the type of synthesized errors: unlike
-- response (or request) headers, we have no opportunity to check the trailers
-- for synthesized errors ahead of time, so having a type to signal
-- "trailers without synthesized errors" is not particularly useful.
type ProperTrailers' = ProperTrailers_ (Checked (InvalidHeaders GrpcException))

deriving stock instance Show    ProperTrailers
deriving stock instance Eq      ProperTrailers
deriving stock instance Generic ProperTrailers

deriving stock instance Show e => Show (ProperTrailers_ (Checked e))
deriving stock instance Eq   e => Eq   (ProperTrailers_ (Checked e))

instance HKD.Traversable ProperTrailers_ where
  traverse f x =
      ProperTrailers
        <$> (f    $ properTrailersGrpcStatus     x)
        <*> (f    $ properTrailersGrpcMessage    x)
        <*> (f    $ properTrailersStatusDetails  x)
        <*> (f    $ properTrailersPushback       x)
        <*> (f    $ properTrailersOrcaLoadReport x)
        <*> (pure $ properTrailersMetadata       x)
        <*> (f    $ properTrailersUnrecognized   x)

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
  deriving anyclass (HKD.Coerce)

-- | Trailers for the Trailers-Only case (without allowing for invalid trailers)
type TrailersOnly = TrailersOnly_ Undecorated

-- | Trailers for the Trailers-Only case, allowing for invalid headers
type TrailersOnly' e = TrailersOnly_ (Checked (InvalidHeaders e))

deriving stock instance Show    TrailersOnly
deriving stock instance Eq      TrailersOnly
deriving stock instance Generic TrailersOnly

deriving stock instance Show e => Show (TrailersOnly_ (Checked e))
deriving stock instance Eq   e => Eq   (TrailersOnly_ (Checked e))

instance HKD.Traversable TrailersOnly_ where
  traverse f x =
      TrailersOnly
        <$> (f              $ trailersOnlyContentType x)
        <*> (HKD.traverse f $ trailersOnlyProper      x)

-- | t'ProperTrailers' is a subset of t'TrailersOnly'
properTrailersToTrailersOnly ::
     (ProperTrailers_ f, HKD f (Maybe ContentType))
  -> TrailersOnly_ f
properTrailersToTrailersOnly (proper, ct) = TrailersOnly {
      trailersOnlyProper      = proper
    , trailersOnlyContentType = ct
    }

-- | t'TrailersOnly' is a superset of t'ProperTrailers'
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

{-------------------------------------------------------------------------------
  Termination
-------------------------------------------------------------------------------}

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
-- conversation over. To distinguish, we look at 'properTrailersGrpcStatus'.
grpcClassifyTermination ::
     ProperTrailers'
  -> Either GrpcException GrpcNormalTermination
grpcClassifyTermination =
    -- If there are any synthesized errors, those take precedence
    either Left aux . throwSynthesized throwError
  where
    aux ::
         ProperTrailers_ (Checked (InvalidHeaders HandledSynthesized))
      -> Either GrpcException GrpcNormalTermination
    aux ProperTrailers { properTrailersGrpcStatus
                       , properTrailersGrpcMessage
                       , properTrailersMetadata
                       } =
        case properTrailersGrpcStatus of
          Right GrpcOk -> Right GrpcNormalTermination {
              grpcTerminatedMetadata =
                customMetadataMapToList properTrailersMetadata
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
