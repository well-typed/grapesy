{-# LANGUAGE OverloadedStrings #-}

-- | Deal with HTTP2 responses
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Response qualified as Resp
module Network.GRPC.Spec.Response (
    -- * Headers
    ResponseHeaders(..)
  , ProperTrailers(..)
  , TrailersOnly(..)
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
    -- ** Parsing
  , parseResponseHeaders
  , parseProperTrailers
  , parseTrailersOnly
  ) where

import Control.Exception
import Control.Monad.Except
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.SOP
import Data.Text (Text)
import Generics.SOP qualified as SOP
import GHC.Generics qualified as GHC
import GHC.Stack
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)

import Network.GRPC.Spec.Common
import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.Status
import Network.GRPC.Util.Partial

{-------------------------------------------------------------------------------
  Outputs (messages received from the peer)
-------------------------------------------------------------------------------}

-- | Response headers
data ResponseHeaders = ResponseHeaders {
      -- | Compression used for outbound messages
      responseCompression :: Maybe CompressionId

      -- | Compression accepted for inbound messages
    , responseAcceptCompression :: Maybe (NonEmpty CompressionId)

      -- | Initial response metadata
      --
      -- The response can include additional metadata in the trailers; see
      -- 'properTrailersMetadata'.
    , responseMetadata :: Map HeaderName HeaderValue

      -- | Content-type
      --
      -- Set to 'Nothing' to omit the content-type header altogether.
    , responseContentType :: Maybe ContentType
    }
  deriving stock (Show, Eq)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Information sent by the peer after the final output
--
-- Response trailers are a
-- [HTTP2 concept](https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.3):
-- they are HTTP headers that are sent /after/ the content body. For example,
-- imagine the server is streaming a file that it's reading from disk; it could
-- use trailers to give the client an MD5 checksum when streaming is complete.
data ProperTrailers = ProperTrailers {
      -- | gPRC status
      properTrailersGrpcStatus :: GrpcStatus

      -- | Additional status message
      --
      -- NOTE: in @grapesy@ this message is only used in error cases.
    , properTrailersGrpcMessage :: Maybe Text

      -- | Trailing metadata
      --
      -- See also 'responseMetadata' for the initial metadata.
    , properTrailersMetadata :: Map HeaderName HeaderValue
    }
  deriving stock (Show, Eq)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Trailers sent in the gRPC Trailers-Only case
--
-- We deal with the HTTP status elsewhere.
data TrailersOnly = TrailersOnly {
      -- | Analogue of 'properTrailersGrpcStatus'
      trailersOnlyGrpcStatus :: GrpcStatus

      -- | Analogue of 'properTrailersGrpcMessage'
    , trailersOnlyGrpcMessage :: Maybe Text

      -- | Analogue of 'properTrailersMetadata'
    , trailersOnlyMetadata :: Map HeaderName HeaderValue

      -- | Content type
      --
      -- Set to 'Nothing' to omit the content-type altogether.
    , trailersOnlyContentType :: Maybe ContentType
    }
  deriving stock (Show, Eq)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | 'ProperTrailers' is a subset of 'TrailersOnly'
properTrailersToTrailersOnly ::
     (ProperTrailers, Maybe ContentType)
  -> TrailersOnly
properTrailersToTrailersOnly (x, ct) = TrailersOnly {
      trailersOnlyGrpcStatus  = properTrailersGrpcStatus  x
    , trailersOnlyGrpcMessage = properTrailersGrpcMessage x
    , trailersOnlyMetadata    = properTrailersMetadata    x
    , trailersOnlyContentType = ct
    }

-- | 'TrailersOnly' is a superset of 'ProperTrailers'
trailersOnlyToProperTrailers ::
      TrailersOnly
   -> (ProperTrailers, Maybe ContentType)
trailersOnlyToProperTrailers x = (
      ProperTrailers{
         properTrailersGrpcStatus  = trailersOnlyGrpcStatus  x
       , properTrailersGrpcMessage = trailersOnlyGrpcMessage x
       , properTrailersMetadata    = trailersOnlyMetadata    x
       }
    , trailersOnlyContentType x
    )

{-------------------------------------------------------------------------------
  gRPC exceptions
-------------------------------------------------------------------------------}

-- | Server indicated a gRPC error
data GrpcException = GrpcException {
      grpcError         :: GrpcError
    , grpcErrorMessage  :: Maybe Text
    , grpcErrorMetadata :: [CustomMetadata]
    }
  deriving stock (Show, Eq, GHC.Generic)
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
grpcExceptionFromTrailers (ProperTrailers status msg metadata) =
    case status of
      GrpcOk        -> Left (msg, Map.toList $ metadata)
      GrpcError err -> Right GrpcException{
          grpcError         = err
        , grpcErrorMessage  = msg
        , grpcErrorMetadata = Map.toList $ metadata
        }

-- | Translate gRPC exception to response trailers
grpcExceptionToTrailers ::  GrpcException -> ProperTrailers
grpcExceptionToTrailers err = ProperTrailers{
      properTrailersGrpcStatus  = GrpcError (grpcError err)
    , properTrailersGrpcMessage = grpcErrorMessage err
    , properTrailersMetadata    = Map.fromList $ grpcErrorMetadata  err
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
      runPartialParser uninitResponseHeaders
    . mapM_ parseHeader
  where
    -- HTTP2 header names are always lowercase, and must be ASCII.
    -- <https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.2>
    parseHeader :: HTTP.Header -> PartialParser ResponseHeaders m ()
    parseHeader hdr@(name, _value)
      | name == "content-type"
      = update updContentType $ \_ ->
          Just <$> parseContentType proxy hdr

      | name == "grpc-encoding"
      = update updCompression $ \_ ->
          Just <$> parseMessageEncoding hdr

      | name == "grpc-accept-encoding"
      = update updAcceptCompression $ \_ ->
          Just <$> parseMessageAcceptEncoding hdr

      | otherwise
      = parseCustomMetadata hdr >>=
        update updCustom . fmap . uncurry Map.insert

    uninitResponseHeaders :: Partial m ResponseHeaders
    uninitResponseHeaders =
           return (Nothing   :: Maybe CompressionId)
        :* return (Nothing   :: Maybe (NonEmpty CompressionId))
        :* return (Map.empty :: Map HeaderName HeaderValue)
        :* return (Nothing   :: Maybe ContentType)
        :* Nil

    (    updCompression
      :* updAcceptCompression
      :* updCustom
      :* updContentType
      :* Nil ) = partialUpdates (Proxy @ResponseHeaders)

{-------------------------------------------------------------------------------
  > Trailers → Status [Status-Message] *Custom-Metadata
-------------------------------------------------------------------------------}

-- | Build trailers (see 'buildTrailersOnly' for the Trailers-Only case)
buildProperTrailers :: IsRPC rpc => Proxy rpc -> ProperTrailers -> [HTTP.Header]
buildProperTrailers proxy properTrailers =
    buildTrailersOnly proxy $
      properTrailersToTrailersOnly (properTrailers, Nothing)

-- | Build trailers for the Trailers-Only case
buildTrailersOnly :: IsRPC rpc => Proxy rpc -> TrailersOnly -> [HTTP.Header]
buildTrailersOnly proxy TrailersOnly{
                            trailersOnlyGrpcStatus
                          , trailersOnlyGrpcMessage
                          , trailersOnlyMetadata
                          , trailersOnlyContentType
                          } = concat [
      [ buildContentType proxy x
      | Just x <- [trailersOnlyContentType]
      ]
    , [ ( "grpc-status"
        , BS.Strict.C8.pack $ show $ fromGrpcStatus trailersOnlyGrpcStatus
        )
      ]
    , [ ( "grpc-message"
        , PercentEncoding.encode x
        )
      | Just x <- [trailersOnlyGrpcMessage]
      ]
    , [ buildCustomMetadata x
      | x <- Map.toList trailersOnlyMetadata
      ]
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
    runPartialParser uninitTrailersOnly . mapM_ parseHeader
  where
    parseHeader :: HTTP.Header -> PartialParser TrailersOnly m ()
    parseHeader hdr@(name, value)
      | name == "grpc-status"
      = case toGrpcStatus =<< readMaybe (BS.Strict.C8.unpack value) of
          Nothing -> throwError $ "Invalid status: " ++ show value
          Just v  -> update updGrpcStatus $ \_ -> return v

      | name == "grpc-message"
      = case PercentEncoding.decode value of
          Left  err -> throwError $ show err
          Right msg -> update updGrpcMessage $ \_ -> return (Just msg)

      | name == "content-type"
      = update updContentType $ \_ ->
          Just <$> parseContentType proxy hdr

      | otherwise
      = parseCustomMetadata hdr >>=
        update updMetadata . fmap . uncurry Map.insert

    uninitTrailersOnly :: Partial m TrailersOnly
    uninitTrailersOnly =
           throwError "missing: grpc-status" -- trailersOnlyGrpcStatus
        :* return Nothing                    -- trailersOnlyGrpcMessage
        :* return Map.empty                  -- trailersOnlyMetadata
        :* return Nothing                    -- trailersOnlyContentType
        :* Nil

    (   updGrpcStatus
     :* updGrpcMessage
     :* updMetadata
     :* updContentType
     :* Nil ) = partialUpdates (Proxy @TrailersOnly)
