{-# LANGUAGE OverloadedStrings #-}

-- | Deal with HTTP2 responses
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Response qualified as Resp
module Network.GRPC.Spec.Response (
    -- * Construction
    buildHeaders
  , buildProperTrailers
  , buildTrailersOnly
    -- * Parsing
  , parseHeaders
  , parseProperTrailers
  , parseTrailersOnly
  ) where

import Control.Monad.Except
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.List.NonEmpty (NonEmpty)
import Data.SOP
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)

import Network.GRPC.Spec
import Network.GRPC.Spec.Common
import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Partial

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
buildHeaders :: IsRPC rpc => Proxy rpc -> ResponseHeaders -> [HTTP.Header]
buildHeaders proxy
             ResponseHeaders{ responseCompression
                            , responseAcceptCompression
                            , responseMetadata
                            } = concat [
      [ buildContentType proxy ]
    , [ buildMessageEncoding x
      | Just x <- [responseCompression]
      ]
    , [ buildMessageAcceptEncoding x
      | Just x <- [responseAcceptCompression]
      ]
    , [ buildCustomMetadata x
      | x <- responseMetadata
      ]
    ]

-- | Parse response headers
parseHeaders ::
     IsRPC rpc
  => Proxy rpc -> [HTTP.Header] -> Either String ResponseHeaders
parseHeaders proxy =
      runPartialParser uninitResponseHeaders
    . mapM_ parseHeader
  where
    -- HTTP2 header names are always lowercase, and must be ASCII.
    -- <https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.2>
    parseHeader ::
         HTTP.Header
      -> PartialParser ResponseHeaders (Either String) ()
    parseHeader hdr@(name, _value)
      | name == "content-type"
      = parseContentType proxy hdr

      | name == "grpc-encoding"
      = update updCompression $ \_ ->
          Just <$> parseMessageEncoding hdr

      | name == "grpc-accept-encoding"
      = update updAcceptCompression $ \_ ->
          Just <$> parseMessageAcceptEncoding hdr

      | otherwise
      = parseCustomMetadata hdr >>= update updCustom . fmap . (:)

    uninitResponseHeaders :: Partial (Either String) ResponseHeaders
    uninitResponseHeaders =
           return (Nothing :: Maybe CompressionId)
        :* return (Nothing :: Maybe (NonEmpty CompressionId))
        :* return ([]      :: [CustomMetadata])
        :* Nil

    (    updCompression
      :* updAcceptCompression
      :* updCustom
      :* Nil ) = partialUpdates (Proxy @ResponseHeaders)

{-------------------------------------------------------------------------------
  > Trailers → Status [Status-Message] *Custom-Metadata
-------------------------------------------------------------------------------}

-- | Build trailers (see 'buildTrailersOnly' for the Trailers-Only case)
buildProperTrailers :: ProperTrailers -> [HTTP.Header]
buildProperTrailers ProperTrailers{
                        trailerGrpcStatus
                      , trailerGrpcMessage
                      , trailerMetadata
                      } = concat [
      [ ( "grpc-status"
        , BS.Strict.C8.pack $ show $ fromGrpcStatus trailerGrpcStatus
        )
      ]
    , [ ( "grpc-message"
        , PercentEncoding.encode x
        )
      | Just x <- [trailerGrpcMessage]
      ]
    , [ buildCustomMetadata x
      | x <- trailerMetadata
      ]
    ]

-- | Build trailers for the Trailers-Only case
buildTrailersOnly :: TrailersOnly -> [HTTP.Header]
buildTrailersOnly (TrailersOnly trailers) =
      ("content-type", "application/grpc")
    : buildProperTrailers trailers

-- | Parse response trailers
--
-- This also allows for the @Content-Type@ header; see 'parseTrailersOnly'
-- for discussion.
parseProperTrailers ::
     IsRPC rpc
  => Proxy rpc -> [HTTP.Header] -> Either String ProperTrailers
parseProperTrailers proxy =
    runPartialParser uninitTrailers . mapM_ parseHeader
  where
    parseHeader ::
         HTTP.Header
      -> PartialParser ProperTrailers (Either String) ()
    parseHeader hdr@(name, value)
      | name == "grpc-status"
      = case toGrpcStatus =<< readMaybe (BS.Strict.C8.unpack value) of
          Nothing -> throwError $ "Invalid status: " ++ show value
          Just v  -> update updGrpcStatus $ \_ -> return v

      | name == "grpc-message"
      = case PercentEncoding.decode value of
          Left  err -> throwError $ show err
          Right msg -> update updGrpcMessage $ \_ -> return (Just msg)

        -- For the Trailers-Only case
      | name == "content-type"
      = parseContentType proxy hdr

      | otherwise
      = parseCustomMetadata hdr >>= update updCustom . fmap . (:)

    uninitTrailers :: Partial (Either String) ProperTrailers
    uninitTrailers =
           throwError "missing: grpc-status"  -- trailerGrpcStatus
        :* return Nothing                     -- trailerGrpcMessage
        :* return []                          -- trailerCustom
        :* Nil

    (     updGrpcStatus
      :* updGrpcMessage
      :* updCustom
      :* Nil ) = partialUpdates (Proxy @ProperTrailers)

-- | Parse trailers in the gRPC @Trailers-Only@ case
--
-- The gRPC spec defines:
--
-- > Trailers      → Status [Status-Message] *Custom-Metadata
-- > Trailers-Only → HTTP-Status Content-Type Trailers
--
-- This means that the only difference between the two is the precise of the
-- @Content-Type@ header (@HTTP-Status@ is dealt with elsewhere). Since we don't
-- actually extract any information from the @Content-Type@ header, we can just
-- use one parser for both @Trailers@ and @Trailers-Only@.
parseTrailersOnly ::
     IsRPC rpc
  => Proxy rpc -> [HTTP.Header] -> Either String TrailersOnly
parseTrailersOnly proxy = fmap TrailersOnly . parseProperTrailers proxy