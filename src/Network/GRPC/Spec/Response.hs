{-# LANGUAGE OverloadedStrings #-}

-- | Deal with HTTP2 responses
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Response qualified as Resp
module Network.GRPC.Spec.Response (
    -- * Construction
    buildHeaders
  , buildTrailers
    -- * Parsing
  , parseHeaders
  , parseTrailers
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
buildHeaders :: IsRPC rpc => rpc -> ResponseHeaders -> [HTTP.Header]
buildHeaders rpc
             ResponseHeaders{ responseCompression
                            , responseAcceptCompression
                            , responseMetadata
                            } = concat [
      [ buildContentType rpc ]
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
  => rpc -> [HTTP.Header] -> Either String ResponseHeaders
parseHeaders rpc =
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
      = parseContentType rpc hdr

      | name == "grpc-encoding"
      = update updCompression $ \_ ->
          Just <$> parseMessageEncoding hdr

      | name == "grpc-accept-encoding"
      = update updAcceptCompression $ \_ ->
          Just <$> parseMessageAcceptEncoding hdr

      -- Trailers-Only case

      | name == "grpc-status"  = return ()
      | name == "grpc-message" = return ()

      -- Custom metadata

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

-- | Build trailers
buildTrailers :: Trailers -> [HTTP.Header]
buildTrailers Trailers{ trailerGrpcStatus
                      , trailerGrpcMessage
                      , trailerMetadata } = concat [
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

-- | Parse response trailers
--
-- === The @Trailers-Only@ case
--
-- The gRPC spec defines:
--
-- > Response-Headers → HTTP-Status
-- >                    [Message-Encoding]
-- >                    [Message-Accept-Encoding]
-- >                    Content-Type
-- >                    *Custom-Metadata
-- > Trailers         → Status
-- >                    [Status-Message]
-- >                    *Custom-Metadata
-- > Trailers-Only    → HTTP-Status
-- >                    Content-Type
-- >                    Trailers
--
-- We treat this case in a somewhat coarse-grained manner (see also
-- 'parseTrailers' in "Network.GRPC.Util.Session.API"): in the case of
-- @Trailers-Only@, we parse the same set of headers twice, once using
-- 'parseHeaders' and once using 'parseTrailers'. Both parsers are designed to
-- ignore each other's headers. We can only do this because:
--
-- * The @Trailers-Only@ case does not carry any information that is not present
--   in the combination of @Response-Headers@ and @Trailers@.
-- * The only headers that are /required/ by @Response-Headers@ are also present
--   in @Trailers-Only@.
--
-- These two properties of the spec do not feel accidental; nonetheless, should
-- they no longer hold in future versions of the spec, we will need to do this
-- differently (starting by introducing a new types in
-- "Network.GRPC.Util.Session.API", corresponding to the @Trailers-Only@ case).
--
-- One consequence of this approach is that if there is custom metadata present,
-- we will record that metadata both as 'responseMetadata' and as
-- 'trailerMetadata'; this is perhaps not unreasonable. Client code can
-- distinguish between the two cases simply by keeping track how many outputs
-- have been sent.
parseTrailers :: [HTTP.Header] -> Either String Trailers
parseTrailers =
    runPartialParser uninitTrailers . mapM_ parseHeader
  where
    parseHeader :: HTTP.Header -> PartialParser Trailers (Either String) ()
    parseHeader hdr@(name, value)
      | name == "grpc-status"
      = case toGrpcStatus =<< readMaybe (BS.Strict.C8.unpack value) of
          Nothing -> throwError $ "Invalid status: " ++ show value
          Just v  -> update updGrpcStatus $ \_ -> return v

      | name == "grpc-message"
      = case PercentEncoding.decode value of
          Left  err -> throwError $ show err
          Right msg -> update updGrpcMessage $ \_ -> return (Just msg)

      -- Trailers-Only case

      | name == "content-type"         = return ()
      | name == "grpc-encoding"        = return ()
      | name == "grpc-accept-encoding" = return ()

      -- Custom metadata

      | otherwise
      = parseCustomMetadata hdr >>= update updCustom . fmap . (:)

    uninitTrailers :: Partial (Either String) Trailers
    uninitTrailers =
           throwError "missing: grpc-status"  -- trailerGrpcStatus
        :* return Nothing                     -- trailerGrpcMessage
        :* return []                          -- trailerCustom
        :* Nil

    (     updGrpcStatus
      :* updGrpcMessage
      :* updCustom
      :* Nil ) = partialUpdates (Proxy @Trailers)

