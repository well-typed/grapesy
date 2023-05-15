{-# LANGUAGE OverloadedStrings #-}

-- | Deal with HTTP2 responses
--
-- Intended for unqualified import.
--
-- > import Network.GRPC.Spec.HTTP2.Response qualified as Response
module Network.GRPC.Spec.HTTP2.Response (
    -- * Construction
    buildTrailers
    -- * Parsing
  , parseHeaders
  , parseTrailers
  ) where

import Control.Monad.Except
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.CaseInsensitive qualified as CI
import Data.List (intersperse)
import Data.SOP
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)

import Network.GRPC.Spec
import Network.GRPC.Spec.Compression qualified as Compression
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.ByteString
import Network.GRPC.Util.Partial

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

buildTrailers :: Trailers -> [HTTP.Header]
buildTrailers Trailers{ trailerGrpcStatus
                      , trailerGrpcMessage = _TODO1
                      , trailerCustom = _TODO2 } = concat [
      [ ( "grpc-status"
        , BS.Strict.C8.pack $ show $ fromGrpcStatus trailerGrpcStatus
        )
      ]
    ]

{-------------------------------------------------------------------------------
  Parsing

  TODO: We should attempt to be more lenient in our parsing here, and throw
  fewer errors. Perhaps have an @Invalid@ constructor or something, so that
  we can mark incoming headers that were not valid, but still give them to the
  user, but then throw an error if we try to /send/ those.

  TODO: Related to the above, the spec says: "Implementations MUST accept padded
  and un-padded values and should emit un-padded values." We don't currently do
  this for incoming headers.
-------------------------------------------------------------------------------}

-- | Parse response headers
--
-- > Response-Headers →
-- >   HTTP-Status
-- >   [Message-Encoding]
-- >   [Message-Accept-Encoding]
-- >   Content-Type
-- >   *Custom-Metadata
--
-- We do not deal with @HTTP-Status@ here; @http2@ reports this separately.
parseHeaders :: IsRPC rpc => rpc -> [HTTP.Header] -> Either String Headers
parseHeaders rpc =
      runPartialParser uninitResponseHeaders
    . mapM_ parseHeader
  where
    -- HTTP2 header names are always lowercase, and must be ASCII.
    -- <https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.2>
    parseHeader :: HTTP.Header -> PartialParser Headers (Either String) ()
    parseHeader (name, value)
      | name == "content-type"
      = parseContentType rpc value

      | name == "grpc-encoding"
      = update updCompression $ \_ -> Right . Just $
          Compression.deserializeId value

      | name == "grpc-accept-encoding"
      = update updAcceptCompression $ \_ -> Right . Just $
          map (Compression.deserializeId . strip) $
            BS.Strict.splitWith (== ascii ',') value

      | otherwise
      = parseCustomMetadata name value >>= update updCustom . fmap . (:)

    uninitResponseHeaders :: Partial (Either String) Headers
    uninitResponseHeaders =
           return Nothing  -- headerCompression
        :* return Nothing  -- headerAcceptCompression
        :* return []       -- headerCustom
        :* Nil


    (    updCompression
      :* updAcceptCompression
      :* updCustom
      :* Nil ) = partialUpdates (Proxy @Headers)

-- | Parse response trailers
--
-- This can be used for either @Trailers@ or @Trailers-Only@:
--
-- > Trailers → Status [Status-Message] *Custom-Metadata
-- > Trailers-Only → HTTP-Status Content-Type Trailers
--
-- We can use one function for both, because
--
-- 1. We do not deal with @HTTP-Status@ at all in this module (the HTTP status
--    is reported separately by @http2@)
-- 2. We do not report a value for @Content-Type@, but merely check that its
--    value matches what we expect, if it's set.
--
-- If in future updates to the gRPC spec @Trailers-Only@ starts to diverge more
-- from @Trailers@, we will need to split this into two functions.
parseTrailers ::
     IsRPC rpc
  => rpc
  -> [HTTP.Header] -> Either String Trailers
parseTrailers rpc =
    runPartialParser uninitTrailers . mapM_ parseHeader
  where
    parseHeader :: HTTP.Header -> PartialParser Trailers (Either String) ()
    parseHeader (name, value)
      | name == "grpc-status"
      = case toGrpcStatus =<< readMaybe (BS.Strict.C8.unpack value) of
          Nothing -> throwError $ "Invalid status: " ++ show value
          Just v  -> update updGrpcStatus $ \_ -> return v

      | name == "grpc-message"
      = case PercentEncoding.decode value of
          Left  err -> throwError $ show err
          Right msg -> update updGrpcMessage $ \_ -> return (Just msg)

      | name == "content-type" -- only relevant in the Trailers-Only case
      = parseContentType rpc value

      | otherwise
      = parseCustomMetadata name value >>= update updCustom . fmap . (:)

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

parseContentType ::
     IsRPC rpc
  => rpc
  -> Strict.ByteString
  -> PartialParser r (Either String) ()
parseContentType rpc value =
    unless (value `elem` accepted) $
      throwError $ concat [
          "Unexpected content-type "
        , BS.Strict.C8.unpack value
        , "; expected one of "
        , mconcat . intersperse ", " . map BS.Strict.C8.unpack $ accepted
        , "'"
        ]
  where
    accepted :: [Strict.ByteString]
    accepted = [
        "application/grpc"
      , "application/grpc+" <> serializationFormat rpc
      ]

parseCustomMetadata ::
     HTTP.HeaderName
  -> Strict.ByteString
  -> PartialParser r (Either String) CustomMetadata
parseCustomMetadata name value
  | "grpc-" `BS.Strict.isPrefixOf` CI.foldedCase name
  = throwError $ "Reserved header: " ++ show (name, value)

  | "-bin" `BS.Strict.isSuffixOf` CI.foldedCase name
  = case safeHeaderName (BS.Strict.dropEnd 4 $ CI.foldedCase name) of
      Just name' ->
        return $ BinaryHeader name' value
      _otherwise ->
        throwError $ "Invalid custom binary header: " ++ show (name, value)

  | otherwise
  = case ( safeHeaderName (CI.foldedCase name)
         , safeAsciiValue value
         ) of
      (Just name', Just value') ->
        return $ AsciiHeader name' value'
      _otherwise ->
        throwError $ "Invalid custom ASCII header: " ++ show (name, value)

