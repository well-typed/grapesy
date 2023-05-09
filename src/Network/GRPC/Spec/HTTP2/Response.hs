{-# LANGUAGE OverloadedStrings #-}

-- | Deal with HTTP2 responses
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.HTTP2.Response (ResponseHeaders, ResponseTrailers)
-- > import Network.GRPC.Spec.HTTP2.Response qualified as Response
module Network.GRPC.Spec.HTTP2.Response (
    -- * Definition
    ResponseHeaders(..)
  , ResponseTrailers(..)
  , toTrailers
    -- * Parsing
  , parseHeaders
  , parseTrailers
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.CaseInsensitive qualified as CI
import Data.Kind
import Data.List (intersperse)
import Data.SOP
import Data.Text (Text)
import Generics.SOP qualified as SOP
import GHC.Generics qualified as GHC
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)

import Network.GRPC.Spec
import Network.GRPC.Spec.Compression (CompressionId)
import Network.GRPC.Spec.Compression qualified as Compression
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.ByteString
import Network.GRPC.Util.HKD (IsHKD)
import Network.GRPC.Util.HKD qualified as HKD
import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Response headers
--
-- When we add this, should also update the parser.
data ResponseHeaders (f :: Type -> Type) = ResponseHeaders {
      headerCompression       :: f (Maybe CompressionId)
    , headerAcceptCompression :: f (Maybe [CompressionId])
    , headerCustomMetadata    :: f [CustomMetadata]
    }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Response trailers
--
-- Response trailers are a
-- [HTTP2 concept](https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.3):
-- they are HTTP headers that are sent /after/ the content body. For example,
-- imagine the server is streaming a file that it's reading from disk; it could
-- use trailers to give the client an MD5 checksum when streaming is complete.
data ResponseTrailers (f :: Type -> Type) = ResponseTrailers {
      trailerStatus         :: f GrpcStatus
    , trailerMessage        :: f (Maybe Text)
    , trailerCustomMetadata :: f [CustomMetadata]
    }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

deriving instance (forall a. Show a => Show (f a)) => Show (ResponseHeaders  f)
deriving instance (forall a. Show a => Show (f a)) => Show (ResponseTrailers f)

uninitResponseHeaders :: ResponseHeaders (Either String)
uninitResponseHeaders = ResponseHeaders {
      headerCompression       = Right Nothing
    , headerAcceptCompression = Right Nothing
    , headerCustomMetadata    = Right []
    }

uninitResponseTrailers :: ResponseTrailers (Either String)
uninitResponseTrailers = ResponseTrailers {
      trailerStatus         = Left "Missing grpc-status"
    , trailerMessage        = Right Nothing
    , trailerCustomMetadata = Right []
    }

toTrailers :: ResponseTrailers I -> Trailers
toTrailers trailers = Trailers {
      grpcStatus  = unI $ trailerStatus  trailers
    , grpcMessage = unI $ trailerMessage trailers
    }

{-------------------------------------------------------------------------------
  Response headers

  TODO: We should attempt to be more lenient in our parsing here, and throw
  fewer errors. Perhaps have an @Invalid@ constructor or something, so that
  we can mark incoming headers that were not valid, but still give them to the
  user, but then throw an error if we try to /send/ those.

  TODO: Related to the above, the spec says: "Implementations MUST accept padded
  and un-padded values and should emit un-padded values." We don't currently do
  this for incoming headers.
-------------------------------------------------------------------------------}

newtype HeaderParser s a = WrapHeaderParser {
      unwrapHeaderParser :: StateT (s (Either String)) (Except String) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadError String
    , MonadState (s (Either String))
    )

runHeaderParser ::
     IsHKD s
  => s (Either String)
  -> HeaderParser s () -> Either String (s I)
runHeaderParser uninit =
      (>>= HKD.hsequence)
    . runExcept
    . flip execStateT uninit
    . unwrapHeaderParser

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
parseHeaders ::
     IsRPC rpc
  => rpc
  -> [HTTP.Header]
  -> Either String (ResponseHeaders I)
parseHeaders rpc =
      runHeaderParser uninitResponseHeaders
    . mapM_ parseHeader
  where
    -- HTTP2 header names are always lowercase, and must be ASCII.
    -- <https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.2>
    parseHeader :: HTTP.Header -> HeaderParser ResponseHeaders ()
    parseHeader (name, value)
      | name == "content-type"
      = parseContentType rpc value

      | name == "grpc-encoding"
      = modify $ \partial -> partial{
            headerCompression = Right $ Just $ Compression.deserializeId value
          }

      | name == "grpc-accept-encoding"
      = modify $ \partial -> partial{
            headerAcceptCompression = Right . Just $
              map (Compression.deserializeId . strip) $
                BS.Strict.splitWith (== ascii ',') value
          }

      | otherwise
      = parseCustomMetadata name value >>= \md ->
        modify $ \partial -> partial{
              headerCustomMetadata = (md:) <$> headerCustomMetadata partial
            }

parseContentType :: IsRPC rpc => rpc -> Strict.ByteString -> HeaderParser a ()
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
  -> HeaderParser a CustomMetadata
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
  -> [HTTP.Header] -> Either String (ResponseTrailers I)
parseTrailers rpc =
      runHeaderParser uninitResponseTrailers
    . mapM_ parseHeader
  where
    parseHeader :: HTTP.Header -> HeaderParser ResponseTrailers ()
    parseHeader (name, value)
      | name == "grpc-status"
      = case toGrpcStatus =<< readMaybe (BS.Strict.C8.unpack value) of
          Nothing -> throwError $ "Invalid status: " ++ show value
          Just v  -> modify $ \partial -> partial{
                         trailerStatus = Right v
                       }

      | name == "grpc-message"
      = case PercentEncoding.decode value of
          Left  err -> throwError $ show err
          Right msg -> modify $ \partial -> partial{
                           trailerMessage = Right (Just msg)
                         }

      | name == "content-type" -- only relevant in the Trailers-Only case
      = parseContentType rpc value

      | otherwise
      = parseCustomMetadata name value >>= \md ->
        modify $ \partial -> partial{
              trailerCustomMetadata = (md:) <$> trailerCustomMetadata partial
            }

