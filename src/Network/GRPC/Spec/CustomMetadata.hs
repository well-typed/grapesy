{-# LANGUAGE OverloadedStrings #-}

-- | Custom metadata
--
-- These are application-defined headers/trailers.
--
-- Intended for unqualified import.
module Network.GRPC.Spec.CustomMetadata (
    -- * Definition
    CustomMetadata(..)
  , customHeaderName
    -- * Header-Name
  , HeaderName(HeaderName)
  , getHeaderName
  , safeHeaderName
    -- * ASCII value
  , AsciiValue(AsciiValue)
  , getAsciiValue
  , safeAsciiValue
    -- * Binary value
  , BinaryValue(..)
  , buildBinaryValue
  , parseBinaryValue
    -- * To and from HTTP headers
  , buildCustomMetadata
  , parseCustomMetadata
    -- * Convenience
  , lookupCustomMetadata
  ) where

import Control.Monad.Except
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.CaseInsensitive qualified as CI
import Data.String
import Data.Word
import GHC.Generics qualified as GHC
import GHC.Show
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec.Base64
import Network.GRPC.Util.ByteString (strip, ascii, dropEnd)

{-------------------------------------------------------------------------------
  Definition

  > Custom-Metadata → Binary-Header / ASCII-Header
  > Binary-Header   → {Header-Name "-bin" } {base64 encoded value}
  > ASCII-Header    → Header-Name ASCII-Value
-------------------------------------------------------------------------------}

-- | Custom metadata
--
-- This is an arbitrary set of key-value pairs defined by the application layer.
--
-- Custom metadata order is not guaranteed to be preserved except for values
-- with duplicate header names. Duplicate header names may have their values
-- joined with "," as the delimiter and be considered semantically equivalent.
data CustomMetadata =
    -- | Binary header
    --
    -- Binary headers will be base-64 encoded.
    --
    -- The header name will be given a @-bin@ suffix (runtime libraries use this
    -- suffix to detect binary headers and properly apply base64 encoding &
    -- decoding as headers are sent and received).
    --
    -- Since this is binary data, padding considerations do not apply.
    BinaryHeader HeaderName BinaryValue

    -- | ASCII header
    --
    -- HTTP2 does not allow arbitrary octet sequences for header values; use
    -- 'BinaryHeader' for Base64 encoding. See 'isValidAsciiValue' for what
    -- constitutes a valid value.
    --
    -- Any padding will be removed before sending. The gRPC spec is not precise
    -- about what exactly constitutes \"padding\", but the ABNF spec defines it
    -- as "space and horizontal tab"
    -- <https://www.rfc-editor.org/rfc/rfc5234#section-3.1>.
  | AsciiHeader HeaderName AsciiValue
  deriving stock (Show, Eq, Ord, GHC.Generic)

customHeaderName :: CustomMetadata -> HeaderName
customHeaderName (BinaryHeader n _) = n
customHeaderName (AsciiHeader  n _) = n

{-------------------------------------------------------------------------------
  Header-Name

  > Header-Name → 1*( %x30-39 / %x61-7A / "_" / "-" / ".") ; 0-9 a-z _ - .
-------------------------------------------------------------------------------}

-- | Header name
--
-- Header names cannot be empty, and must consist of digits (@0-9@), lowercase
-- letters (@a-z@), underscore (@_@), hyphen (@-@), or period (@.@).
--
-- Header names should not start with @grpc-@ (these are reserved for future
-- GRPC use).
newtype HeaderName = UnsafeHeaderName {
      getHeaderName :: Strict.ByteString
    }
  deriving stock (Eq, Ord)
  deriving newtype (IsString)

-- | 'Show' instance relies on the 'HeaderName' pattern synonym
instance Show HeaderName where
  showsPrec p (UnsafeHeaderName name) = showParen (p >= appPrec1) $
        showString "HeaderName "
      . showsPrec appPrec1 name

pattern HeaderName :: Strict.ByteString -> HeaderName
pattern HeaderName n <- UnsafeHeaderName n
  where
    HeaderName n
      | isValidHeaderName n = UnsafeHeaderName n
      | otherwise = error $ "invalid HeaderName: " ++ show n

{-# COMPLETE HeaderName #-}

safeHeaderName :: Strict.ByteString -> Maybe HeaderName
safeHeaderName bs
  | isValidHeaderName bs = Just $ UnsafeHeaderName bs
  | otherwise            = Nothing

-- | Check for header name validity
isValidHeaderName :: Strict.ByteString -> Bool
isValidHeaderName bs = and [
      BS.Strict.length bs >= 1
    , BS.Strict.all isValidChar bs
    , not $ "grpc-" `BS.Strict.isPrefixOf` bs

      -- @grapesy@ adds and removes the @-bin@ suffix automatically
    , not $ "-bin" `BS.Strict.isSuffixOf` bs
    ]
  where
    isValidChar :: Word8 -> Bool
    isValidChar c = or [
          0x30 <= c && c <= 0x39
        , 0x61 <= c && c <= 0x7A
        , c == ascii '_'
        , c == ascii '-'
        , c == ascii '.'
        ]

{-------------------------------------------------------------------------------
  ASCII-Value

  > ASCII-Value → 1*( %x20-%x7E ) ; space and printable ASCII
-------------------------------------------------------------------------------}

-- | Value of ASCII header
--
-- ASCII headers cannot be empty, and can only use characters in the range
-- @0x20 .. 0x7E@. Note that although this range includes whitespace, any
-- padding will be removed when constructing the value.
newtype AsciiValue = UnsafeAsciiValue {
      getAsciiValue :: Strict.ByteString
    }
  deriving stock (Eq, Ord)

-- | 'Show' instance relies on the 'AsciiValue' pattern synonym
instance Show AsciiValue where
  showsPrec p (UnsafeAsciiValue value) = showParen (p >= appPrec1) $
        showString "AsciiValue "
      . showsPrec appPrec1 value

instance IsString AsciiValue where
  fromString = AsciiValue . fromString

pattern AsciiValue :: Strict.ByteString -> AsciiValue
pattern AsciiValue v <- UnsafeAsciiValue v
  where
    AsciiValue v
      | isValidAsciiValue v = UnsafeAsciiValue (strip v)
      | otherwise = error $ "invalid AsciiValue: " ++ show v

{-# COMPLETE AsciiValue #-}

safeAsciiValue :: Strict.ByteString -> Maybe AsciiValue
safeAsciiValue bs
  | isValidAsciiValue bs = Just $ UnsafeAsciiValue bs
  | otherwise            = Nothing

-- | Check for valid ASCII header value
--
-- NOTE: By rights this should also verify that the header is non-empty.
-- However, empty header values do occasionally show up, and so we permit them.
-- The main reason for checking for validity at all is to ensure that we don't
-- confuse binary headers and ASCII headers.
isValidAsciiValue :: Strict.ByteString -> Bool
isValidAsciiValue bs = BS.Strict.all (\c -> 0x20 <= c && c <= 0x7E) bs

{-------------------------------------------------------------------------------
  Binary value
-------------------------------------------------------------------------------}

newtype BinaryValue = BinaryValue {
      getBinaryValue :: Strict.ByteString
    }
  deriving stock (Show, Eq, Ord)

buildBinaryValue :: BinaryValue -> Strict.ByteString
buildBinaryValue = encodeBase64 . getBinaryValue

parseBinaryValue :: MonadError String m => Strict.ByteString -> m BinaryValue
parseBinaryValue bs =
    case BinaryValue <$> decodeBase64 bs of
      Left  err -> throwError err
      Right val -> return val

{-------------------------------------------------------------------------------
  To/from HTTP2
-------------------------------------------------------------------------------}

buildCustomMetadata :: CustomMetadata -> HTTP.Header
buildCustomMetadata (BinaryHeader name value) = (
      CI.mk $ getHeaderName name <> "-bin"
    , buildBinaryValue value
    )
buildCustomMetadata (AsciiHeader name value) = (
      CI.mk $ getHeaderName name
    , getAsciiValue value
    )

parseCustomMetadata :: MonadError String m => HTTP.Header -> m CustomMetadata
parseCustomMetadata (name, value)
  | "grpc-" `BS.Strict.isPrefixOf` CI.foldedCase name
  = throwError $ "Reserved header: " ++ show (name, value)

  | "-bin" `BS.Strict.isSuffixOf` CI.foldedCase name
  = case ( safeHeaderName (dropEnd 4 $ CI.foldedCase name)
         , parseBinaryValue value
         ) of
      (Nothing, _) ->
        throwError $ "Invalid header name: " ++ show (name, value)
      (_, Left err) ->
        throwError $ "Cannot decode binary header: " ++ err
      (Just name', Right value') ->
        return $ BinaryHeader name' value'

  | otherwise
  = case ( safeHeaderName (CI.foldedCase name)
         , safeAsciiValue value
         ) of
      (Nothing, _) ->
        throwError $ "Invalid header name: " ++ show name
      (_, Nothing) ->
        throwError $ "Invalid ASCII header value: " ++ show value
      (Just name', Just value') ->
        return $ AsciiHeader name' value'

{-------------------------------------------------------------------------------
  Convenience functions
-------------------------------------------------------------------------------}

lookupCustomMetadata ::
     HeaderName
  -> [CustomMetadata]
  -> Maybe (Either BinaryValue AsciiValue)
lookupCustomMetadata name = go
  where
    go :: [CustomMetadata] -> Maybe (Either BinaryValue AsciiValue)
    go []     = Nothing
    go (h:hs) =
        case h of
          BinaryHeader name' value | name == name' ->
            Just $ Left value
          AsciiHeader name' value | name == name' ->
            Just $ Right value
          _otherwise ->
            go hs