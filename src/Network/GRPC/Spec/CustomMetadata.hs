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
    -- * To and from HTTP headers
  , buildCustomMetadata
  , parseCustomMetadata
  ) where

import Control.Monad.Except
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Base64 qualified as BS.Strict.B64
import Data.CaseInsensitive qualified as CI
import Data.String
import Data.Word
import GHC.Generics qualified as GHC
import GHC.Show
import Network.HTTP.Types qualified as HTTP
import Text.Show.Pretty

import Network.GRPC.Util.ByteString (strip, ascii, dropEnd)
import Network.GRPC.Util.PrettyVal

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
    -- Since these considered binary data, padding considerations do not apply.
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
  deriving anyclass (PrettyVal)

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
  deriving (PrettyVal) via StrictByteString_IsString HeaderName

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
  deriving PrettyVal via StrictByteString_IsString AsciiValue

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
isValidAsciiValue :: Strict.ByteString -> Bool
isValidAsciiValue bs = and [
      BS.Strict.length bs >= 1
    , BS.Strict.all (\c -> 0x20 <= c && c <= 0x7E) bs
    ]

{-------------------------------------------------------------------------------
  Binary value
-------------------------------------------------------------------------------}

newtype BinaryValue = BinaryValue {
      getBinaryValue :: Strict.ByteString
    }
  deriving stock (Show, Eq, Ord)
  deriving PrettyVal via StrictByteString_Binary "BinaryValue" BinaryValue

{-------------------------------------------------------------------------------
  To/from HTTP2
-------------------------------------------------------------------------------}

buildCustomMetadata :: CustomMetadata -> HTTP.Header
buildCustomMetadata (BinaryHeader name value) = (
      CI.mk $ getHeaderName name <> "-bin"
    , BS.Strict.B64.encode (getBinaryValue value)
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
         , BS.Strict.B64.decode value
         ) of
      (Nothing, _) ->
        throwError $ "Invalid header name: " ++ show (name, value)
      (_, Left err) ->
        throwError $ "Cannot decode binary header: " ++ err
      (Just name', Right value') ->
        return $ BinaryHeader name' (BinaryValue value')

  | otherwise
  = case safeHeaderName (CI.foldedCase name) of
      Nothing -> throwError $ "Invalid header name: " ++ show name
      -- We assume that by the time 'http2' gives us a header value, it is a
      -- valid http2 header value.
      Just name' -> return $ AsciiHeader name' (UnsafeAsciiValue value)
