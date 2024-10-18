-- | Percent encoding
--
-- The gRPC spec is similar, but not identical, to URI encoding.
--
-- We work with strict bytestrings here, since these are ultimately intended
-- as header values.
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
module Network.GRPC.Spec.PercentEncoding (
    encode
  , decode
  ) where

import Control.Exception
import Data.Bifunctor
import Data.Bits
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Word

import Network.GRPC.Util.ByteString (ascii)

{-------------------------------------------------------------------------------
  Encoding
-------------------------------------------------------------------------------}

encode :: Text -> Strict.ByteString
encode =
      BS.Lazy.toStrict
    . Builder.toLazyByteString
    . foldMap encodeChar
    . BS.Strict.unpack
    . Text.encodeUtf8

encodeChar :: Word8 -> Builder
encodeChar c
  | needsEncoding c = let (hi, lo) = toBase16 c
                      in mconcat [
                             Builder.char7 '%'
                           , Builder.word8 hi
                           , Builder.word8 lo
                           ]
  | otherwise       = Builder.word8 c

-- | Does this character have to be encoded?
--
-- The gRPC spec unencoded characters as "space and VCHAR, except %":
--
-- > Percent-Byte-Unencoded → 1*( %x20-%x24 / %x26-%x7E )
needsEncoding :: Word8 -> Bool
needsEncoding c
  | 0x20 <= c && c <= 0x24 = False
  | 0x26 <= c && c <= 0x7E = False
  | otherwise              = True

{-------------------------------------------------------------------------------
  Decoding
-------------------------------------------------------------------------------}

data DecodeException =
    -- | Hex digit outside its range @(0..9, A..F)@
    InvalidHexDigit Word8

    -- | Percent (@%@) which was not followed by two hex digits
  | MissingHexDigits

    -- | Hex-decoding was fine, but encoded string was not valid UTF8
  | InvalidUtf8 Text.UnicodeException
  deriving stock (Show)

instance Exception DecodeException where
  displayException (InvalidHexDigit w) =
      "invalid hex digit '" ++ show w ++ "'"
  displayException MissingHexDigits =
      "'%' not followed by two hex digits"
  displayException (InvalidUtf8 err) =
      displayException err

decode :: Strict.ByteString -> Either DecodeException Text
decode =
      (>>= first InvalidUtf8 . Text.decodeUtf8')
    . fmap BS.Strict.pack
    . go []
    . BS.Strict.unpack
  where
    go :: [Word8] -> [Word8] -> Either DecodeException [Word8]
    go acc (c:cs)
      | c == ascii '%' = case cs of
                           hi:lo:cs' -> do
                             c' <- fromBase16 (hi, lo)
                             go (c':acc) cs'
                           _otherwise ->
                             Left $ MissingHexDigits
      | otherwise      = go (c:acc) cs
    go acc []          = Right (reverse acc)

{-------------------------------------------------------------------------------
  Utilities for working with base16

  We could depend on @base16@ or @base16-bytestring@ here, but they deal with
  entire strings at a time, which doesn't quite fit our needs here and would
  result in quite a bit of overhead.
-------------------------------------------------------------------------------}

toBase16 :: Word8 -> (Word8, Word8)
toBase16 c = (toHexDigit hi, toHexDigit lo)
  where
    hi, lo :: Word8
    hi = c `shiftR` 4
    lo = c .&. 0x0F;

fromBase16 :: (Word8, Word8) -> Either DecodeException Word8
fromBase16 = \(hi, lo) -> aux <$> fromHexDigit hi <*> fromHexDigit lo
  where
    aux :: Word8 -> Word8 -> Word8
    aux hi lo = (hi `shiftL` 4) .|. lo

toHexDigit :: Word8 -> Word8
toHexDigit c
  |  0 <= c && c <=  9 = ascii '0' + c
  | 10 <= c && c <= 15 = ascii 'A' + (c - 10)
  | otherwise          = error "toHexDigit: out of range"

-- | Value of a single hex digit
--
-- The gRPC spec does not actually allow for lowercase here, but we support it
-- in case we're dealing with non-conformant peers.
fromHexDigit :: Word8 -> Either DecodeException Word8
fromHexDigit c
  | ascii '0' <= c && c <= ascii '9' = Right $      c - ascii '0'
  | ascii 'A' <= c && c <= ascii 'F' = Right $ 10 + c - ascii 'A'
  | ascii 'a' <= c && c <= ascii 'f' = Right $ 10 + c - ascii 'a'
  | otherwise                        = Left $ InvalidHexDigit c
