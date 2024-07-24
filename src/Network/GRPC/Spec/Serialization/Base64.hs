-- | gRPC-style base64-encoding
--
-- The gRPC specification mandates standard Base64-encoding for binary headers
-- <https://datatracker.ietf.org/doc/html/rfc4648#section-4>, /but/ without
-- padding.
module Network.GRPC.Spec.Serialization.Base64 (
    encodeBase64
  , decodeBase64
  ) where

import Control.Monad
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Base64 qualified as BS.Strict.B64
import Data.ByteString.Char8 qualified as BS.Strict.Char8

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

encodeBase64 :: Strict.ByteString -> Strict.ByteString
encodeBase64 = removePadding . BS.Strict.B64.encode

decodeBase64 :: Strict.ByteString -> Either String Strict.ByteString
decodeBase64 = BS.Strict.B64.decode <=< addPadding

{-------------------------------------------------------------------------------
  Internal: Adding and removing padding

  In Base64 encoding, every group of three bytes in the input is represented as
  4 bytes in the output. We therefore have three possibilities:

  * The input has size @3n@. No padding bytes are added.
  * The input has size @3n + 1@. Two padding byte are added.
  * The input has size @3n + 2@. One padding byte is added.

  Standard base64 encoding (including padding) therefore /always/ has size @4m@.
-------------------------------------------------------------------------------}

removePadding :: Strict.ByteString -> Strict.ByteString
removePadding bs
  -- Empty bytestring
  --
  -- If the bytestring is not null, it must have at least 4 bytes, justifying
  -- the calls to @index@ below.
  | BS.Strict.null bs
  = bs

  -- Two padding bytes
  | BS.Strict.Char8.index bs (len - 2) == '='
  = BS.Strict.take (len - 2) bs

  -- One padding byte
  | BS.Strict.Char8.index bs (len - 1) == '='
  = BS.Strict.take (len - 1) bs

  | otherwise
  = bs
  where
    len :: Int
    len = BS.Strict.length bs

addPadding :: Strict.ByteString -> Either String Strict.ByteString
addPadding bs
  -- Three padding bytes (i.e., invalid strict)
  | len `mod` 4 == 1
  = Left $ "Invalid length of unpadded base64-encoded string " ++ show len

  -- Two padding bytes
  | len `mod` 4 == 2
  = Right $ bs <> BS.Strict.Char8.pack "=="

  -- One padding bytes
  | len `mod` 4 == 3
  = Right $ bs <> BS.Strict.Char8.pack "="

  -- No padding (this includes the empty string)
  | otherwise
  = Right bs
  where
    len :: Int
    len = BS.Strict.length bs