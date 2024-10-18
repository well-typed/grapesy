{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.Serialization.CustomMetadata (
    -- * HeaderName
    buildHeaderName
  , parseHeaderName
    -- * AsciiValue
  , buildAsciiValue
  , parseAsciiValue
    -- * BinaryValue
  , buildBinaryValue
  , parseBinaryValue
    -- * CustomMetadata
  , buildCustomMetadata
  , parseCustomMetadata
  ) where

import Control.Monad
import Control.Monad.Except (MonadError(throwError))
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.List (intersperse)
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec
import Network.GRPC.Spec.Serialization.Base64
import Network.GRPC.Util.ByteString (ascii)

{-------------------------------------------------------------------------------
  HeaderName
-------------------------------------------------------------------------------}

buildHeaderName :: HeaderName -> CI Strict.ByteString
buildHeaderName name =
    case name of
      BinaryHeader name' -> CI.mk name'
      AsciiHeader  name' -> CI.mk name'

parseHeaderName :: MonadError String m => CI Strict.ByteString -> m HeaderName
parseHeaderName name =
    case safeHeaderName (CI.foldedCase name) of
      Nothing    -> throwError $ "Invalid header name: " ++ show name
      Just name' -> return name'

{-------------------------------------------------------------------------------
  AsciiValue
-------------------------------------------------------------------------------}

buildAsciiValue :: Strict.ByteString -> Strict.ByteString
buildAsciiValue = id

parseAsciiValue ::
     MonadError String m
  => Strict.ByteString -> m Strict.ByteString
parseAsciiValue bs = do
    unless (isValidAsciiValue bs) $
      throwError $ "Invalid ASCII header: " ++ show bs
    return bs

{-------------------------------------------------------------------------------
  BinaryValue
-------------------------------------------------------------------------------}

buildBinaryValue :: Strict.ByteString -> Strict.ByteString
buildBinaryValue = encodeBase64

-- | Parse binary value
--
-- The presence of duplicate headers makes this a bit subtle. Let's consider an
-- example. Suppose we have two duplicate headers
--
-- > foo-bin: YWJj    -- encoding of "abc"
-- > foo-bin: ZGVm    -- encoding of "def"
--
-- The spec says
--
-- > Custom-Metadata header order is not guaranteed to be preserved except for
-- > values with duplicate header names. Duplicate header names may have their
-- > values joined with "," as the delimiter and be considered semantically
-- > equivalent.
--
-- In @grapesy@ we will do the decoding of both headers /prior/ to joining
-- duplicate headers, and so the value we will reconstruct for @foo-bin@ is
-- \"abc,def\".
--
-- However, suppose we deal with a (non-compliant) peer which is unaware of
-- binary headers and has applied the joining rule /without/ decoding:
--
-- > foo-bin: YWJj,ZGVm
--
-- The spec is a bit vague about this case, saying only:
--
-- > Implementations must split Binary-Headers on "," before decoding the
-- > Base64-encoded values.
--
-- Here we assume that this case must be treated the same way as if the headers
-- /had/ been decoded prior to joining. Therefore, we split the input on commas,
-- decode each result separately, and join the results with commas again.
parseBinaryValue :: forall m.
     MonadError String m
  => Strict.ByteString -> m Strict.ByteString
parseBinaryValue bs = do
    let chunks = BS.Strict.split (ascii ',') bs
    decoded <- mapM decode chunks
    return $ mconcat $ intersperse "," decoded
  where
    decode :: Strict.ByteString -> m Strict.ByteString
    decode chunk =
        case decodeBase64 chunk of
          Left  err -> throwError err
          Right val -> return val

{-------------------------------------------------------------------------------
  CustomMetadata
-------------------------------------------------------------------------------}

buildCustomMetadata :: CustomMetadata -> HTTP.Header
buildCustomMetadata (CustomMetadata name value) =
    case name of
      BinaryHeader _ -> (buildHeaderName name, buildBinaryValue value)
      AsciiHeader  _ -> (buildHeaderName name, buildAsciiValue  value)

parseCustomMetadata ::
     MonadError (InvalidHeaders GrpcException) m
  => HTTP.Header -> m CustomMetadata
parseCustomMetadata hdr@(name, value) = throwInvalidHeader hdr $ do
    name'     <- parseHeaderName name
    value'    <- case name' of
                   AsciiHeader  _ -> parseAsciiValue  value
                   BinaryHeader _ -> parseBinaryValue value
    -- If parsing succeeds, that justifies the use of 'UnsafeCustomMetadata'
    return $ CustomMetadata name' value'
