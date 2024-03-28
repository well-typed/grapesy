{-# LANGUAGE OverloadedStrings #-}

-- | Custom metadata
--
-- These are application-defined headers/trailers.
--
-- Intended for unqualified import.
module Network.GRPC.Spec.CustomMetadata.Raw (
    -- * Definition
    CustomMetadata(CustomMetadata)
  , customMetadataName
  , customMetadataValue
  , safeCustomMetadata
  , HeaderName(BinaryHeader, AsciiHeader)
  , safeHeaderName
    -- * Serialization
  , buildHeaderName
  , buildAsciiValue
  , buildBinaryValue
  , buildCustomMetadata
  , parseHeaderName
  , parseAsciiValue
  , parseBinaryValue
  , parseCustomMetadata
  ) where

import Control.Monad
import Control.Monad.Except (MonadError(throwError))
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.List (intersperse)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Word
import GHC.Show
import GHC.Stack
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec.Base64
import Network.GRPC.Util.ByteString (strip, ascii)

{-------------------------------------------------------------------------------
  Definition

  > Custom-Metadata → Binary-Header / ASCII-Header
  > Binary-Header   → {Header-Name "-bin" } {base64 encoded value}
  > ASCII-Header    → Header-Name ASCII-Value

  Implementation note: ASCII headers and binary headers are distinguished based
  on their name (see 'HeaderName'). We do /not/ introduce a different type for
  the /values/ of such headers, because if we did, we would then need additional
  machinery to make sure that binary header names are paired with binary header
  values, and similarly for ASCII headers, with little benefit. Instead we check
  in the smart constructor for 'CustomMetadata' that the header value satisfies
  the rules for the particular type of header.
-------------------------------------------------------------------------------}

-- | Custom metadata
--
-- This is an arbitrary set of key-value pairs defined by the application layer.
--
-- Custom metadata order is not guaranteed to be preserved except for values
-- with duplicate header names. Duplicate header names may have their values
-- joined with "," as the delimiter and be considered semantically equivalent.
data CustomMetadata = UnsafeCustomMetadata {
      customMetadataName  :: HeaderName
    , customMetadataValue :: Strict.ByteString
    }
  deriving stock (Eq)

-- | 'Show' instance relies on the 'CustomMetadata' pattern synonym
instance Show CustomMetadata where
  showsPrec p (UnsafeCustomMetadata name value) = showParen (p >= appPrec1) $
         showString "CustomMetadata "
       . showsPrec appPrec1 name
       . showSpace
       . showsPrec appPrec1 value

-- | Check for valid ASCII header value
--
-- > ASCII-Value → 1*( %x20-%x7E ) ; space and printable ASCII
--
-- NOTE: By rights this should verify that the header is non-empty. However,
-- empty header values do occasionally show up, and so we permit them. The main
-- reason for checking for validity at all is to ensure that we don't confuse
-- binary headers and ASCII headers.
isValidAsciiValue :: Strict.ByteString -> Bool
isValidAsciiValue bs = BS.Strict.all (\c -> 0x20 <= c && c <= 0x7E) bs

safeCustomMetadata :: HeaderName -> Strict.ByteString -> Maybe CustomMetadata
safeCustomMetadata name value =
    case name of
      UnsafeAsciiHeader _ -> do
        guard $ isValidAsciiValue value
        return $ UnsafeCustomMetadata name (strip value)
      UnsafeBinaryHeader _ ->
        -- Values of binary headers are not subject to any constraints
        return $ UnsafeCustomMetadata name value

pattern CustomMetadata ::
     HasCallStack
  => HeaderName -> Strict.ByteString -> CustomMetadata
pattern CustomMetadata name value <- UnsafeCustomMetadata name value
  where
    CustomMetadata name value =
        fromMaybe (invalid constructedForError) $
          safeCustomMetadata name value
      where
        constructedForError :: CustomMetadata
        constructedForError = UnsafeCustomMetadata name value

{-# COMPLETE CustomMetadata #-}

{-------------------------------------------------------------------------------
  Header-Name

  > Header-Name → 1*( %x30-39 / %x61-7A / "_" / "-" / ".") ; 0-9 a-z _ - .
----------------------\--------------------------------------------------------}

-- | Header name
--
-- To construct a 'HeaderName', you can either use the 'IsString' instance
--
-- > "foo"     :: HeaderName -- an ASCII header
-- > "bar-bin" :: HeaderName -- a binary header
--
-- or alternatively use the 'AsciiHeader' and 'BinaryHeader' patterns
--
-- > AsciiHeader  "foo"
-- > BinaryHeader "bar-bin"
--
-- The latter style is more explicit, and can catch more errors:
--
-- > AsciiHeader  "foo-bin" -- exception: unexpected -bin suffix
-- > BinaryHeader "bar"     -- exception: expected   -bin suffix
--
-- Header names cannot be empty, and must consist of digits (@0-9@), lowercase
-- letters (@a-z@), underscore (@_@), hyphen (@-@), or period (@.@).
-- Reserved header names are disallowed.
--
-- See also 'safeHeaderName'.
data HeaderName =
    -- | Binary header
    --
    -- Binary headers will be base-64 encoded.
    --
    -- The header name must have a @-bin@ suffix (runtime libraries use this
    -- suffix to detect binary headers and properly apply base64 encoding &
    -- decoding as headers are sent and received).
    --
    -- Since this is binary data, padding considerations do not apply.
    UnsafeBinaryHeader Strict.ByteString

    -- | ASCII header
    --
    -- ASCII headers cannot be empty, and can only use characters in the range
    -- @0x20 .. 0x7E@. Note that although this range includes whitespace, any
    -- padding will be removed when constructing the value.
    --
    -- The gRPC spec is not precise about what exactly constitutes \"padding\",
    -- but the ABNF spec defines it as "space and horizontal tab"
    -- <https://www.rfc-editor.org/rfc/rfc5234#section-3.1>.
  | UnsafeAsciiHeader Strict.ByteString
  deriving stock (Eq, Ord)

pattern BinaryHeader :: HasCallStack => Strict.ByteString -> HeaderName
pattern BinaryHeader name <- UnsafeBinaryHeader name
  where
    BinaryHeader name =
      case safeHeaderName name of
        Just name'@UnsafeBinaryHeader{} ->
          name'
        Just UnsafeAsciiHeader{} ->
          error "binary headers must have -bin suffix"
        Nothing ->
          error $ "Invalid header name " ++ show name

pattern AsciiHeader :: HasCallStack => Strict.ByteString -> HeaderName
pattern AsciiHeader name <- UnsafeAsciiHeader name
  where
    AsciiHeader name =
      case safeHeaderName name of
        Just name'@UnsafeAsciiHeader{} ->
          name'
        Just UnsafeBinaryHeader{} ->
          error "ASCII headers cannot have -bin suffix"
        Nothing ->
          error $ "Invalid header name " ++ show name

{-# COMPLETE BinaryHeader, AsciiHeader #-}

-- | Check for header name validity
--
-- We choose between 'BinaryHeader' and 'AsciiHeader' based on the presence or
-- absence of a @-bin suffix.
safeHeaderName :: Strict.ByteString -> Maybe HeaderName
safeHeaderName bs = do
    guard $ BS.Strict.length bs >= 1
    guard $ BS.Strict.all isValidChar bs
    guard $ not $ "grpc-" `BS.Strict.isPrefixOf` bs
    guard $ not $ bs `Set.member` reservedNames
    return $ if "-bin" `BS.Strict.isSuffixOf` bs
               then UnsafeBinaryHeader bs
               else UnsafeAsciiHeader  bs
  where
    isValidChar :: Word8 -> Bool
    isValidChar c = or [
          0x30 <= c && c <= 0x39
        , 0x61 <= c && c <= 0x7A
        , c == ascii '_'
        , c == ascii '-'
        , c == ascii '.'
        ]

    -- Reserved header names that do not start with @grpc-@
    reservedNames :: Set Strict.ByteString
    reservedNames = Set.fromList [
          "user-agent"
        , "content-type"
        , "te"
        , "trailer"
        ]

instance IsString HeaderName where
  fromString str =
       fromMaybe (invalid constructedForError) $
         safeHeaderName (fromString str)
    where
      constructedForError :: HeaderName
      constructedForError =
          if "-bin" `List.isSuffixOf` str
            then UnsafeBinaryHeader $ fromString str
            else UnsafeAsciiHeader  $ fromString str

-- | 'Show' instance relies on the 'IsString' instance
instance Show HeaderName where
  show (UnsafeBinaryHeader name) = show name
  show (UnsafeAsciiHeader  name) = show name

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

buildHeaderName :: HeaderName -> CI Strict.ByteString
buildHeaderName name =
    case name of
      UnsafeBinaryHeader name' -> CI.mk name'
      UnsafeAsciiHeader  name' -> CI.mk name'

parseHeaderName :: MonadError String m => CI Strict.ByteString -> m HeaderName
parseHeaderName name =
    case safeHeaderName (CI.foldedCase name) of
      Nothing    -> throwError $ "Invalid header name: " ++ show name
      Just name' -> return name'

buildAsciiValue :: Strict.ByteString -> Strict.ByteString
buildAsciiValue = id

parseAsciiValue ::
     MonadError String m
  => Strict.ByteString -> m Strict.ByteString
parseAsciiValue bs = do
    unless (isValidAsciiValue bs) $
      throwError $ "Invalid ASCII header: " ++ show bs
    return bs

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

buildCustomMetadata :: CustomMetadata -> HTTP.Header
buildCustomMetadata (CustomMetadata name value) =
    case name of
      UnsafeBinaryHeader _ -> (buildHeaderName name, buildBinaryValue value)
      UnsafeAsciiHeader  _ -> (buildHeaderName name, buildAsciiValue  value)

parseCustomMetadata :: MonadError String m => HTTP.Header -> m CustomMetadata
parseCustomMetadata (name, value) = do
    name'     <- parseHeaderName name
    value'    <- case name' of
                   UnsafeAsciiHeader  _ -> parseAsciiValue  value
                   UnsafeBinaryHeader _ -> parseBinaryValue value
    -- If parsing succeeds, that justifies the use of 'UnsafeCustomMetadata'
    return $ UnsafeCustomMetadata name' value'

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

invalid :: (Show a, HasCallStack) => a -> b
invalid x = error $ "Invalid: " ++ show x ++ " at "