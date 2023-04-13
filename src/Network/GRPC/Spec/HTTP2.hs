{-# LANGUAGE OverloadedStrings #-}

-- | Spec-mandated translation to HTTP2
--
-- This module only contains pure functions; it does not deal with control flow.
--
-- Intended for qualified import.
module Network.GRPC.Spec.HTTP2 (
    -- * Connection
    authority
  , method
  , path
    -- * Requests
  , buildRequestHeaders
    -- * Responses
  , parseResponseHeaders
  , parseResponseTrailers
    -- * Length-prefixed messages
  , MessagePrefix(..)
  , ParseResult(..)
  , buildLengthPrefixedMessage
  , parseLengthPrefixedMessage
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Binary.Get (Get)
import Data.Binary.Get qualified as Binary
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Base64 qualified as BS.Strict.B64
import Data.ByteString.Builder qualified as BS (Builder)
import Data.ByteString.Builder qualified as BS.Builder
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8
import Data.CaseInsensitive qualified as CI
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.SOP
import Data.Version
import Data.Word
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)

import Network.GRPC.Spec
import Network.GRPC.Spec.Compression (Compression(..))
import Network.GRPC.Spec.Compression qualified as Compression
import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.ByteString
import Network.GRPC.Util.HKD (IsHKD)
import Network.GRPC.Util.HKD qualified as HKD

import PackageInfo_grapesy qualified as PackageInfo

{-------------------------------------------------------------------------------
  Connections
-------------------------------------------------------------------------------}

-- | HTTP authority
--
-- From the HTTP2 spec <https://www.rfc-editor.org/rfc/rfc3986#section-3.2>:
--
-- > authority = [ userinfo "@" ] host [ ":" port ]
--
-- The spec also mandates the use of UTF8
-- <https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2>.
--
-- (The example from the gRPC spec just has the hostname here.)
authority :: Authority -> Strict.ByteString
authority auth = mconcat [
      case authorityUserInfo auth of
        Just userInfo -> BS.Strict.UTF8.fromString userInfo <> "@"
        Nothing       -> mempty
    , BS.Strict.UTF8.fromString $ authorityHost auth
    , ":"
    , BS.Strict.C8.pack $ show (authorityPort auth)
    ]

-- | Method is always POST for gRPC
method :: Strict.ByteString
method = "POST"

-- | Path
path :: IsRPC rpc => rpc -> Strict.ByteString
path rpc = "/" <> serviceName rpc <> "/" <> methodName rpc

{-------------------------------------------------------------------------------
  Request headers
-------------------------------------------------------------------------------}

-- | Request headers
--
-- > Request-Headers →
-- >   Call-Definition
-- >   *Custom-Metadata
buildRequestHeaders :: IsRPC rpc => RequestMeta -> rpc -> [HTTP.Header]
buildRequestHeaders meta rpc = concat [
      callDefinition meta rpc
    , map customMetadata $ requestCustomMetadata meta
    ]
  where
    customMetadata :: CustomMetadata -> HTTP.Header
    customMetadata (BinaryHeader name value) = (
          CI.mk $ getHeaderName name <> "-bin"
        , BS.Strict.B64.encode value
        )
    customMetadata (AsciiHeader name value) = (
          CI.mk $ getHeaderName name
        , getAsciiValue value
        )

-- | Call definition
--
-- > Call-Definition →
-- >   Method
-- >   Scheme
-- >   Path
-- >   TE
-- >   [Authority]
-- >   [Timeout]
-- >   Content-Type
-- >   [Message-Type]
-- >   [Message-Encoding]
-- >   [Message-Accept-Encoding]
-- >   [User-Agent]
--
-- However, the spec additionally mandates that
--
--   HTTP2 requires that reserved headers, ones starting with ":" appear
--   before all other headers. Additionally implementations should send
--   Timeout immediately after the reserved headers and they should send the
--   Call-Definition headers before sending Custom-Metadata.
--
-- (Relevant part of the HTTP2 spec:
-- <https://www.rfc-editor.org/rfc/rfc7540#section-8.1.2.1>.) This means
-- @TE@ should come /after/ @Authority@ (if using). However, we will not include
-- the reserved headers here /at all/, as they are automatically added by
-- `http2`.
callDefinition :: IsRPC rpc => RequestMeta -> rpc -> [HTTP.Header]
callDefinition = \meta rpc -> catMaybes [
      hdrTimeout <$> requestTimeout   meta
    , Just $ hdrTe
    , Just $ hdrContentType rpc
    , Just $ hdrMessageType rpc
    , Just $ hdrMessageEncoding (requestCompression meta)
    , Just $ hdrMessageAcceptEncoding (requestAcceptCompression meta)
    , Just $ hdrUserAgent
    ]
  where
    -- > Timeout      → "grpc-timeout" TimeoutValue TimeoutUnit
    -- > TimeoutValue → {positive integer as ASCII string of at most 8 digits}
    -- > TimeoutUnit  → Hour / Minute / Second / Millisecond / Microsecond / Nanosecond
    -- > Hour         → "H"
    -- > Minute       → "M"
    -- > Second       → "S"
    -- > Millisecond  → "m"
    -- > Microsecond  → "u"
    -- > Nanosecond   → "n"
    hdrTimeout :: Timeout -> HTTP.Header
    hdrTimeout (Timeout unit val) = (
          "grpc-timeout"
        , mconcat [
              BS.Strict.C8.pack $ show $ getTimeoutValue val
            , " "
            , case unit of
                Hour        -> "H"
                Minute      -> "M"
                Second      -> "S"
                Millisecond -> "m"
                Microsecond -> "u"
                Nanosecond  -> "n"
            ]
        )

    -- > TE → "te" "trailers" # Used to detect incompatible proxies
    hdrTe :: HTTP.Header
    hdrTe  = ("te", "trailers")

    -- > Content-Type →
    -- >   "content-type"
    -- >   "application/grpc"
    -- >   [("+proto" / "+json" / {custom})]
    hdrContentType :: IsRPC rpc => rpc -> HTTP.Header
    hdrContentType rpc = (
          "content-type"
        , "application/grpc+" <> contentEncoding rpc
        )

    -- > Message-Type → "grpc-message-type" {type name for message schema}
    hdrMessageType :: IsRPC rpc => rpc -> HTTP.Header
    hdrMessageType rpc = ("grpc-message-type", messageType rpc)

    -- > Message-Encoding → "grpc-encoding" Content-Coding
    -- > Content-Coding → "identity" / "gzip" / "deflate" / "snappy" / {custom}
    hdrMessageEncoding :: Compression -> HTTP.Header
    hdrMessageEncoding coding = (
          "grpc-encoding"
        , Compression.serializeId $ compressionId coding
        )

    -- > Message-Accept-Encoding →
    -- >   "grpc-accept-encoding" Content-Coding *("," Content-Coding)
    hdrMessageAcceptEncoding :: NonEmpty Compression -> HTTP.Header
    hdrMessageAcceptEncoding codings = (
          "grpc-accept-encoding"
        , mconcat . intersperse "," $
            map (Compression.serializeId . compressionId) (toList codings)
        )

    -- > User-Agent → "user-agent" {structured user-agent string}
    --
    -- The spec says:
    --
    --   While the protocol does not require a user-agent to function it is
    --   recommended that clients provide a structured user-agent string that
    --   provides a basic description of the calling library, version & platform
    --   to facilitate issue diagnosis in heterogeneous environments. The
    --   following structure is recommended to library developers
    --
    -- > User-Agent →
    -- >   "grpc-"
    -- >   Language
    -- >   ?("-" Variant)
    -- >   "/"
    -- >   Version
    -- >   ?( " ("  *(AdditionalProperty ";") ")" )
    hdrUserAgent :: HTTP.Header
    hdrUserAgent = (
          "user-agent"
        , mconcat [
              "grpc-"
            , "haskell"
            , "-" <> BS.Strict.C8.pack (PackageInfo.name)
            , "/"
            , mconcat . intersperse "." $
                map (BS.Strict.C8.pack . show) $
                  versionBranch PackageInfo.version
            ]
        )

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
parseResponseHeaders :: [HTTP.Header] -> Either String (ResponseHeaders I)
parseResponseHeaders =
      runHeaderParser uninitResponseHeaders
    . mapM_ parseHeader
  where
    -- HTTP2 header names are always lowercase, and must be ASCII.
    -- <https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.2>
    parseHeader :: HTTP.Header -> HeaderParser ResponseHeaders ()
    parseHeader (name, value)
      | name == "content-type"
      = modify $ \partial -> partial{
            responseContentType = Right $
              value
          }

      | name == "grpc-accept-encoding"
      = modify $ \partial -> partial{
            responseAcceptCompression = Right . Just $
              map (Compression.deserializeId . strip) $
                BS.Strict.splitWith (== ascii ',') value
          }

      | "grpc-" `BS.Strict.isPrefixOf` CI.foldedCase name
      = throwError $ "Reserved header: " ++ show (name, value)

      | "-bin" `BS.Strict.isSuffixOf` CI.foldedCase name
      = case safeHeaderName (BS.Strict.dropEnd 4 $ CI.foldedCase name) of
          Just name' ->
            modify $ \partial -> partial{
                responseCustomMetadata = Right $
                    BinaryHeader name' value
                  : fromRight [] (responseCustomMetadata partial)
              }
          _otherwise ->
            throwError $ "Invalid custom binary header: " ++ show (name, value)

      | otherwise
      = case ( safeHeaderName (CI.foldedCase name)
             , safeAsciiValue value
             ) of
          (Just name', Just value') ->
            modify $ \partial -> partial{
                responseCustomMetadata = Right $
                    AsciiHeader name' value'
                  : fromRight [] (responseCustomMetadata partial)
              }
          _otherwise ->
            throwError $ "Invalid custom ASCII header: " ++ show (name, value)

-- | Parse response trailers
--
-- TODO: We don't currently parse the status message. We should find an example
-- server that gives us some, so that we can test.
parseResponseTrailers :: [HTTP.Header] -> Either String (ResponseTrailers I)
parseResponseTrailers =
      runHeaderParser uninitResponseTrailers
    . mapM_ parseHeader
  where
    parseHeader :: HTTP.Header -> HeaderParser ResponseTrailers ()
    parseHeader (name, value)
      | name == "grpc-status"
      = case readMaybe (BS.Strict.C8.unpack value) of
          Nothing -> throwError $ "Invalid status: " ++ show value
          Just v  -> modify $ \partial -> partial{
                         responseGrpcStatus = Right v
                       }

      | otherwise
      = throwError $ "Unrecognized header: " ++ show (name, value)

{-------------------------------------------------------------------------------
  Length-prefixed messages

  These are used both for inputs and outputs
-------------------------------------------------------------------------------}

data MessagePrefix = MessagePrefix {
      msgIsCompressed :: Bool
    , msgLength       :: Word32
    }
  deriving (Show)

buildMessagePrefix :: MessagePrefix -> BS.Builder
buildMessagePrefix MessagePrefix{msgLength, msgIsCompressed} = mconcat [
      BS.Builder.word8    $ if msgIsCompressed then 1 else 0
    , BS.Builder.word32BE $ fromIntegral msgLength
    ]

getMessagePrefix :: Get MessagePrefix
getMessagePrefix = do
    msgIsCompressed <- Binary.getWord8 >>= \case
                         0 -> return False
                         1 -> return True
                         n -> fail $ "parseMessagePrefix: unxpected " ++ show n
    msgLength       <- Binary.getWord32be
    return MessagePrefix{msgIsCompressed, msgLength}

-- | Inputs sent from the client to the server
--
-- > Length-Prefixed-Message → Compressed-Flag Message-Length Message
-- >
-- > Compressed-Flag → 0 / 1
-- >                     # encoded as 1 byte unsigned integer
-- > Message-Length  → {length of Message}
-- >                     # encoded as 4 byte unsigned integer (big endian)
-- > Message         → *{binary octet}
buildLengthPrefixedMessage ::
     IsRPC rpc
  => RequestMeta -> rpc -> Input rpc -> BS.Builder
buildLengthPrefixedMessage meta rpc input = mconcat [
      buildMessagePrefix prefix
    , BS.Builder.lazyByteString serialized
    ]
  where
    serialized :: Lazy.ByteString
    serialized = serializeInput rpc input

    prefix :: MessagePrefix
    prefix = MessagePrefix {
          msgIsCompressed =
            not $ Compression.isIdentity (requestCompression meta)
        , msgLength =
            fromIntegral $ BS.Lazy.length serialized
        }


data ParseResult a =
    -- | We haven't read enough bytes from the remote peer yet
    --
    -- It may well happen that we have insufficient bytes to fully decode the
    -- message, but we /do/ have sufficient bytes to decode the message prefix.
    -- We therefore return the message prefix, if known, as well as the
    -- unconsumed input, to avoid reparsing that message prefix each time.
    InsufficientBytes (Maybe MessagePrefix) Lazy.ByteString

    -- | We have sufficient data, but the parse failed
  | ParseError String

    -- | Parsing succeed
    --
    -- We return the value as well as the remaining bytes
  | Parsed a Lazy.ByteString

-- | Parse length-prefixed message
--
-- TODO: This will need to know about compression.
parseLengthPrefixedMessage ::
     IsRPC rpc
  => rpc
  -> Maybe MessagePrefix  -- ^ Message prefix, if already known
  -> Lazy.ByteString      -- ^ Input (not including prefix, if known)
  -> ParseResult (Output rpc)
parseLengthPrefixedMessage rpc Nothing bs =
    if BS.Lazy.length bs < 5 then
      InsufficientBytes Nothing bs
    else
      case Binary.runGetOrFail getMessagePrefix bs of
        Left (_, _, err) ->
          ParseError err
        Right (unconsumed, _lenConsumed, prefix) ->
          parseLengthPrefixedMessage rpc (Just prefix) unconsumed
parseLengthPrefixedMessage rpc (Just prefix) bs =
    if BS.Lazy.length bs < fromIntegral (msgLength prefix) then
      InsufficientBytes (Just prefix) bs
    else
      let (serialized, unconsumed) =
             BS.Lazy.splitAt (fromIntegral $ msgLength prefix) bs
      in case deserializeOutput rpc serialized of
           Left err -> ParseError err
           Right a  -> Parsed a unconsumed

