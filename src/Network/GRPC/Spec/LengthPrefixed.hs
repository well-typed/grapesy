-- | Length-prefixed messages
--
-- These are used both for inputs and outputs.
module Network.GRPC.Spec.LengthPrefixed (
    -- * Message prefix
    MessagePrefix(..)
    -- * Length-prefixex messages
    -- ** Construction
  , OutboundEnvelope(..)
  , buildInput
  , buildOutput
    -- ** Parsing
  , InboundEnvelope(..)
  , parseInput
  , parseOutput
  ) where

import Data.Binary.Get (Get)
import Data.Binary.Get qualified as Binary
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Default
import Data.Proxy
import Data.Word

import Network.GRPC.Spec.Compression
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.Parser (Parser(..))

{-------------------------------------------------------------------------------
  Message prefix
-------------------------------------------------------------------------------}

data MessagePrefix = MessagePrefix {
      msgIsCompressed :: Bool
    , msgLength       :: Word32
    }
  deriving (Show)

buildMessagePrefix :: MessagePrefix -> Builder
buildMessagePrefix MessagePrefix{msgLength, msgIsCompressed} = mconcat [
      Builder.word8    $ if msgIsCompressed then 1 else 0
    , Builder.word32BE $ msgLength
    ]

getMessagePrefix :: Get MessagePrefix
getMessagePrefix = do
    msgIsCompressed <- Binary.getWord8 >>= \case
                         0 -> return False
                         1 -> return True
                         n -> fail $ "parseMessagePrefix: unxpected " ++ show n
    msgLength       <- Binary.getWord32be
    return MessagePrefix{msgIsCompressed, msgLength}

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

data OutboundEnvelope = OutboundEnvelope {
      -- | Enable compression for this message
      --
      -- Even if enabled, compression will only be used if this results in a
      -- smaller message.
      outboundEnableCompression :: Bool
    }
  deriving stock (Show)

instance Default OutboundEnvelope where
  def = OutboundEnvelope {
        outboundEnableCompression = True
      }

-- | Serialize RPC input
--
-- > Length-Prefixed-Message → Compressed-Flag Message-Length Message
-- >
-- > Compressed-Flag → 0 / 1
-- >                     # encoded as 1 byte unsigned integer
-- > Message-Length  → {length of Message}
-- >                     # encoded as 4 byte unsigned integer (big endian)
-- > Message         → *{binary octet}
buildInput ::
     IsRPC rpc
  => Proxy rpc
  -> Compression
  -> (OutboundEnvelope, Input rpc)
  -> Builder
buildInput = buildMsg . serializeInput

-- | Serialize RPC output
buildOutput ::
     IsRPC rpc
  => Proxy rpc
  -> Compression
  -> (OutboundEnvelope, Output rpc)
  -> Builder
buildOutput = buildMsg . serializeOutput

-- | Generalization of 'buildInput' and 'buildOutput'
buildMsg ::
     (x -> Lazy.ByteString)
  -> Compression
  -> (OutboundEnvelope, x)
  -> Builder
buildMsg build compr (envelope, x) = mconcat [
      buildMessagePrefix prefix
    , Builder.lazyByteString $
        if shouldCompress
          then compressed
          else uncompressed
    ]
  where
    uncompressed, compressed :: Lazy.ByteString
    uncompressed = build x
    compressed   = compress compr uncompressed

    shouldCompress :: Bool
    shouldCompress = and [
          not $ compressionIsIdentity compr
        , outboundEnableCompression envelope
        , BS.Lazy.length compressed < BS.Lazy.length uncompressed
        ]

    prefix :: MessagePrefix
    prefix
      | shouldCompress
      = MessagePrefix {
            msgIsCompressed = True
          , msgLength       = fromIntegral $ BS.Lazy.length compressed
          }

      | otherwise
      = MessagePrefix {
            msgIsCompressed = False
          , msgLength       = fromIntegral $ BS.Lazy.length uncompressed
          }

{-------------------------------------------------------------------------------
  Parsing

  TODO: We should stress-test this parser, cutting the data stream into
  arbitrary chunks, ideally both by calling the parser directly and through the
  larger library, testing 'Network.GRPC.Call.Peer.receiveMessages'.
-------------------------------------------------------------------------------}

data InboundEnvelope = InboundEnvelope {
      -- | Size of the message in compressed form, /if/ it was compressed
      inboundCompressedSize :: Maybe Word32

      -- | Size of the message in uncompressed (but still serialized) form
    , inboundUncompressedSize :: Word32
    }
  deriving stock (Show)

parseInput ::
     IsRPC rpc
  => Proxy rpc
  -> Compression
  -> Parser (InboundEnvelope, Input rpc)
parseInput = parseMsg . deserializeInput

parseOutput ::
     IsRPC rpc
  => Proxy rpc
  -> Compression
  -> Parser (InboundEnvelope, Output rpc)
parseOutput = parseMsg . deserializeOutput

parseMsg :: forall x.
     (Lazy.ByteString -> Either String x)
  -> Compression
  -> Parser (InboundEnvelope, x)
parseMsg parse compr =
    waitForPrefix BS.Lazy.empty
  where
    waitForPrefix :: Lazy.ByteString -> Parser (InboundEnvelope, x)
    waitForPrefix acc
      | BS.Lazy.length acc < 5
      = ParserNeedsData acc $ \next -> waitForPrefix (snoc acc next)

      | otherwise
      = case Binary.runGetOrFail getMessagePrefix acc of
          Left (_, _, err) ->
            ParserError err
          Right (unconsumed, _lenConsumed, prefix) ->
            withPrefix prefix unconsumed

    withPrefix ::
         MessagePrefix
      -> Lazy.ByteString
      -> Parser (InboundEnvelope, x)
    withPrefix prefix acc
      | BS.Lazy.length acc < fromIntegral (msgLength prefix)
      = ParserNeedsData acc $ \next -> withPrefix prefix (snoc acc next)

      | otherwise
      = let (msg, rest) = BS.Lazy.splitAt (fromIntegral $ msgLength prefix) acc
            serialized  = if msgIsCompressed prefix
                            then decompress compr msg
                            else msg
            envelope    = InboundEnvelope {
                              inboundCompressedSize =
                                if msgIsCompressed prefix
                                  then Just $ msgLength prefix
                                  else Nothing
                            , inboundUncompressedSize =
                                fromIntegral $ BS.Lazy.length serialized
                            }
        in case parse serialized of
             Left err -> ParserError err
             Right a  -> ParserDone (envelope, a) $ waitForPrefix rest

    snoc :: Lazy.ByteString -> Strict.ByteString -> Lazy.ByteString
    snoc acc next = acc <> BS.Lazy.fromStrict next
