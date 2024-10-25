-- | Length-prefixed messages
--
-- These are used both for inputs and outputs.
module Network.GRPC.Spec.Serialization.LengthPrefixed (
    -- * Message prefix
    MessagePrefix(..)
    -- * Length-prefixex messages
    -- ** Construction
  , OutboundMeta(..)
  , buildInput
  , buildOutput
    -- ** Parsing
  , InboundMeta(..)
  , parseInput
  , parseOutput
  ) where

import Data.Binary.Get (Get)
import Data.Binary.Get qualified as Binary
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Proxy
import Data.Word

import Network.GRPC.Spec
import Network.GRPC.Spec.Util.Parser (Parser)
import Network.GRPC.Spec.Util.Parser qualified as Parser

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
     SupportsClientRpc rpc
  => Proxy rpc
  -> Compression
  -> (OutboundMeta, Input rpc)
  -> Builder
buildInput = buildMsg . rpcSerializeInput

-- | Serialize RPC output
buildOutput ::
     SupportsServerRpc rpc
  => Proxy rpc
  -> Compression
  -> (OutboundMeta, Output rpc)
  -> Builder
buildOutput = buildMsg . rpcSerializeOutput

-- | Generalization of 'buildInput' and 'buildOutput'
buildMsg ::
     (x -> Lazy.ByteString)
  -> Compression
  -> (OutboundMeta, x)
  -> Builder
buildMsg build compr (meta, x) = mconcat [
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
          uncompressedSizeThreshold compr uncompressedLength
        , outboundEnableCompression meta
        , compressedLength < uncompressedLength
        ]
      where
        uncompressedLength = BS.Lazy.length uncompressed
        compressedLength = BS.Lazy.length compressed

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
-------------------------------------------------------------------------------}

-- | Parse input
parseInput ::
     SupportsServerRpc rpc
  => Proxy rpc
  -> Compression
  -> Parser String (InboundMeta, Input rpc)
parseInput = parseMsg . rpcDeserializeInput

-- | Parse output
parseOutput ::
     SupportsClientRpc rpc
  => Proxy rpc
  -> Compression
  -> Parser String (InboundMeta, Output rpc)
parseOutput = parseMsg . rpcDeserializeOutput

parseMsg :: forall x.
     (Lazy.ByteString -> Either String x)
  -> Compression
  -> Parser String (InboundMeta, x)
parseMsg parse compr = do
    prefix <- Parser.getExactly 5 getMessagePrefix
    Parser.consumeExactly (fromIntegral $ msgLength prefix) $
      parseBody (msgIsCompressed prefix)
  where
    parseBody :: Bool -> Lazy.ByteString -> Either String (InboundMeta, x)
    parseBody False body =
        (meta,) <$> parse body
      where
        meta :: InboundMeta
        meta = InboundMeta {
              inboundCompressedSize   = Nothing
            , inboundUncompressedSize = lengthOf body
            }
    parseBody True compressed =
        (meta,) <$> parse uncompressed
      where
        uncompressed :: Lazy.ByteString
        uncompressed = decompress compr compressed

        meta :: InboundMeta
        meta = InboundMeta {
              inboundCompressedSize   = Just (lengthOf compressed)
            , inboundUncompressedSize = lengthOf uncompressed
            }

    lengthOf :: Num a => Lazy.ByteString -> a
    lengthOf = fromIntegral . BS.Lazy.length
