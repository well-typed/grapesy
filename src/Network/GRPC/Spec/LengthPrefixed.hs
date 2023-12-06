-- | Length-prefixed messages
--
-- These are used both for inputs and outputs.
module Network.GRPC.Spec.LengthPrefixed (
    -- * Message prefix
    MessagePrefix(..)
    -- * Length-prefixex messages
  , buildInput
  , buildOutput
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
import Data.Proxy
import Data.Word

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
    , Builder.word32BE $ fromIntegral msgLength
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

  TODO: We currently /always/ use the specified compression algorithm. We might
  prefer to use identity coding when the compressed message turns out to be
  larger than the original.
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
buildInput :: IsRPC rpc => Proxy rpc -> Input rpc -> Builder
buildInput = buildMsg . serializeInput

-- | Serialize RPC output
buildOutput :: IsRPC rpc => Proxy rpc -> Output rpc -> Builder
buildOutput = buildMsg . serializeOutput

-- | Generalization of 'buildInput' and 'buildOutput'
buildMsg :: (x -> Lazy.ByteString) -> x -> Builder
buildMsg build x = mconcat [
      buildMessagePrefix prefix
    , Builder.lazyByteString compressed
    ]
  where
    compressed :: Lazy.ByteString
    compressed = build x

    prefix :: MessagePrefix
    prefix = MessagePrefix {
          msgIsCompressed = False
        , msgLength       = fromIntegral $ BS.Lazy.length compressed
        }

{-------------------------------------------------------------------------------
  Parsing

  TODO: We should stress-test this parser, cutting the data stream into
  arbitrary chunks, ideally both by calling the parser directly and through the
  larger library, testing 'Network.GRPC.Call.Peer.receiveMessages'.
-------------------------------------------------------------------------------}

parseInput :: IsRPC rpc => Proxy rpc -> Parser (Input rpc)
parseInput = parseMsg . deserializeInput

parseOutput :: IsRPC rpc => Proxy rpc -> Parser (Output rpc)
parseOutput = parseMsg . deserializeOutput

parseMsg :: forall x.
     (Lazy.ByteString -> Either String x)
  -> Parser x
parseMsg parse =
    waitForPrefix BS.Lazy.empty
  where
    waitForPrefix :: Lazy.ByteString -> Parser x
    waitForPrefix acc
      | BS.Lazy.length acc < 5
      = ParserNeedsData acc $ \next -> waitForPrefix (snoc acc next)

      | otherwise
      = case Binary.runGetOrFail getMessagePrefix acc of
          Left (_, _, err) ->
            ParserError err
          Right (unconsumed, _lenConsumed, prefix) ->
            withPrefix prefix unconsumed

    withPrefix :: MessagePrefix -> Lazy.ByteString -> Parser x
    withPrefix prefix acc
      | BS.Lazy.length acc < fromIntegral (msgLength prefix)
      = ParserNeedsData acc $ \next -> withPrefix prefix (snoc acc next)

      | otherwise
      = let (msg, rest) = BS.Lazy.splitAt (fromIntegral $ msgLength prefix) acc
            serialized  = if msgIsCompressed prefix
                            then error "parseMsg: compression unsupported"
                            else msg
        in case parse serialized of
             Left err -> ParserError err
             Right a  -> ParserDone a $ waitForPrefix rest

    snoc :: Lazy.ByteString -> Strict.ByteString -> Lazy.ByteString
    snoc acc next = acc <> BS.Lazy.fromStrict next
