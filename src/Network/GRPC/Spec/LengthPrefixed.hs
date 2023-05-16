-- | Length-prefixed messages
--
-- These are used both for inputs and outputs.
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.LengthPrefixed qualified as LengthPrefixed
module Network.GRPC.Spec.LengthPrefixed (
    -- * Message prefix
    MessagePrefix(..)
    -- * Construction
  , build
    -- * Parsing
  , ParseResult(..)
  , parse
  ) where

import Data.Binary.Get (Get)
import Data.Binary.Get qualified as Binary
import Data.ByteString.Builder qualified as BS (Builder)
import Data.ByteString.Builder qualified as BS.Builder
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Word

import Network.GRPC.Spec.Compression (Compression)
import Network.GRPC.Spec.Compression qualified as Compression
import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  Message prefix
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

{-------------------------------------------------------------------------------
  Construction

  TODO: We currently /always/ use the specified compression algorithm. We might
  prefer to use identity coding when the compressed message turns out to be
  larger than the original.
-------------------------------------------------------------------------------}

-- | Inputs sent from the client to the server
--
-- > Length-Prefixed-Message → Compressed-Flag Message-Length Message
-- >
-- > Compressed-Flag → 0 / 1
-- >                     # encoded as 1 byte unsigned integer
-- > Message-Length  → {length of Message}
-- >                     # encoded as 4 byte unsigned integer (big endian)
-- > Message         → *{binary octet}
build :: IsRPC rpc => Compression -> rpc -> Input rpc -> BS.Builder
build compr rpc input = mconcat [
      buildMessagePrefix prefix
    , BS.Builder.lazyByteString compressed
    ]
  where
    compressed :: Lazy.ByteString
    compressed = Compression.compress compr $ serializeInput rpc input

    prefix :: MessagePrefix
    prefix = MessagePrefix {
          msgIsCompressed = not $ Compression.isIdentity compr
        , msgLength       = fromIntegral $ BS.Lazy.length compressed
        }

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

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
parse ::
     IsRPC rpc
  => Compression
  -> rpc
  -> Maybe MessagePrefix  -- ^ Message prefix, if already known
  -> Lazy.ByteString      -- ^ Input (not including prefix, if known)
  -> ParseResult (Output rpc)
parse compr rpc Nothing bs =
    if BS.Lazy.length bs < 5 then
      InsufficientBytes Nothing bs
    else
      case Binary.runGetOrFail getMessagePrefix bs of
        Left (_, _, err) ->
          ParseError err
        Right (unconsumed, _lenConsumed, prefix) ->
          parse compr rpc (Just prefix) unconsumed
parse compr rpc (Just prefix) bs =
    if BS.Lazy.length bs < fromIntegral (msgLength prefix) then
      InsufficientBytes (Just prefix) bs
    else
      let (msg, rest) = BS.Lazy.splitAt (fromIntegral $ msgLength prefix) bs
          serialized  = if msgIsCompressed prefix
                          then Compression.decompress compr msg
                          else msg
      in case deserializeOutput rpc serialized of
           Left err -> ParseError err
           Right a  -> Parsed a rest
