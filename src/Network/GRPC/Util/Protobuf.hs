-- | Protobuf utilities
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Util.Protobuf qualified as Protobuf
module Network.GRPC.Util.Protobuf (
    -- * Serialization
    parseStrict
  , parseLazy
  , buildStrict
  , buildLazy
  ) where

import Data.Binary.Builder qualified as Builder
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ProtoLens (Message)
import Data.ProtoLens qualified as Protobuf
import Data.ProtoLens.Encoding.Parser (Parser)
import Data.ProtoLens.Encoding.Parser qualified as Protobuf
import Data.Proxy
import Data.Text qualified as Text

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

parseStrict :: Message msg => Strict.ByteString -> Either String msg
parseStrict = Protobuf.runParser parseMessage

-- | Parse lazy bytestring
--
-- TODO: <https://github.com/well-typed/grapesy/issues/119>.
-- We currently turn this into a strict bytestring before parsing.
parseLazy :: Message msg => Lazy.ByteString -> Either String msg
parseLazy = parseStrict . BS.Lazy.toStrict

buildStrict :: Message msg => msg -> Strict.ByteString
buildStrict = BS.Lazy.toStrict . buildLazy

buildLazy :: Message msg => msg -> Lazy.ByteString
buildLazy = Builder.toLazyByteString . Protobuf.buildMessage

{-------------------------------------------------------------------------------
  Internal auxilairy
-------------------------------------------------------------------------------}

parseMessage :: forall msg. Protobuf.Message msg => Parser msg
parseMessage = do
    msg   <- Protobuf.parseMessage
    atEnd <- Protobuf.atEnd
    if atEnd then
      return msg
    else
      fail $ concat [
          Text.unpack $ Protobuf.messageName $ Proxy @msg
        , ": unconsumed bytes"
        ]
