-- | Incremental parser interface
module Network.GRPC.Util.Parser (
    Parser(..)
  , absurdParser
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Void

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Parser a =
    -- | Successfully parsed a message
    --
    -- Also returns a continuation parser (which, if not all bytes were
    -- consumed by the message, might already have consumed some data; indeed,
    -- it too might be done).
    ParserDone a (Parser a)

    -- | Parser needs more data to continue
    --
    -- We also record the data already consumed by the parser.
  | ParserNeedsData Lazy.ByteString (Strict.ByteString -> Parser a)

    -- | Parse error
    --
    -- We do not provide any recovery: once the parser fails, all data on
    -- the same stream is lost.
  | ParserError String

-- | Parser for 'Void'
--
-- Since there /are/ no values of type 'Void', we wait until we have evidence
-- of the impossible, and then error out.
absurdParser :: Parser Void
absurdParser =
    ParserNeedsData mempty $ \_ ->
    ParserError "Unexpected message of type Void"