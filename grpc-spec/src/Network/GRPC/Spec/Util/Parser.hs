-- | Incremental parser interface
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.Util.Parser (Parser)
-- > import Network.GRPC.Spec.Util.Parser qualified as Parser
module Network.GRPC.Spec.Util.Parser (
    Parser -- opaque
    -- * Construction
  , consumeExactly
  , getExactly
    -- * Execution
  , IsFinal
  , Leftover
  , ProcessResult(..)
  , processAll
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Binary (Get)
import Data.Binary.Get qualified as Binary
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Int

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype Parser e a = Parser {
      runParser :: Accumulator -> Result e a
    }

data Result e a =
    -- | Parsing failed
    --
    -- This implies that we can stop parsing: getting more data won't fix the
    -- problem (see also 'InsufficientData')
    Failed e

    -- | We make some partial progress, but we need more data to continue
  | NeedData (Parser e a) Accumulator

    -- | Parsing succeeded
    --
    -- Also returns the left-over data
  | Done a Accumulator

instance Bifunctor Result where
  bimap f _ (Failed e)      = Failed (f e)
  bimap _ g (Done a bs)     = Done (g a) bs
  bimap f g (NeedData p bs) = NeedData (bimap f g p) bs

instance Functor (Result e) where
  fmap = second

instance Bifunctor Parser where
  bimap f g (Parser p) = Parser (bimap f g . p)

instance Functor (Parser e) where
  fmap = second

instance Applicative (Parser e) where
  pure x = Parser $ Done x
  (<*>)  = ap

instance Monad (Parser e) where
  f >>= g = Parser $ \bs ->
      case runParser f bs of
        Failed e        -> Failed e
        NeedData f' bs' -> NeedData (f' >>= g) bs'
        Done a bs'      -> runParser (g a) bs'

instance MonadFail (Parser String) where
  fail err = Parser $ \_ -> Failed err

{-------------------------------------------------------------------------------
  Accumulated input

  This is an internal abstraction only; client code never sees this.
-------------------------------------------------------------------------------}

data Accumulator = Accumulator {
      -- | All the chunks received so far, in reverse order
      accumulatedChunks :: [Strict.ByteString]

      -- | Total accumulated length
    , accumulatedLength :: !Int64
    }

nil :: Accumulator
nil = Accumulator {
      accumulatedChunks = []
    , accumulatedLength = 0
    }

-- | Append chunks at the end of the accumulator
--
-- @O(1)@ (this is the raison d'être of this abstraction)
snoc :: Accumulator -> Strict.ByteString -> Accumulator
snoc acc chunk = Accumulator {
      accumulatedChunks =
        chunk : accumulatedChunks acc
    , accumulatedLength =
        accumulatedLength acc + fromIntegral (BS.Strict.length chunk)
    }

toLazy :: Accumulator -> Lazy.ByteString
toLazy = BS.Lazy.fromChunks . reverse . accumulatedChunks

split :: Int64 -> Accumulator -> Maybe (Lazy.ByteString, Accumulator)
split n acc
  | n > accumulatedLength acc
  = Nothing

  | otherwise
  = let bs            = toLazy acc
        (front, back) = BS.Lazy.splitAt n bs
        remainder     = Accumulator {
            accumulatedChunks = reverse (BS.Lazy.toChunks back)
          , accumulatedLength = accumulatedLength acc - n
          }
    in Just (front, remainder)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

consumeExactly :: forall e a.
     Int64                           -- ^ Length
  -> (Lazy.ByteString -> Either e a) -- ^ Parser
  -> Parser e a
consumeExactly len parse = go
  where
    go :: Parser e a
    go = Parser $ \acc ->
        case split len acc of
          Nothing          -> NeedData go acc
          Just (raw, left) -> either Failed (flip Done left) $ parse raw

-- | Convenience wrapper around 'consumeExactly'
getExactly :: Int64 -> Get a -> Parser String a
getExactly len get =
    consumeExactly len $ either failed ok . Binary.runGetOrFail get
  where
    failed :: (Lazy.ByteString, Binary.ByteOffset, String) -> Either String a
    failed (_, _, err) = Left err

    ok :: (Lazy.ByteString, Binary.ByteOffset, a) -> Either String a
    ok (unconsumed, lenConsumed, a)
      | not (BS.Lazy.null unconsumed) = Left "Unconsumed data"
      | lenConsumed /= len            = error "impossible"
      | otherwise                     = Right a

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

type IsFinal  = Bool
type Leftover = Lazy.ByteString

data ProcessResult e b =
    -- | Parse error during processing
    ProcessError e

    -- | Parsing succeeded (compare to 'ProcessedWithoutFinal')
  | ProcessedWithFinal b Leftover

    -- | Parsing succeeded, but we did not recognize the final message on time
    --
    -- There are two ways that parsing can terminate: the final few chunks may
    -- look like this:
    --
    -- > chunk1       -- not marked final
    -- > chunk2       -- not marked final
    -- > chunk3       -- marked final
    --
    -- or like this:
    --
    -- > chunk1       -- not marked final
    -- > chunk2       -- not marked final
    -- > chunk3       -- not marked final
    -- > empty chunk  -- marked final
    --
    -- In the former case, we know that we are processing the final message /as/
    -- we are processing it ('ProcessedFinal'); in the latter case, we realize
    -- this only after we receive the final empty chunk.
  | ProcessedWithoutFinal Leftover

-- | Process all incoming data
--
-- Returns any unprocessed data.
-- Also returns if we knew that the final result
-- was in fact the final result when we received it (this may or may not be the
-- case, depending on
processAll :: forall m e a b.
     Monad m
  => m (Strict.ByteString, IsFinal)  -- ^ Get next chunk
  -> (a -> m ())                     -- ^ Process value
  -> (a -> m b)                      -- ^ Process final value
  -> Parser e a                      -- ^ Parser
  -> m (ProcessResult e b)
processAll getChunk processOne processFinal parser =
    go $ runParser parser nil
  where
    go :: Result e a -> m (ProcessResult e b)
    go (Failed err)            = return $ ProcessError err
    go (Done a left)           = processOne a >> go (runParser parser left)
    go (NeedData parser' left) = do
        (bs, isFinal) <- getChunk
        if not isFinal
          then go         $ runParser parser' (left `snoc` bs)
          else goFinal [] $ runParser parser' (left `snoc` bs)

    -- We have received the final chunk; extract all messages until we are done
    goFinal :: [a] -> Result e a -> m (ProcessResult e b)
    goFinal _   (Failed err)      = return $ ProcessError err
    goFinal acc (Done a left)     = goFinal (a:acc) $ runParser parser left
    goFinal acc (NeedData _ left) = do
        mb <- processLastFew (reverse acc)
        return $ case mb of
                   Just b  -> ProcessedWithFinal b  $ toLazy left
                   Nothing -> ProcessedWithoutFinal $ toLazy left

    processLastFew :: [a] -> m (Maybe b)
    processLastFew []     = return Nothing
    processLastFew [a]    = Just <$> processFinal a
    processLastFew (a:as) = processOne a >> processLastFew as

