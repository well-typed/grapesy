-- | Incremental parser interface
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Util.Parser (Parser)
-- > import Network.GRPC.Util.Parser qualified as Parser
module Network.GRPC.Util.Parser (
    Parser -- opaque
    -- * Construction
  , consumeExactly
  , getExactly
    -- * Execution
  , processAll
  , processAllIO
  ) where

import Control.Exception
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

-- | Process all incoming data
--
-- Returns any unprocessed data.
processAll :: forall m e a.
     Monad m
  => m Strict.ByteString  -- ^ Get next chunk (empty indicates end of input)
  -> (a -> m ())          -- ^ Process single value
  -> Parser e a           -- ^ Parser
  -> m (Either e Lazy.ByteString)
processAll getChunk processOne parser =
    go $ runParser parser nil
  where
    go :: Result e a -> m (Either e Lazy.ByteString)
    go (Failed err)      = return $ Left err
    go (Done result bs') = processOne result >> go (runParser parser bs')
    go (NeedData parser' acc) = do
        bs <- getChunk
        if not (BS.Strict.null bs)
          then go $ runParser parser' (snoc acc bs)
          else return $ Right (toLazy acc)

-- | Wrapper around 'processAll' that throws errors as exceptions
processAllIO :: forall e a.
     Exception e
  => IO Strict.ByteString
  -> (a -> IO ())
  -> Parser e a
  -> IO Lazy.ByteString
processAllIO getChunk processOne parser =
    processAll getChunk processOne parser >>= either throwIO return
