module Test.Prop.IncrementalParsing (tests) where

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.List (inits)
import Data.List qualified as List
import Data.Word
import Test.Tasty
import Test.Tasty.QuickCheck

import Network.GRPC.Util.Parser (Parser)
import Network.GRPC.Util.Parser qualified as Parser

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Prop.IncrementalParsing" [
      testProperty "parser" test_parser
    ]

test_parser :: Input -> [ChunkSize] -> PhraseSize -> Property
test_parser input splits phraseSize =
      counterexample ("chunks: " ++ show chunks)
    $ case processAll chunks processPhrase (parsePhrase phraseSize) of
        Left err ->
          counterexample ("Unexpected failure " ++ show err) $ False
        Right unconsumed ->
              fromIntegral (BS.Lazy.length unconsumed)
          === inputLength input `mod` getPhraseSize phraseSize
  where
    chunks :: [Strict.ByteString]
    chunks = mkChunks input splits

{-------------------------------------------------------------------------------
  Pure parser execution
-------------------------------------------------------------------------------}

-- | Monad in which we can execute the parser
--
-- 'StateT' is used to feed chunks of the input to the parser ('getChunk').
newtype Pure a = Pure {
      unwrapPure :: StateT [Strict.ByteString] (Except String) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadState [Strict.ByteString]
    , MonadError String
    )

runPure ::
     [Strict.ByteString]
  -> Pure a
  -> Either String (a, [Strict.ByteString])
runPure chunks =
      runExcept
    . flip runStateT chunks
    . unwrapPure

getChunk :: Pure Strict.ByteString
getChunk =
    state $ \case
      []   -> (BS.Strict.empty, [])
      c:cs -> (c, cs)

processAll ::
     [Strict.ByteString]
  -> (a -> Pure ())
  -> Parser String a
  -> Either String Lazy.ByteString
processAll chunks processOne p =
    runPure chunks aux >>= verifyAllChunksConsumed
  where
    -- 'processAll' does not assume that the monad @m@ in which it is executed
    -- has any way of reporting errors: if there is a parse failure during
    -- execution, this failure is returned as a value. For the specific case of
    -- 'Pure', however, we /can/ throw errors in @m@ (to allow 'processOne' to
    -- throw errors), so we can reuse that also for any parse failures.
    aux :: Pure Lazy.ByteString
    aux =
            Parser.processAll getChunk processOne p
        >>= either throwError return

    -- 'processAll' should run until all chunks are used
    verifyAllChunksConsumed ::
         (Lazy.ByteString, [Strict.ByteString])
      -> Either String Lazy.ByteString
    verifyAllChunksConsumed (unconsumed, leftoverChunks)
      | null leftoverChunks
      = Right unconsumed

      | otherwise
      = Left "not all chunks consumed"

{-------------------------------------------------------------------------------
  Test input

  The strategy is as follows:

  * We construct an 'Input' of arbitrary length in the form

    ```
    [0, 1, 2, .., 255, 0, 1, 2, .., 255, 0, ..]
    ```

  * We split this input into non-empty chunks of varying sizes @[ChunkSize]@.

  * We then choose a non-zero 'PhraseSize' @n@. The idea is that the parser
    splits the input into phrases of @n@ bytes

    ```
    [0, 1, 2, .., 255, 0, 1, 2, .., 255, 0, ..]
    \-----/\-----/\-----/\-----/\-----/
       n      n      n      n      n
    ```

    The parser returns the phrase after checking its length. This verifies that
    the parser framework gives us correct size inputs, independent of the sizes
    of the chunks of the input.

  * To process each phrase, we verify that each value in the phrase is the
    successor of the previous (mod 256). This verifies that the parser framework
    does not duplicate or miss any chunks.

  * Finally, in the main property, we check that no parse errors are reported
    and that the unconsumed input has the expected length. We also verify
    (in 'processAll') that all input chunks are fed to the parser.
-------------------------------------------------------------------------------}

newtype Input      = Input      { getInputBytes :: [Word8] } deriving (Show)
newtype ChunkSize  = ChunkSize  { getChunkSize  :: Int     } deriving (Show)
newtype PhraseSize = PhraseSize { getPhraseSize :: Int     } deriving (Show)

inputLength :: Input -> Int
inputLength = length . getInputBytes

mkChunks :: Input -> [ChunkSize] -> [Strict.ByteString]
mkChunks (Input bytes) =
    go bytes
  where
    go :: [Word8] -> [ChunkSize] -> [Strict.ByteString]
    go [] _      = []
    go bs []     = [BS.Strict.pack bs]
    go bs (s:ss) = let (pref, suf) = List.splitAt (getChunkSize s) bs
                   in BS.Strict.pack pref : go suf ss

parsePhrase :: PhraseSize -> Parser String [Word8]
parsePhrase (PhraseSize n) =
    Parser.consumeExactly (fromIntegral n) checkLength
  where
    checkLength :: Lazy.ByteString -> Either String [Word8]
    checkLength bs
      | BS.Lazy.length bs /= fromIntegral n
      = Left $ "Unexpected phrase length"

      | otherwise
      = Right $ BS.Lazy.unpack bs

processPhrase :: [Word8] -> Pure ()
processPhrase phrase =
    case phrase of
      []     -> throwError "Empty phrase"
      (x:xs) -> go x xs
  where
    go :: Word8 -> [Word8] -> Pure ()
    go _    []        = return ()
    go prev (x:xs)
      | x == prev + 1 = go x xs
      | otherwise     = throwError $ "Invalid phrase " ++ show phrase

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary Input where
  arbitrary = sized $ \n -> do
                len <- choose (0, n * 100)
                return $ Input $ take len $ cycle [0 .. 255]
  shrink    = map Input . init . inits . getInputBytes

deriving via Positive Int instance Arbitrary ChunkSize
deriving via Positive Int instance Arbitrary PhraseSize
