{-# OPTIONS_GHC -O2 #-}

-- | Stateful random number generation
--
-- Intended for qualified import.
--
-- > import KVStore.Util.RandomGen (RandomGen)
-- > import KVStore.Util.RandomGen qualified as RandomGen
module KVStore.Util.RandomGen (
    RandomGen -- opaque
    -- * API
  , new
  , nextDouble
  , nextWord64
  , nextInt
  , RandomBytes(..)
  , nextBytes
  ) where

import Control.Concurrent
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BS.Base16
import Data.ByteString.Char8 qualified as BS.Char8
import Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import Data.Tuple (swap)
import Data.Word
import Foreign qualified as C
import Foreign.Ptr
import System.Random.SplitMix (SMGen)
import System.Random.SplitMix qualified as SplitMix

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Thread-safe PRNG
newtype RandomGen = Wrap {
      unwrap :: MVar SMGen
    }

{-------------------------------------------------------------------------------
  Basic API
-------------------------------------------------------------------------------}

new :: IO RandomGen
new = SplitMix.newSMGen >>= fmap Wrap . newMVar

-- | Generate a Double in @[0, 1)@ range
nextDouble :: RandomGen -> IO Double
nextDouble gen = withGen gen $ SplitMix.nextDouble

nextWord64 :: RandomGen -> IO Word64
nextWord64 gen = withGen gen $ SplitMix.nextWord64

-- | Uniformly distributed int value in @[0, n)@ range
nextInt :: RandomGen -> Int -> IO Int
nextInt gen n
  | n <= 0    = error "bound must be positive"
  | otherwise = (\w -> fromIntegral w `mod` n) <$> nextWord64 gen

{-------------------------------------------------------------------------------
  Random bytes
-------------------------------------------------------------------------------}

newtype RandomBytes = RandomBytes {
      getRandomBytes :: ByteString
    }

instance Show RandomBytes where
  show = BS.Char8.unpack . BS.Base16.encode . getRandomBytes

nextBytes :: RandomGen -> Int -> IO RandomBytes
nextBytes _   0 = return $ RandomBytes BS.empty
nextBytes gen n = modifyMVar (unwrap gen) $ \initGen -> do
    -- Allocate enough memory for a whole number of Word64
    ptr :: Ptr Word64 <- C.mallocBytes (numWords * 8)

    let loop :: SMGen -> [Int] -> IO SMGen
        loop g []     = return g
        loop g (i:is) = do
            let (w, g') = SplitMix.nextWord64 g
            C.poke (ptr `plusPtr` i) w
            loop g' is

    g' <- loop initGen [0, 8 .. n - 1]
    bs <- unsafePackMallocCStringLen (castPtr ptr, n)
    return (g', RandomBytes bs)
  where
    numWords :: Int
    numWords = (n - 1) `div` 8 + 1

{-------------------------------------------------------------------------------
  Internal: wrap pure operations
-------------------------------------------------------------------------------}

withGen :: RandomGen -> (SMGen -> (a, SMGen)) -> IO a
withGen gen f = modifyMVar (unwrap gen) $ return . swap . f
