{-# LANGUAGE CPP #-}

module Test.Stress.Common
  ( -- * Logging
    say

    -- * Miscellaneous
  , randomMsg
  ) where

import Data.ByteString.Lazy qualified as Lazy
import System.Random

-- | Generate a random 'Lazy.ByteString' between 128 and 256 bytes in length
randomMsg :: IO Lazy.ByteString
randomMsg = do
    g1 <- getStdGen
    let (l, g2) = randomR (128, 256) g1
    return . Lazy.fromStrict . fst $
#if MIN_VERSION_random(1,3,0)
      uniformByteString l g2
#else
      genByteString l g2
#endif

-- | Log the message, if logging is enabled
say :: Bool -> String -> IO ()
say enabled msg
    | enabled
    = putStrLn $ "test-stress: " ++ msg
    | otherwise
    = return ()
