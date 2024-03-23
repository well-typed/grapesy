{-# LANGUAGE CPP #-}

module Network.GRPC.Util.ByteString (
    ascii
  , strip
  ) where

import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.Char
import Data.Word
import GHC.Stack

ascii :: HasCallStack => Char -> Word8
ascii c
  | 0 <= x && x <= 127 = fromIntegral x
  | otherwise          = error $ "ascii: not an ASCII character " ++ show c
  where
    x :: Int
    x = ord c

strip :: Strict.ByteString -> Strict.ByteString
strip =
      BS.Strict.dropWhileEnd isWhitespace
    . BS.Strict.dropWhile    isWhitespace
  where
    isWhitespace :: Word8 -> Bool
    isWhitespace c = or [
          c == ascii ' '
        , c == ascii '\t'
        ]
