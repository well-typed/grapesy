module Test.Stress.Common where

collatz :: Word -> Word
collatz n
    | even n
    = n `div` 2
    | otherwise
    = n * 3 + 1
