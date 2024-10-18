{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.Serialization.Timeout (
    buildTimeout
  , parseTimeout
  ) where

import Control.Monad.Except
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.Char (isDigit)

import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Serialization

  > Timeout      → "grpc-timeout" TimeoutValue TimeoutUnit
  > TimeoutValue → {positive integer as ASCII string of at most 8 digits}
  > TimeoutUnit  → Hour / Minute / Second / Millisecond / Microsecond / Nanosecond
  > Hour         → "H"
  > Minute       → "M"
  > Second       → "S"
  > Millisecond  → "m"
  > Microsecond  → "u"
  > Nanosecond   → "n"
-------------------------------------------------------------------------------}

buildTimeout :: Timeout -> Strict.ByteString
buildTimeout (Timeout unit val) = mconcat [
      BS.Strict.C8.pack $ show $ getTimeoutValue val
    , case unit of
        Hour        -> "H"
        Minute      -> "M"
        Second      -> "S"
        Millisecond -> "m"
        Microsecond -> "u"
        Nanosecond  -> "n"
    ]

parseTimeout :: forall m.
     MonadError String m
  => Strict.ByteString -> m Timeout
parseTimeout bs = do
    let (bsVal, bsUnit) = BS.Strict.C8.span isDigit bs

    val <-
      if BS.Strict.length bsVal < 1 || BS.Strict.length bsVal > 8
        then invalid
        else return . TimeoutValue $ read (BS.Strict.C8.unpack bsVal)

    charUnit <-
      case BS.Strict.C8.uncons bsUnit of
        Nothing ->
          invalid
        Just (u, remainder) ->
          if BS.Strict.null remainder
            then return u
            else invalid

    unit <-
      case charUnit of
        'H' -> return Hour
        'M' -> return Minute
        'S' -> return Second
        'm' -> return Millisecond
        'u' -> return Microsecond
        'n' -> return Nanosecond
        _   -> invalid

    return $ Timeout unit val
  where
    invalid :: m a
    invalid = throwError $ "Could not parse timeout " ++ show bs

