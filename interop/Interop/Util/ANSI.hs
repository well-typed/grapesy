{-# LANGUAGE OverloadedStrings #-}

module Interop.Util.ANSI (
    Doc(Color, Show)
  , putDoc
  , putDocLn
    -- * Re-exports
  , ColorIntensity(..)
  , Color(..)
  , IsString(..)
  ) where

import Data.String
import System.Console.ANSI
import System.IO

data Doc =
    Color Color Doc
  | String String
  | forall a. Show a => Show a
  | Empty
  | Append Doc Doc

instance Semigroup Doc where (<>)       = Append
instance Monoid    Doc where mempty     = Empty
instance IsString  Doc where fromString = String

putDoc :: Doc -> IO ()
putDoc = \doc -> do
    stdoutSupportsANSI <- hSupportsANSIColor stdout
    if stdoutSupportsANSI
      then go doc
      else putStr $ flattenDoc doc
  where
    -- TODO: By rights we should keep track of the state of the console here,
    -- so that we can properly deal with nesting, rather than just resetting
    -- everything. For now this is good enough.
    go :: Doc -> IO ()
    go (Color color doc) = do
      setSGR [SetColor Foreground Vivid color]
      go doc
      setSGR [Reset]
    go (String str) =
      putStr str
    go (Show x) =
      putStr $ show x
    go Empty =
      return ()
    go (Append doc1 doc2) = do
      go doc1
      go doc2

putDocLn :: Doc -> IO ()
putDocLn doc = putDoc (doc <> "\n")

flattenDoc :: Doc -> String
flattenDoc (Color _ doc)      = flattenDoc doc
flattenDoc (String str)       = str
flattenDoc (Show x)           = show x
flattenDoc Empty              = mempty
flattenDoc (Append doc1 doc2) = mappend (flattenDoc doc1) (flattenDoc doc2)

