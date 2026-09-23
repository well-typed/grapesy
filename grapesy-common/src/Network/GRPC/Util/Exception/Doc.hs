-- | Barebones rendering abstraction for nested indentation
module Network.GRPC.Util.Exception.Doc (
    Doc(..)
    -- * Construction
  , fromLines
  , withHeader
    -- * Rendering
  , renderDoc
  ) where

import Data.String
import Data.Semigroup

import Data.Foldable qualified as Foldable

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Doc =
    FromString String
  | VCat [Doc]
  | Indent Int Doc

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

instance IsString Doc where
  fromString = FromString

instance Monoid Doc where
  mempty  = VCat []
  mconcat = VCat

instance Semigroup Doc where
  a <> b  = VCat [a, b]
  sconcat = VCat . Foldable.toList

fromLines :: String -> Doc
fromLines = mconcat . map fromString . lines

withHeader :: String -> Doc -> Doc
withHeader header body = mconcat [
      fromString header
    , Indent 2 body
    ]

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

renderDoc :: Doc -> String
renderDoc = \d ->
      unlines
    $ map (\(i, str) -> replicate i ' ' ++ str)
    $ go [(0, d)]
  where
    go :: [(Int, Doc)] -> [(Int, String)]
    go []            = []
    go ((i, d) : ds) =
        case d of
          FromString str   -> (i, str) : go ds
          VCat    ds'   -> go $ map (i,) ds' ++ ds
          Indent     i' d' -> go $ (i + i', d') : ds
