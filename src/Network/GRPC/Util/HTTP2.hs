module Network.GRPC.Util.HTTP2 (
    fromHeaderTable
  ) where

import Data.Bifunctor
import Network.HPACK qualified as HPACK
import Network.HPACK.Token qualified as HPACK
import Network.HTTP.Types qualified as HTTP

fromHeaderTable :: HPACK.HeaderTable -> [HTTP.Header]
fromHeaderTable = map (first HPACK.tokenKey) . fst
