module Network.GRPC.Util.HeaderTable (
    -- * General auxiliary
    fromHeaderTable,
) where

import Network.GRPC.Util.Imports

import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Semantics qualified as HTTP.Semantics

{-------------------------------------------------------------------------------
  General auxiliary
-------------------------------------------------------------------------------}


fromHeaderTable :: HTTP.Semantics.TokenHeaderTable -> [HTTP.Header]
fromHeaderTable = map (first HTTP.Semantics.tokenKey) . fst
