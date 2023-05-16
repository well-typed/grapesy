{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Network.GRPC.Util.RedundantConstraint (addConstraint) where

import Data.Proxy

-- | Add redundant constraint without triggering ghc warning
addConstraint :: c => Proxy c -> ()
addConstraint _ = ()