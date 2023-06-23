{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Network.GRPC.Util.RedundantConstraint (addConstraint) where

-- | Add redundant constraint without triggering ghc warning
--
-- Example usage:
--
-- > foo :: forall .. . SomeRedundantConstraint => ..
-- > foo .. = ..
-- >   where
-- >     _ = addConstraint @SomeRedundantConstraint
addConstraint :: c => ()
addConstraint = ()