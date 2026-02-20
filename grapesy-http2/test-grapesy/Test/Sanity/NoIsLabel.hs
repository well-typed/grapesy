{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sanity.NoIsLabel (tests) where

import Data.Proxy
import Data.String
import GHC.OverloadedLabels
import GHC.TypeLits
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Common.Protobuf ()
import Network.GRPC.Common.Protobuf.Any ()

-- | Too-polymorphic instance of 'IsLabel'
--
-- \"Good\" 'IsLabel' instances should have a concrete type, rather than just a
-- variable variable @a@. However, some packages (notably @lens@) define a very
-- general instance; this is problematic if @Data.ProtoLens.Labels@ is in scope.
-- We should therefore avoid importing from this module in @grapesy@, leaving
-- the choice whether or not to use @Data.ProtoLens.Labels@ to the user.
--
-- The point of this test module is to verify that no 'IsLabel' instance is in
-- scope even if we import from @Network.GRPC.Common.Protobuf@.
instance (KnownSymbol symb, IsString q) => IsLabel symb (p -> q) where
 fromLabel = \_ -> fromString (symbolVal (Proxy @symb))

tests :: TestTree
tests = testGroup "Test.Sanity.NoIsLabel" [
      testCase "Data.ProtoLens.Labels not in scope" thisShouldCompile
    ]

thisShouldCompile :: Assertion
thisShouldCompile =
    assertEqual "" "hi" $ f 5
  where
    f :: Int -> String
    f = #hi

