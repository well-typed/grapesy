{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

-- For now (ghc 9.2 .. 9.10) this is required if using OverloadedRecordUpdate
{-# LANGUAGE RebindableSyntax #-}

module Test.OverloadedRecordUpdate (tests) where

import Prelude
import GHC.Records.Compat

import Network.GRPC.Common.Protobuf

import Test.Tasty
import Test.Tasty.HUnit

import Proto.Spec

tests :: TestTree
tests = testGroup "Test.OverloadedRecordUpdate" [
      testCase "update" test_update
    ]

test_update :: Assertion
test_update = do
    do let msg' = exampleMessage{defaultScalar01 = 1}
       assertEqual "defaultScalar01" 1 $ msg'.defaultScalar01

exampleMessage :: Proto ExampleMessage
exampleMessage = defMessage