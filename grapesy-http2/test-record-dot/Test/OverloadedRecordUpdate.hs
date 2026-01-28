{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

-- For now (ghc 9.2 .. 9.10) this is required if using OverloadedRecordUpdate
{-# LANGUAGE RebindableSyntax #-}

module Test.OverloadedRecordUpdate (tests) where

import Prelude

import Network.GRPC.Common.Protobuf

import Test.Tasty
import Test.Tasty.HUnit

import Proto.Spec

#if MIN_VERSION_base(4,22,0)
import GHC.Records.Compat (getField)
import GHC.Records.Compat qualified as Compat

-- See <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0583-hasfield-redesign.rst>
setField :: forall fld a r. Compat.HasField fld r a => a -> r -> r
setField a r = Compat.setField @fld @r @a r a
#else
import GHC.Records.Compat (getField, setField)
#endif


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