module Test.Common.Exception (tests) where

import Control.Exception (SomeException (..))
import Test.Tasty
import Test.Tasty.QuickCheck
import System.ThreadManager qualified as ThreadManager
import Data.TreeDiff.QuickCheck (ediffEq)

import Network.GRPC.Common.Exception

tests :: TestTree
tests = testGroup "Test.Common.Exception"
    [ testProperty "thread-manager-1" $ do
        let expected = unlines
                [ "KilledByThreadManager"
                , "  IOException"
                , "    user error (foo)"
                ]

        let exc = ThreadManager.KilledByThreadManager $ Just $ SomeException $ userError "foo"
        ediffEq expected (renderException defaultFormatCtx exc)
    ]
