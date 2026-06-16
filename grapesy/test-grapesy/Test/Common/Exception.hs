module Test.Common.Exception (tests) where

import Control.Exception (SomeException (..))
import Test.Tasty
import Test.Tasty.HUnit
import System.ThreadManager qualified as ThreadManager

import Network.GRPC.Common.Exception

tests :: TestTree
tests = testGroup "Test.Common.Exception"
    [ testCase "thread-manager" $ do
        let ex = ThreadManager.KilledByThreadManager $ Just $ SomeException $ userError "foo"
        renderException ex @?= unlines
            [ "KilledByThreadManager"
            , "  IOException"
            , "    user error (foo)"
            ]
    ]
