{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.ClientServer (
    -- * Basic client-server test
    testClientServer
  , propClientServer
    -- * Re-exports
  , module Test.Util.ClientServer
  ) where

import Control.Monad.IO.Class
import Test.QuickCheck.Monadic qualified as QuickCheck
import Test.Tasty.QuickCheck qualified as QuickCheck

import Test.Util.ClientServer

{-------------------------------------------------------------------------------
  Basic client-server test
-------------------------------------------------------------------------------}

-- | Run client server test, and check for expected failures
testClientServer :: ClientServerTest -> IO ()
testClientServer test =
    runTestClientServer test

-- | Turn client server test into property
propClientServer :: IO ClientServerTest -> QuickCheck.Property
propClientServer mkTest =
    QuickCheck.monadicIO $ liftIO $ runTestClientServer =<< mkTest
