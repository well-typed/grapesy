{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.ClientServer (
    -- * Basic client-server test
    testClientServer
  , propClientServer
    -- * Re-exports
  , module Test.Util.ClientServer
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Test.QuickCheck.Monadic qualified as QuickCheck
import Test.Tasty.QuickCheck qualified as QuickCheck

import Test.Util.ClientServer

{-------------------------------------------------------------------------------
  Basic client-server test
-------------------------------------------------------------------------------}

-- | Run client server test, and check for expected failures
testClientServer :: ClientServerTest () -> IO String
testClientServer test = do
    mRes <- try $ runTestClientServer test
    case mRes of
      Right () -> return ""
      Left err ->
        case isExpectedException (config test) err of
          Right err' -> return $ "Got expected error: " ++ show err'
          Left  err' -> throwIO err' -- test failure

-- | Turn client server test into property
propClientServer :: IO (ClientServerTest ()) -> QuickCheck.Property
propClientServer mkTest =
    QuickCheck.monadicIO $ liftIO $ do
      test <- mkTest
      mRes <- try $ runTestClientServer test
      case mRes of
        Right () -> return ()
        Left err ->
          -- We cannot report information about expected failures during
          -- property based testing
          case isExpectedException (config test) err of
            Right _    -> return ()
            Left  err' -> throwIO err'