{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.ClientServer (
    -- * Basic client-server test
    ClientServerTest(..)
  , testClientServer
  , propClientServer
    -- * Re-exports
  , module Test.Util.ClientServer
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Default
import Test.QuickCheck.Monadic qualified as QuickCheck
import Test.Tasty.QuickCheck qualified as QuickCheck

import Network.GRPC.Client qualified as Client
import Network.GRPC.Server qualified as Server

import Test.Util.ClientServer

{-------------------------------------------------------------------------------
  Basic client-server test
-------------------------------------------------------------------------------}

data ClientServerTest = ClientServerTest {
      config :: ClientServerConfig
    , client :: Client.Connection -> IO ()
    , server :: [Server.RpcHandler IO]
    }

instance Default ClientServerTest where
  def = ClientServerTest {
        config = def
      , client = \_ -> return ()
      , server = []
      }

-- | Run client server test, and check for expected failures
testClientServer ::
    (forall a. Show a => (ClientServerTest -> IO a) -> IO a)
  -> IO String
testClientServer withTest =
    withTest $ \ClientServerTest{config, client, server} -> do
      mRes <- try $ runTestClientServer config client server
      case mRes of
        Right () -> return ""
        Left err -> case isExpectedException config err of
                      Just err' -> return $ "Got expected error: " ++ show err'
                      Nothing   -> throwIO err -- test failure

-- | Turn client server test into property
--
-- This does /not/ test for expected failures: we're not testing invalid
-- configurations when doing property based testing.
propClientServer ::
    (forall a. Show a => (ClientServerTest -> IO a) -> IO a)
  -> QuickCheck.Property
propClientServer withTest = QuickCheck.monadicIO $
    liftIO $ withTest $ \ClientServerTest{config, client, server} ->
      runTestClientServer config client server