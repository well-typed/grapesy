{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.ClientServer (
    -- * Basic client-server test
    ClientServerTest(..)
  , testClientServer
    -- * Re-exports
  , module Test.Util.ClientServer
  ) where

import Control.Exception
import Data.Default
import Data.Typeable
import Test.Tasty.HUnit

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

testClientServer :: (forall a. (ClientServerTest -> IO a) -> IO a) -> IO String
testClientServer withTest =
    withTest $ \ClientServerTest{config, client, server} -> do
      mRes <- try $ runTestClientServer config client server
      case mRes of
        Left err
          | Just (testFailure :: HUnitFailure) <- fromException err
          -> throwIO testFailure

          | isExpectedException config err
          -> return $ "Got expected error: " ++ show err

          | otherwise
          -> assertFailure $ concat [
                "Unexpected exception of type "
              , case err of
                  SomeException e -> show (typeOf e)
              , ": "
              , show err
              ]
        Right () ->
          return ""

