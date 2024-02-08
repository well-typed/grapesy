{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.ClientServer (
    -- * Basic client-server test
    ClientServerTest(..)
  , testClientServer
  , propClientServer
  , noCustomExceptions
    -- * Re-exports
  , module Test.Util.ClientServer
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Void
import Test.QuickCheck.Monadic qualified as QuickCheck
import Test.Tasty.QuickCheck qualified as QuickCheck
import Text.Show.Pretty

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Server qualified as Server

import Test.Util.ClientServer

{-------------------------------------------------------------------------------
  Basic client-server test
-------------------------------------------------------------------------------}

data ClientServerTest = ClientServerTest {
      config :: ClientServerConfig
    , client :: (forall a. (Client.Connection -> IO a) -> IO a) -> IO ()
    , server :: [Server.RpcHandler IO]
    }

instance Default ClientServerTest where
  def = ClientServerTest {
        config = def
      , client = \_ -> return ()
      , server = []
      }

-- | Run client server test, and check for expected failures
testClientServer :: (Show e, PrettyVal e)
  => (SomeException -> Maybe e)
  -- ^ Check if an exception was expected
  --
  -- See 'noCustomExceptions' if there are expected (test-specific) exceptions.
  -> ClientServerTest
  -> IO String
testClientServer assessCustomException
                 ClientServerTest{config, client, server} = do
    mRes <- try $ runTestClientServer config client server
    case mRes of
      Right () -> return ""
      Left err ->
        case isExpectedException config assessCustomException err of
          Right err' -> return $ "Got expected error: " ++ show err'
          Left  err' -> throwIO err' -- test failure

-- | Turn client server test into property
propClientServer ::
     (SomeException -> Maybe e)
  -> IO ClientServerTest
  -> QuickCheck.Property
propClientServer assessCustomException mkTest =
    QuickCheck.monadicIO $ liftIO $ do
      ClientServerTest{config, client, server} <- mkTest
      mRes <- try $ runTestClientServer config client server
      case mRes of
        Right () -> return ()
        Left err ->
          -- We cannot report information about expected failures during
          -- property based testing
          case isExpectedException config assessCustomException err of
            Right _    -> return ()
            Left  err' -> throwIO err'

noCustomExceptions :: SomeException -> Maybe Void
noCustomExceptions _ = Nothing