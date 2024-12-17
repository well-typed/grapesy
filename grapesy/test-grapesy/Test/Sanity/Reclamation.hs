module Test.Sanity.Reclamation (tests) where

import Control.Exception
import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server qualified as Server

import Test.Driver.ClientServer

import Proto.API.Ping

tests :: TestTree
tests = testGroup "Test.Sanity.Reclamation" [
      testCase "serverException1" serverException1
    , testCase "serverException2" serverException2
    ]

{-------------------------------------------------------------------------------
  Server-side exception

  Test for <https://github.com/well-typed/grapesy/issues/257>.
-------------------------------------------------------------------------------}

-- | Handler that throws immediately
brokenHandler :: Server.Call Ping -> IO ()
brokenHandler _call = throwIO $ DeliberateException $ userError "Broken handler"

serverException1 :: Assertion
serverException1 = testClientServer $ ClientServerTest {
      config = def
    , server = [Server.someRpcHandler $ Server.mkRpcHandler brokenHandler]
    , client = \params testServer delimitTestScope -> delimitTestScope $
        replicateM_ 1000 $ do
          Client.withConnection params testServer $ \conn ->
            Client.withRPC conn def (Proxy @Ping) $ \call -> do
              resp <- try $ Client.recvFinalOutput call
              case resp of
                Left GrpcException{} -> return ()
                Right _ -> assertFailure "Unexpected response"
    }

serverException2 :: Assertion
serverException2 = testClientServer $ ClientServerTest {
      config = def
    , server = [Server.someRpcHandler $ Server.mkRpcHandler brokenHandler]
    , client = \params testServer delimitTestScope -> delimitTestScope $
        replicateM_ 1000 $
          Client.withConnection params testServer $ \conn ->
            Client.withRPC conn def (Proxy @Ping) $ \call -> do

              -- The only difference between serverException1 is this line:
              Client.sendFinalInput call defMessage

              resp <- try $ Client.recvFinalOutput call
              case resp of
                Left GrpcException{} -> return ()
                Right _ -> assertFailure "Unexpected response"
    }
