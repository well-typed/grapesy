module Test.Sanity.StreamingType.NonStreaming (tests) where

import Control.Exception
import Data.Default
import Data.Proxy
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.ClientServer

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common.Binary (BinaryRpc)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Binary
import Network.GRPC.Server.StreamType

tests :: TestTree
tests = testGroup "Test.Sanity.StreamingType.NonStreaming" [
      testGroup "increment" [
          testCaseInfo "default" $
            test_increment def
        , testGroup "TLS" [
              testGroup "ok" [
                  testCaseInfo "certAsRoot" $
                    test_increment def {
                        useTLS = Just $ TlsOk TlsOkCertAsRoot
                      }
                , testCaseInfo "skipValidation" $
                    test_increment def {
                        useTLS = Just $ TlsOk TlsOkSkipValidation
                      }
                  ]
            , testGroup "fail" [
                  testCaseInfo "validation" $
                    test_increment def {
                        useTLS = Just $ TlsFail TlsFailValidation
                      }
                , testCaseInfo "hostname" $
                    test_increment def {
                        useTLS = Just $ TlsFail TlsFailHostname
                      }
                , testCaseInfo "unsupported" $
                    test_increment def {
                        useTLS = Just $ TlsFail TlsFailUnsupported
                      }
                ]
            ]
        , testCaseInfo "gzip" $
            test_increment def {
                clientCompr = Compr.require Compr.gzip
              , serverCompr = Compr.require Compr.gzip
              }
        ]
    ]

{-------------------------------------------------------------------------------
  Binary (without Protobuf)
-------------------------------------------------------------------------------}

type BinaryIncrement = BinaryRpc "binary" "increment"

test_increment :: ClientServerConfig -> IO String
test_increment cfg = do
    mRes <- try $ testClientServer cfg client server
    case mRes of
      Right res -> do
        assertEqual "" res 2
        return ""
      Left  err ->
          case isTestFailure cfg err of
            Just failure -> do
              assertFailure failure
            Nothing ->
              return $ "Got expected error: " ++ show err
  where
    client :: Client.Connection -> IO Word8
    client conn = Client.withRPC conn def (Proxy @BinaryIncrement) $ \call -> do
        Binary.sendFinalInput call (1 :: Word8)
        fst <$> Binary.recvFinalOutput call

    server :: [Server.RpcHandler IO]
    server = [
          streamingRpcHandler (Proxy @BinaryIncrement) $
            Binary.mkNonStreaming $ \(n :: Word8) ->
              return (succ n)
        ]
