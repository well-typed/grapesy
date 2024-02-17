{-# LANGUAGE OverloadedStrings #-}

module Test.Sanity.StreamingType.NonStreaming (tests) where

import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common
import Network.GRPC.Common.Binary (BinaryRpc)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server.Binary qualified as Binary
import Network.GRPC.Server.StreamType

import Test.Driver.ClientServer

tests :: TestTree
tests = testGroup "Test.Sanity.StreamingType.NonStreaming" [
      testGroup "increment" [
          testCaseInfo "default" $
            test_increment def
        , testGroup "Content-Type" [
              testGroup "ok" [
                  -- Without the +format part
                  testCaseInfo "application/grpc" $
                    test_increment def {
                        clientContentType =
                          ValidOverride "application/grpc"
                      }

                  -- Random other format
                  -- See discussion in 'parseContentType'
                , testCaseInfo "application/grpc+gibberish" $
                    test_increment def {
                        clientContentType =
                          ValidOverride "application/grpc+gibberish"
                      }
                ]
            , testGroup "fail" [
                  testCaseInfo "application/invalid-subtype" $
                    test_increment def {
                        clientContentType =
                          InvalidOverride "application/invalid-subtype"
                      }

                  -- gRPC spec does not allow parameters
                , testCaseInfo "charset" $
                    test_increment def {
                        clientContentType =
                          InvalidOverride "application/grpc; charset=us-ascii"
                      }
                ]
            ]
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
        , testGroup "compression" [
              testCaseInfo "gzip" $
                test_increment def {
                    clientCompr = Compr.only Compr.gzip
                  , serverCompr = Compr.only Compr.gzip
                  }
            , testCaseInfo "deflate" $
                test_increment def {
                    clientCompr = Compr.only Compr.deflate
                  , serverCompr = Compr.only Compr.deflate
                  }
            , testCaseInfo "clientChoosesUnsupported" $
                test_increment def {
                    clientInitCompr = Just Compr.gzip
                  , serverCompr     = Compr.none
                  }
            , testCaseInfo "serverChoosesUnsupported" $
                test_increment def {
                    clientCompr = Compr.only   Compr.gzip
                  , serverCompr = Compr.insist Compr.deflate
                  }
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Binary (without Protobuf)
-------------------------------------------------------------------------------}

type BinaryIncrement = BinaryRpc "binary" "increment"

test_increment :: ClientServerConfig -> IO String
test_increment config = testClientServer $ ClientServerTest {
      config
    , client = \withConn -> withConn $ \conn -> do
        Client.withRPC conn def (Proxy @BinaryIncrement) $ \call -> do
          Binary.sendFinalInput @Word8 call 1
          resp <- fst <$> Binary.recvFinalOutput @Word8 call
          assertEqual "" 2 $ resp
    , server = [
          streamingRpcHandler (Proxy @BinaryIncrement) $
            Binary.mkNonStreaming $ \(n :: Word8) ->
              return (succ n)
        ]
    }
