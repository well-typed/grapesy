{-# LANGUAGE CPP               #-}
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
import Network.GRPC.Spec (ContentType(ContentTypeOverride))

tests :: TestTree
tests = testGroup "Test.Sanity.StreamingType.NonStreaming" [
      testGroup "increment" [
          testCase "default" $
            test_increment def
        , testGroup "Content-Type" [
              testGroup "ok" [
                  -- Without the +format part
                  testCase "application/grpc" $
                    test_increment def {
                        clientContentType = ValidOverride $
                          ContentTypeOverride "application/grpc"
                      }

                  -- Random other format
                  -- See discussion in 'parseContentType'
                , testCase "application/grpc+gibberish" $
                    test_increment def {
                        clientContentType = ValidOverride $
                          ContentTypeOverride "application/grpc+gibberish"
                      }
                ]
            , testGroup "fail" [
                  testCase "application/invalid-subtype" $
                    test_increment def {
                        clientContentType = InvalidOverride $
                           ContentTypeOverride "application/invalid-subtype"
                      }

                  -- gRPC spec does not allow parameters
                , testCase "charset" $
                    test_increment def {
                        clientContentType = InvalidOverride $
                           ContentTypeOverride "application/grpc; charset=us-ascii"
                      }
                ]
            ]
        , testGroup "TLS" [
              testGroup "ok" [
                  testCase "certAsRoot" $
                    test_increment def {
                        useTLS = Just $ TlsOk TlsOkCertAsRoot
                      }
                , testCase "skipValidation" $
                    test_increment def {
                        useTLS = Just $ TlsOk TlsOkSkipValidation
                      }
                  ]
            , testGroup "fail" [
                  testCase "validation" $
                    test_increment def {
                        useTLS = Just $ TlsFail TlsFailValidation
                      }
                , testCase "hostname" $
                    test_increment def {
                        useTLS = Just $ TlsFail TlsFailHostname
                      }
                , testCase "unsupported" $
                    test_increment def {
                        useTLS = Just $ TlsFail TlsFailUnsupported
                      }
                ]
            ]
        , testGroup "compression" [
              testCase "gzip" $
                test_increment def {
                    clientCompr = Compr.only Compr.gzip
                  , serverCompr = Compr.only Compr.gzip
                  }
            , testCase "deflate" $
                test_increment def {
                    clientCompr = Compr.only Compr.deflate
                  , serverCompr = Compr.only Compr.deflate
                  }
#ifdef SNAPPY
            , testCase "snappy" $
                test_increment def {
                    clientCompr = Compr.only Compr.snappy
                  , serverCompr = Compr.only Compr.snappy
                  }
#endif
            , testCase "clientChoosesUnsupported" $
                test_increment def {
                    clientInitCompr = Just Compr.gzip
                  , serverCompr     = Compr.none
                  }
            , testCase "serverChoosesUnsupported" $
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

test_increment :: ClientServerConfig -> IO ()
test_increment config = testClientServer $ ClientServerTest {
      config
    , client = simpleTestClient $ \conn -> do
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
