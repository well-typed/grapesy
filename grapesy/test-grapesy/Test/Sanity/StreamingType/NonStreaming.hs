{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sanity.StreamingType.NonStreaming (tests) where

import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common
import Network.GRPC.Common.Binary (RawRpc)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server (ContentType(..))
import Network.GRPC.Server.StreamType qualified as Server
import Network.GRPC.Server.StreamType.Binary qualified as Binary

import Test.Driver.ClientServer

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
                        clientContentType = InvalidOverride . Just $
                          ContentTypeOverride "application/invalid-subtype"
                      }

                  -- gRPC spec does not allow parameters
                , testCase "charset" $
                    test_increment def {
                        clientContentType = InvalidOverride . Just $
                          ContentTypeOverride "application/grpc; charset=utf-8"
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

type BinaryIncrement = RawRpc "binary" "increment"

type instance RequestMetadata          BinaryIncrement = NoMetadata
type instance ResponseInitialMetadata  BinaryIncrement = NoMetadata
type instance ResponseTrailingMetadata BinaryIncrement = NoMetadata

test_increment :: ClientServerConfig -> IO ()
test_increment config = testClientServer $ ClientServerTest {
      config
    , client = simpleTestClient $ \conn -> do
        Client.withRPC conn def (Proxy @BinaryIncrement) $ \call -> do
          Binary.sendFinalInput @Word8 call 1
          resp <- fst <$> Binary.recvFinalOutput @Word8 call
          assertEqual "" 2 $ resp
    , server = [
         Server.fromMethod @BinaryIncrement $ Binary.mkNonStreaming $ \n ->
           return (succ (n :: Word8))
        ]
    }
