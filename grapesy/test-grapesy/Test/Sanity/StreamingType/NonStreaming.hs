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
import Data.Foldable (toList)

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
              testGroup "supported" $
                let mkTest :: Compr.Compression -> TestTree
                    mkTest compr = testCase comprId $
                        test_increment def {
                            clientCompr = Compr.only compr
                          , serverCompr = Compr.only compr
                          }
                      where
                        comprId :: String
                        comprId = show (Compr.compressionId compr)
                in map mkTest (toList Compr.allSupportedCompression)
            , testGroup "unsupported" [
                  testCase "clientChoosesUnsupported" $
                    test_increment def {
                        clientInitCompr = Just Compr.gzip
                      , serverCompr     = Compr.none
                      }
                , testCase "serverChoosesUnsupported" $
                    test_increment def {
                        clientCompr = Compr.none
                      , serverCompr = Compr.insist Compr.gzip
                      }
                ]
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
