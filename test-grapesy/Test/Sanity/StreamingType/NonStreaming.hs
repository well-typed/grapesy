module Test.Sanity.StreamingType.NonStreaming (tests) where

import Data.Default
import Data.Proxy
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.ClientServer

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common.Binary (BinaryRpc)
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Binary
import Network.GRPC.Server.StreamType

tests :: TestTree
tests = testGroup "Test.Sanity.StreamingType.NonStreaming" [
      testGroup "binary" [
          testCase "increment" test_binary_increment
        ]
    ]

{-------------------------------------------------------------------------------
  Binary (without Protobuf)
-------------------------------------------------------------------------------}

type BinaryIncrement = BinaryRpc "binary" "increment"

test_binary_increment :: Assertion
test_binary_increment = do
    res <- testClientServer def client server
    assertEqual "" res 2
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
