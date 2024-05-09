module Test.Stress.Client (client) where

import Data.ByteString.Lazy.Char8 qualified as BS.Char8

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem

import Test.Stress.Server.API

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

client :: IO ()
client =
    withConnection def server $ \conn ->
      serverStreaming conn 3_000_000
  where
    server :: Server
    server = ServerInsecure address

    address :: Address
    address = Address {
          addressHost      = "127.0.0.1"
        , addressPort      = defaultInsecurePort
        , addressAuthority = Nothing
        }

-- | Server streaming
--
-- Client tells the server how many messages to send. Server sends a sequence of
-- numbers up to @numMessages@, with all even numbers replaced by the number 1.
serverStreaming :: Connection -> Word -> IO ()
serverStreaming conn numMessages =
    withRPC conn def (Proxy @ManyServerStreaming) $ \call -> do
      sendFinalInput call (BS.Char8.pack $ show numMessages)
      recvAll call
  where
    recvAll :: Call ManyServerStreaming -> IO ()
    recvAll call = do
      received <- StreamElem.value <$> recvOutput call
      case received of
        Just _ ->
          recvAll call
        Nothing ->
          return ()
