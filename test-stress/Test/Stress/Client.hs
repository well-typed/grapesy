module Test.Stress.Client (client) where

import Control.Exception (throwIO)
import Control.Monad
import Data.IORef
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ProtoLens.Service.Types (StreamingType(..))
import System.IO.Unsafe (unsafePerformIO)

import Network.GRPC.Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem

import Test.Stress.Cmdline
import Test.Stress.Common
import Test.Stress.Server.API

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

client :: Bool -> Test -> IO ()
client reconnect test =
    case test of
      ManyCalls n ->
        withConnection params server $ \conn ->
          forM_ [1 .. n] $ \i ->
            singleNonStreaming conn i
      ManyConnections n ->
        forM_ [1 .. n] $ \i ->
          withConnection params server $ \conn ->
            singleNonStreaming conn i
      ManyMessages numMessages ClientStreaming ->
          withConnection params server $ \conn ->
            clientStreaming conn numMessages
      ManyMessages numMessages ServerStreaming ->
          withConnection params server $ \conn ->
            serverStreaming conn numMessages
      ManyMessages numMessages BiDiStreaming ->
          withConnection params server $ \conn ->
            biDiStreaming conn numMessages
      ManyMessages _ NonStreaming ->
          putStrLn
            "Use many-calls or many-connections for non-streaming stress tests"
  where
    params :: ConnParams
    params = if reconnect then reconnectParams else def

    reconnectParams :: ConnParams
    reconnectParams =
        def
          { connReconnectPolicy =
              let reconnectPolicy =
                    ReconnectAfter $ do
                      attempts <-
                        atomicModifyIORef' reconnectAttempts (\n -> (n + 1, n))
                      when (attempts `mod` 10 == 0) $
                        putStrLn $ "Reconnect attempt " ++ show attempts
                      return reconnectPolicy
               in reconnectPolicy
          }

    {-# NOINLINE reconnectAttempts #-}
    reconnectAttempts :: IORef Word
    reconnectAttempts = unsafePerformIO $ newIORef 1

    server :: Server
    server = ServerInsecure address

    address :: Address
    address = Address {
          addressHost      = "127.0.0.1"
        , addressPort      = defaultInsecurePort
        , addressAuthority = Nothing
        }

{-------------------------------------------------------------------------------
  Specific RPCs
-------------------------------------------------------------------------------}

-- | One non-streaming, round-trip call
singleNonStreaming :: Connection -> Word -> IO ()
singleNonStreaming conn n =
    withRPC conn def (Proxy @ManyNonStreaming) $ \call -> do
      Binary.sendFinalInput call n
      m <- fst <$> Binary.recvFinalOutput @Word call
      unless (m == succ n) $
        throwIO . userError $ concat [
            "Unexpected " ++ show m ++ "; "
          , "expected " ++ show (succ n)
          ]

-- | Client streaming
--
-- Client sends the server @numMessages@. Server responds with how many it
-- received.
clientStreaming :: Connection -> Word -> IO ()
clientStreaming conn numMessages =
    withRPC conn def (Proxy @ManyClientStreaming) $ \call -> do
      forM_ [1 .. (numMessages - 1)] $ \n ->
        Binary.sendNextInput call (show n)
      Binary.sendFinalInput call (show numMessages)
      n <- fst <$> Binary.recvFinalOutput @Word call
      unless (n == numMessages) $
        throwIO . userError $ concat [
            "Unexpected " ++ show n ++ "; "
          , "expected " ++ show numMessages
          ]

-- | Server streaming
--
-- Client tells the server how many messages to send. Server sends a sequence of
-- numbers up to @numMessages@, with all even numbers replaced by the number 1.
serverStreaming :: Connection -> Word -> IO ()
serverStreaming conn numMessages =
    withRPC conn def (Proxy @ManyServerStreaming) $ \call -> do
      Binary.sendFinalInput call numMessages
      recvAll call
  where
    recvAll :: Call ManyServerStreaming -> IO ()
    recvAll call =
      StreamElem.value <$> Binary.recvOutput @Lazy.ByteString call >>=
        \case
          Just _ ->
            recvAll call
          Nothing ->
            return ()

-- | Bidirectional streaming
--
-- Client and server take turns computing a Collatz sequence until the client
-- has received @numMessages@.
biDiStreaming :: Connection -> Word -> IO ()
biDiStreaming conn numMessages =
    withRPC conn def (Proxy @ManyBiDiStreaming) $ \call -> do
      -- Arbitrarily start the sequence at the number of messages
      Binary.sendNextInput call numMessages
      loop call (collatz numMessages) numMessages
  where
    loop call expect n =
        StreamElem.value <$> Binary.recvOutput @Word call >>=
          \case
            Just m
              | m /= expect ->
                  throwIO . userError $ concat [
                      "Unexpected " ++ show n ++ "; "
                    , "expected " ++ show expect
                    ]
              | otherwise -> do
                  let next = collatz m
                  sendNext next
                  loop call (collatz next) (n - 1)
            Nothing ->
              return ()
      where
        sendNext inp
            | n == 0
            = Binary.sendInput @Word call (StreamElem.NoMoreElems NoMetadata)
            | otherwise
            = Binary.sendNextInput call inp
