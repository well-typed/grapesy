module Test.Stress.Client (client) where

-- TODO: <https://github.com/well-typed/grapesy/issues/60>
--
-- We should stress test a scenario where there is many reconnections.

import Control.Exception
import Control.Monad

import Network.GRPC.Client
import Network.GRPC.Client.Binary qualified as Binary
import Network.GRPC.Common

import Test.Stress.Cmdline
import Test.Stress.Server.API

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

client :: Cmdline -> Test -> IO ()
client _cmdline test =
    case test of
      ManyCalls n ->
        withConnection params server $ \conn ->
          forM_ [1 .. n] $ \i ->
            singleNonStreaming conn i
      ManyConnections n ->
        forM_ [1 .. n] $ \i ->
          withConnection params server $ \conn ->
            singleNonStreaming conn i
      ManyMessages _n _streamingType ->
        -- TODO: <https://github.com/well-typed/grapesy/issues/60>
        -- Need to implement the many-messages stress test
        putStrLn "Not implemented"
  where
    params :: ConnParams
    params = def

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
    withRPC conn def (Proxy @ManyShortLived) $ \call -> do
      Binary.sendFinalInput call n
      m <- fst <$> Binary.recvFinalOutput @Word call
      unless (m == succ n) $
        throwIO . userError $ concat [
            "Unexpected " ++ show m ++ "; "
          , "expected " ++ show (succ n)
          ]
