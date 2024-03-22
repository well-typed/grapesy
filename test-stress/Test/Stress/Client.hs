module Test.Stress.Client (client) where

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
client _cmdline test = do
    withConnection params server $ \conn ->
      case test of
        ManyShortLived ->
          clientManyShortLived conn
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

clientManyShortLived :: Connection -> IO ()
clientManyShortLived conn =
    forM_ [1 .. 5] $ \(i :: Word) -> do
      withRPC conn def (Proxy @ManyShortLived) $ \call -> do
        Binary.sendFinalInput call i
        n <- fst <$> Binary.recvFinalOutput call
        unless (n == succ i) $
          throwIO . userError $ concat [
              "Unexpected " ++ show n ++ "; "
            , "expected " ++ show (succ i)
            ]


