module Interop.Client.Ping (ping, waitReachable) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Timeout qualified as System

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Ping

import Interop.Client.Connect
import Interop.Cmdline

ping :: Cmdline -> IO ()
ping cmdline =
    withConnection def (testServer cmdline) $ \conn ->
      forM_ [1..] $ \i -> do
        let msgPing :: PingMessage
            msgPing = defMessage & #id .~ i
        msgPong <- nonStreaming conn (rpc @(Protobuf PingService "ping")) msgPing
        print msgPong
        threadDelay 1_000_000

waitReachable :: Cmdline -> IO ()
waitReachable cmdline = do
    result <- System.timeout (cmdConnectTimeout cmdline * 1_000_000) loop
    case result of
      Nothing -> fail "Failed to connect to the server"
      Just () -> return ()
  where
    loop :: IO ()
    loop = do
        result :: Either SomeException () <- try reach
        case result of
          Left ex ->
            case fromException ex of
              Just (_ :: System.Timeout) ->
                throwIO ex
              _otherwise -> do
                threadDelay 100_000
                loop
          Right _ ->
            return ()

    reach :: IO ()
    reach = do
        withConnection def (testServer cmdline) $ \conn -> void $
          nonStreaming conn (rpc @(Protobuf PingService "ping")) defMessage
