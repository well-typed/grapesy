{-# LANGUAGE OverloadedLabels #-}

module Interop.Client.Ping (ping) where

import Control.Concurrent
import Control.Monad

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO
import Network.GRPC.Common.Protobuf

import Proto.Ping

import Interop.Client.Connect
import Interop.Cmdline

ping :: Cmdline -> IO ()
ping cmdline = connect cmdline $ \conn ->
    forM_ [1..] $ \i -> do
      let msgPing :: PingMessage
          msgPing = defMessage & #id .~ i
      msgPong <- nonStreaming conn (rpc @(Protobuf PingService "ping")) msgPing
      print msgPong
      threadDelay 1_000_000

