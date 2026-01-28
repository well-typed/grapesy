-- | Run the interop tests against itself
module Interop.SelfTest (selfTest) where

import Network.GRPC.Server.Run

import Interop.Client (runInteropClient)
import Interop.Client.Ping
import Interop.Cmdline
import Interop.Server (withInteropServer)

selfTest :: Cmdline -> IO ()
selfTest cmdline = do
    -- Start the server
    withInteropServer cmdline $ \server -> do

      -- Ask the server for its port
      port <- getServerPort server
      let cmdline' = cmdline{cmdMode = Client, cmdPortOverride = Just port}

      -- Give the server a chance to get ready
      --
      -- We could configure the client to automatically reconnect, but this
      -- changes the semantics of doing RPCs (and in a way that the spec says
      -- should /not/ be done by default), so we prefer to do things this way.
      waitReachable cmdline'

      -- Run the client; when the client terminates, we're done
      runInteropClient cmdline'
