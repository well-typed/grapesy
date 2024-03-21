-- | Run the interop tests against itself
module Interop.SelfTest (selfTest) where

import Control.Concurrent
import Control.Monad

import Interop.Client (client)
import Interop.Client.Ping
import Interop.Cmdline
import Interop.Server (server)

selfTest :: Cmdline -> IO ()
selfTest cmdline = do
    -- Start the server and give it a chance to get ready
    --
    -- We could configure the client to automatically reconnect, but this
    -- changes the semantics of doing RPCs (and in a way that the spec says
    -- should /not/ be done by default), so we prefer to do things this way.
    void $ forkIO $ server cmdline{cmdMode = Server}
    waitReachable cmdline

    -- Run the client; when the client terminates, we're done
    client cmdline{cmdMode = Client}
