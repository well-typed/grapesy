-- | Run the interop tests against itself
module SelfInterop (main) where

import Control.Concurrent
import Control.Monad
import System.Environment

import Interop.Client (client)
import Interop.Client.Ping
import Interop.Cmdline
import Interop.Server (server)

main :: IO ()
main = do
    -- Check that no command line flags were passed (to avoid confusion)
    args <- getArgs
    case args of
      []         -> return ()
      _otherwise -> fail "This test suite does not take any arguments"

    -- Construct defaults
    --
    -- TODO: Justify disabling TLS.
    defaults <- defaultCmdline
    let cmdline = defaults{cmdUseTLS = False}

    -- Start the server and give it a chance to get ready
    --
    -- We could configure the client to automatically reconnect, but this
    -- changes the semantics of doing RPCs (and in a way that the spec says
    -- should /not/ be done by default), so we prefer to do things this way.
    void $ forkIO $ server cmdline{cmdMode = Server}
    waitReachable cmdline

    -- Run the client; when the client terminates, we're done
    client cmdline{cmdMode = Client}

