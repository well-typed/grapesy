module Test.Disconnect.Echo.Client (
    fork_echoOnceThenWait
  , fork_countFrom
  ) where

import Control.Monad
import Data.Proxy
import Data.Word
import GHC.TypeLits

import Network.GRPC.Client        qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Common

import Test.Disconnect.Echo.RPC
import Test.Disconnect.Util.Client

{-------------------------------------------------------------------------------
  Test clients
-------------------------------------------------------------------------------}

-- | Echo once, then wait indefinitely for a server response that never comes
fork_echoOnceThenWait :: forall (rpc :: Symbol).
     KnownSymbol rpc
  => Client.Connection
  -> Proxy (Echo rpc)
  -> IO (ClientThread ())
fork_echoOnceThenWait conn _rpc =
    forkClientThread $ \markConnected _markResult -> do
      Client.withRPC conn def (Proxy @(Echo rpc)) $ \call -> do
        -- One round-trip
        Client.Binary.sendNextInput @Word64 call 0
        _resp <- Client.Binary.recvNextOutput @Word64 call
        markConnected

        -- The next call to 'recvNextOutput' will never return
        void $ Client.Binary.recvNextOutput @Word64 call

-- | Count down from the specified number, and sum all server responses
fork_countFrom :: forall (rpc :: Symbol).
     KnownSymbol rpc
  => Client.Connection
  -> Proxy (Echo rpc)
  -> Word64
  -> IO (ClientThread Word64)
fork_countFrom conn _rpc target =
    forkClientThread $ \markConnected markResult -> do
      Client.withRPC conn def (Proxy @(Echo rpc)) $ \call -> do
        let loop :: Word64 -> Word64 -> IO ()
            loop !acc 0 = do
                Client.sendEndOfInput call
                NoMetadata <- Client.recvTrailers call
                markResult acc
            loop !acc n = do
                Client.Binary.sendNextInput @Word64 call n
                resp <- Client.Binary.recvNextOutput @Word64 call
                markConnected
                loop (acc + resp) (n - 1)
        loop 0 target
