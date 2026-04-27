module Test.Disconnect.Echo.RPC (
    Echo
  , Echo1
  , Echo2
  ) where

import Proto.API.Trivial

type Echo rpc = Trivial' rpc

type Echo1 = Echo "rpc1"
type Echo2 = Echo "rpc2"

