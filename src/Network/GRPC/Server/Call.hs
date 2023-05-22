module Network.GRPC.Server.Call (
    Call -- opaque

    -- * Construction
  , withCall

    -- * Open (ongoing) call
  , recvInput
  , sendOutput
  , sendTrailers
  ) where

-- import Control.Concurrent.Async
import Control.Concurrent.STM
import Network.HTTP2.Server qualified as HTTP2

import Network.GRPC.Spec
import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Call rpc = Call

{-------------------------------------------------------------------------------
  Open a call
-------------------------------------------------------------------------------}

withCall :: HTTP2.Request -> (HTTP2.Response -> IO ()) -> (Call rpc -> IO r) -> IO r
withCall _req _respond k = do
    k Call

{-------------------------------------------------------------------------------
  Open (ongoing) call
-------------------------------------------------------------------------------}

recvInput :: Call rpc -> STM (IsFinal, Maybe (Input rpc))
recvInput = undefined

sendOutput :: Call rpc -> Output rpc -> STM ()
sendOutput = undefined

sendTrailers :: Call rpc -> Trailers -> STM ()
sendTrailers = undefined
