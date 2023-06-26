-- | Convenience functions for working with binary RPC
--
-- Intended for qualified import.
--
-- import Network.GRPC.Client.Binary qualified as Binary
module Network.GRPC.Client.Binary (
    -- | Convenience wrappers using @binary@ for serialization/deserialization
    sendInput
  , sendFinalInput
  , recvOutput
  , recvFinalOutput
  ) where

import Control.Concurrent.STM
import Data.Binary

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client (Call, CustomMetadata)
import Network.GRPC.Common.StreamElem
import Network.GRPC.Common.Binary

{-------------------------------------------------------------------------------
  Convenience wrappers using @binary@ for serialization/deserialization

  We do /not/ wrap the client handlers here, because they are not a good match.
  The standard client streaming handlers expect a /single/ IO action that
  produces all inputs and/or a single IO action that handles all outputs, but
  the raw binary protocol allows message types to be different at each point in
  the communication.
-------------------------------------------------------------------------------}

sendInput ::
     Binary inp
  => Call (BinaryRpc serv meth)
  -> StreamElem () inp
  -> IO ()
sendInput call inp = atomically $
    Client.sendInput call (encode <$> inp)

sendFinalInput ::
     Binary inp
  => Call (BinaryRpc serv meth)
  -> inp
  -> IO ()
sendFinalInput call inp =
   Client.sendFinalInput call (encode inp)

recvOutput ::
     Binary out
  => Call (BinaryRpc serv meth)
  -> IO (StreamElem [CustomMetadata] out)
recvOutput call =
    atomically $ Client.recvOutput call >>= traverse decodeOrThrow

recvFinalOutput ::
     Binary out
  => Call (BinaryRpc serv meth)
  -> IO (out, [CustomMetadata])
recvFinalOutput call = do
    (out, md) <- Client.recvFinalOutput call
    (, md) <$> decodeOrThrow out
