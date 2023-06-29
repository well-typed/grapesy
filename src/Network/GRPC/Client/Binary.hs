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

import Network.GRPC.Client (Call)
import Network.GRPC.Client qualified as Client
import Network.GRPC.Common.Binary
import Network.GRPC.Common.CustomMetadata
import Network.GRPC.Common.StreamElem

{-------------------------------------------------------------------------------
  Convenience wrappers using @binary@ for serialization/deserialization

  We do /not/ wrap the client handlers here, because they are not a good match.
  The standard client streaming handlers expect a /single/ IO action that
  produces all inputs and/or a single IO action that handles all outputs, but
  the raw binary protocol allows message types to be different at each point in
  the communication.

  These functions all have the type of the value sent or received as the
  /first/ argument, to facilitate the use of type arguments.
-------------------------------------------------------------------------------}

sendInput :: forall inp serv meth.
     Binary inp
  => Call (BinaryRpc serv meth)
  -> StreamElem () inp
  -> IO ()
sendInput call inp = atomically $
    Client.sendInput call (encode <$> inp)

sendFinalInput :: forall inp serv meth.
     Binary inp
  => Call (BinaryRpc serv meth)
  -> inp
  -> IO ()
sendFinalInput call inp =
   Client.sendFinalInput call (encode inp)

recvOutput :: forall out serv meth.
     Binary out
  => Call (BinaryRpc serv meth)
  -> IO (StreamElem [CustomMetadata] out)
recvOutput call =
    atomically $ Client.recvOutput call >>= traverse decodeOrThrow

recvFinalOutput :: forall out serv meth.
     Binary out
  => Call (BinaryRpc serv meth)
  -> IO (out, [CustomMetadata])
recvFinalOutput call = do
    (out, md) <- Client.recvFinalOutput call
    (, md) <$> decodeOrThrow out
