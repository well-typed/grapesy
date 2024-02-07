-- | Convenience functions for working with binary RPC
--
-- Intended for qualified import.
--
-- import Network.GRPC.Client.Binary qualified as Binary
module Network.GRPC.Client.Binary (
    -- * Convenience wrappers using @binary@ for serialization/deserialization
    sendInput
  , sendFinalInput
  , recvOutput
  , recvFinalOutput
  ) where

import Control.Monad.IO.Class
import Data.Binary

import Network.GRPC.Client (Call)
import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Binary

{-------------------------------------------------------------------------------
  Convenience wrappers using @binary@ for serialization/deserialization
-------------------------------------------------------------------------------}

sendInput :: forall inp serv meth m.
     (Binary inp, MonadIO m)
  => Call (BinaryRpc serv meth)
  -> StreamElem NoMetadata inp
  -> m ()
sendInput call inp = Client.sendInput call (encode <$> inp)

sendFinalInput :: forall inp serv meth m.
     (Binary inp, MonadIO m)
  => Call (BinaryRpc serv meth)
  -> inp
  -> m ()
sendFinalInput call inp =
   Client.sendFinalInput call (encode inp)

recvOutput :: forall out serv meth m.
     (Binary out, MonadIO m)
  => Call (BinaryRpc serv meth)
  -> m (StreamElem [CustomMetadata] out)
recvOutput call = liftIO $
     Client.recvOutput call >>= traverse decodeOrThrow

recvFinalOutput :: forall out serv meth m.
     (Binary out, MonadIO m)
  => Call (BinaryRpc serv meth)
  -> m (out, [CustomMetadata])
recvFinalOutput call = liftIO $ do
    (out, md) <- Client.recvFinalOutput call
    (, md) <$> decodeOrThrow out

