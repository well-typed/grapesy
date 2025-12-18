-- | Convenience functions for working with binary RPC
--
-- Intended for qualified import.
--
-- import Network.GRPC.Client.Binary qualified as Binary
module Network.GRPC.Client.Binary (
    -- * Convenience wrappers using @binary@ for serialization/deserialization
    sendInput
  , sendNextInput
  , sendFinalInput
  , recvOutput
  , recvNextOutput
  , recvFinalOutput
  ) where

import Network.GRPC.Util.Imports

import Data.Binary (Binary, encode)
import Data.ByteString.Lazy qualified as Lazy (ByteString)

import Network.GRPC.Client (Call)
import Network.GRPC.Client qualified as Client
import Network.GRPC.Common.StreamElem (StreamElem(..))
import Network.GRPC.Common.Binary (decodeOrThrow)

{-------------------------------------------------------------------------------
  Convenience wrappers using @binary@ for serialization/deserialization
-------------------------------------------------------------------------------}

sendInput :: forall inp rpc m.
     (Binary inp, Input rpc ~ Lazy.ByteString, MonadIO m)
  => Call rpc
  -> StreamElem NoMetadata inp
  -> m ()
sendInput call inp = Client.sendInput call (encode <$> inp)

sendNextInput ::
     (Binary inp, Input rpc ~ Lazy.ByteString, MonadIO m)
  => Call rpc
  -> inp
  -> m ()
sendNextInput call inp = Client.sendNextInput call (encode inp)

sendFinalInput :: forall inp rpc m.
     (Binary inp, Input rpc ~ Lazy.ByteString, MonadIO m)
  => Call rpc
  -> inp
  -> m ()
sendFinalInput call inp =
   Client.sendFinalInput call (encode inp)

recvOutput :: forall out rpc m.
     (Binary out, Output rpc ~ Lazy.ByteString, MonadIO m)
  => Call rpc
  -> m (StreamElem (ResponseTrailingMetadata rpc) out)
recvOutput call =
     Client.recvOutput call >>= traverse decodeOrThrow

recvNextOutput ::
     (Binary out, Output rpc ~ Lazy.ByteString, MonadIO m)
  => Call rpc
  -> m out
recvNextOutput call = Client.recvNextOutput call >>= decodeOrThrow

recvFinalOutput :: forall out rpc m.
     (Binary out, Output rpc ~ Lazy.ByteString, MonadIO m)
  => Call rpc
  -> m (out, ResponseTrailingMetadata rpc)
recvFinalOutput call = do
    (out, md) <- Client.recvFinalOutput call
    (, md) <$> decodeOrThrow out

