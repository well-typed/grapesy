-- | Convenience functions for working with binary RPC
--
-- Intended for qualified import.
--
-- import Network.GRPC.Server.Binary qualified as Binary
module Network.GRPC.Server.Binary (
    -- | Convenience wrappers using @binary@ for serialization/deserialization
    sendOutput
  , sendNextOutput
  , sendFinalOutput
  , recvInput
  , recvNextInput
  , recvFinalInput
  ) where

import Data.Binary
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import GHC.Stack

import Network.GRPC.Common
import Network.GRPC.Common.Binary (decodeOrThrow)
import Network.GRPC.Server (Call)
import Network.GRPC.Server qualified as Server

{-------------------------------------------------------------------------------
  Convenience wrapers using @binary@ for serialization/deserialization
-------------------------------------------------------------------------------}

sendOutput ::
     (Binary out, Output rpc ~ Lazy.ByteString, HasCallStack)
  => Call rpc
  -> StreamElem (ResponseTrailingMetadata rpc) out
  -> IO ()
sendOutput call out =
    Server.sendOutput call (encode <$> out)

sendNextOutput ::
     (Binary out, Output rpc ~ Lazy.ByteString, HasCallStack)
  => Call rpc
  -> out
  -> IO ()
sendNextOutput call out =
    Server.sendNextOutput call (encode out)

sendFinalOutput ::
     (Binary out, Output rpc ~ Lazy.ByteString, HasCallStack)
  => Call rpc
  -> (out, ResponseTrailingMetadata rpc)
  -> IO ()
sendFinalOutput call (out, trailers) =
    Server.sendFinalOutput call (encode out, trailers)

recvInput ::
     (Binary inp, Input rpc ~ Lazy.ByteString, HasCallStack)
  => Call rpc
  -> IO (StreamElem NoMetadata inp)
recvInput call =
    Server.recvInput call >>= traverse decodeOrThrow

recvNextInput ::
     (Binary inp, Input rpc ~ Lazy.ByteString, HasCallStack)
  => Call rpc
  -> IO inp
recvNextInput call =
    Server.recvNextInput call >>= decodeOrThrow

recvFinalInput ::
     (Binary inp, Input rpc ~ Lazy.ByteString, HasCallStack)
  => Call rpc
  -> IO inp
recvFinalInput call =
     Server.recvFinalInput call >>= decodeOrThrow
