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
    -- * Streaming types
  , mkNonStreaming
  , mkClientStreaming
  , mkServerStreaming
  , mkBiDiStreaming
  ) where

import Control.Monad.IO.Class
import Data.Binary
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import GHC.Stack

import Network.GRPC.Common
import Network.GRPC.Common.Binary (decodeOrThrow)
import Network.GRPC.Common.StreamType
import Network.GRPC.Server (Call)
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.StreamType qualified as StreamType

{-------------------------------------------------------------------------------
  Convenience wrapers using @binary@ for serialization/deserialization
-------------------------------------------------------------------------------}

sendOutput ::
     (Binary out, Output rpc ~ Lazy.ByteString, HasCallStack)
  => Call rpc
  -> StreamElem [CustomMetadata] out
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
  -> (out, [CustomMetadata])
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

{-------------------------------------------------------------------------------
  Handlers for specific streaming types
-------------------------------------------------------------------------------}

mkNonStreaming :: forall m inp out rpc.
     ( SupportsStreamingType rpc NonStreaming
     , Binary inp, Input  rpc ~ Lazy.ByteString
     , Binary out, Output rpc ~ Lazy.ByteString
     , MonadIO m
     )
  => (    inp
       -> m out
     )
  -> NonStreamingHandler m rpc
mkNonStreaming f = StreamType.mkNonStreaming $ \inp -> do
    inp' <- decodeOrThrow inp
    encode <$> f inp'

mkClientStreaming :: forall m out rpc.
     ( SupportsStreamingType rpc ClientStreaming
     , Binary out, Output rpc ~ Lazy.ByteString
     , MonadIO m
     )
  => (    (forall inp.
                (Binary inp, Input rpc ~ Lazy.ByteString)
             => m (StreamElem NoMetadata inp)
          )
       -> m out
     )
  -> ClientStreamingHandler m rpc
mkClientStreaming f = StreamType.mkClientStreaming $ \recv -> do
    out <- f (recv >>= traverse decodeOrThrow)
    return $ encode out

mkServerStreaming :: forall m inp rpc.
     ( SupportsStreamingType rpc ServerStreaming
     , Binary inp, Input rpc ~ Lazy.ByteString
     , MonadIO m
     )
  => (    inp
       -> (forall out.
                (Binary out, Output rpc ~ Lazy.ByteString)
             => out -> m ()
          )
       -> m ()
     )
  -> ServerStreamingHandler m rpc
mkServerStreaming f = StreamType.mkServerStreaming $ \inp send -> do
    inp' <- decodeOrThrow inp
    f inp' (send . encode)

mkBiDiStreaming :: forall m rpc.
     ( SupportsStreamingType rpc BiDiStreaming
     , MonadIO m
     )
  => (    (forall inp.
                (Binary inp, Input rpc ~ Lazy.ByteString)
             => m (StreamElem NoMetadata inp)
          )
       -> (forall out.
                (Binary out, Output rpc ~ Lazy.ByteString)
             => out -> m ()
          )
       -> m ()
     )
  -> BiDiStreamingHandler m rpc
mkBiDiStreaming f = StreamType.mkBiDiStreaming $ \recv send ->
    f (recv >>= traverse decodeOrThrow) (send . encode)
