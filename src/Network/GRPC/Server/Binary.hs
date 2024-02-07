-- | Convenience functions for working with binary RPC
--
-- Intended for qualified import.
--
-- import Network.GRPC.Server.Binary qualified as Binary
module Network.GRPC.Server.Binary (
    -- | Convenience wrappers using @binary@ for serialization/deserialization
    sendOutput
  , sendFinalOutput
  , recvInput
  , recvFinalInput
    -- * Streaming types
  , mkNonStreaming
  , mkClientStreaming
  , mkServerStreaming
  , mkBiDiStreaming
  ) where

import Control.Monad.Catch
import Data.Binary
import GHC.Stack

import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Common.StreamType
import Network.GRPC.Server (Call)
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.StreamType qualified as StreamType

{-------------------------------------------------------------------------------
  Convenience wrapers using @binary@ for serialization/deserialization
-------------------------------------------------------------------------------}

sendOutput ::
     Binary a
  => Call (BinaryRpc serv meth)
  -> StreamElem [CustomMetadata] a
  -> IO ()
sendOutput call out =
    Server.sendOutput call (encode <$> out)

sendFinalOutput ::
     Binary a
  => Call (BinaryRpc serv meth)
  -> (a, [CustomMetadata])
  -> IO ()
sendFinalOutput call (out, trailers) =
    Server.sendFinalOutput call (encode out, trailers)

recvInput ::
     (Binary a, HasCallStack)
  => Call (BinaryRpc serv meth)
  -> IO (StreamElem NoMetadata a)
recvInput call = do
    Server.recvInput call >>= traverse decodeOrThrow

recvFinalInput ::
     (Binary a, HasCallStack)
  => Call (BinaryRpc serv meth)
  -> IO a
recvFinalInput call =
     Server.recvFinalInput call >>= decodeOrThrow

{-------------------------------------------------------------------------------
  Handlers for specific streaming types
-------------------------------------------------------------------------------}

mkNonStreaming :: forall m inp out serv meth.
     (MonadThrow m, Binary inp, Binary out)
  => (    inp
       -> m out
     )
  -> NonStreamingHandler m (BinaryRpc serv meth)
mkNonStreaming f = StreamType.mkNonStreaming $ \inp -> do
    inp' <- decodeOrThrow inp
    encode <$> f inp'

mkClientStreaming :: forall m out serv meth.
     (MonadThrow m, Binary out)
  => (    (forall inp. Binary inp => m (StreamElem NoMetadata inp))
       -> m out
     )
  -> ClientStreamingHandler m (BinaryRpc serv meth)
mkClientStreaming f = StreamType.mkClientStreaming $ \recv -> do
    out <- f (recv >>= traverse decodeOrThrow)
    return $ encode out

mkServerStreaming :: forall m inp serv meth.
     (MonadThrow m, Binary inp)
  => (    inp
       -> (forall out. Binary out => out -> m ())
       -> m ()
     )
  -> ServerStreamingHandler m (BinaryRpc serv meth)
mkServerStreaming f = StreamType.mkServerStreaming $ \inp send -> do
    inp' <- decodeOrThrow inp
    f inp' (send . encode)

mkBiDiStreaming :: forall m serv meth.
     MonadThrow m
  => (    (forall inp. Binary inp => m (StreamElem NoMetadata inp))
       -> (forall out. Binary out => out -> m ())
       -> m ()
     )
  -> BiDiStreamingHandler m (BinaryRpc serv meth)
mkBiDiStreaming f = StreamType.mkBiDiStreaming $ \recv send ->
    f (recv >>= traverse decodeOrThrow) (send . encode)

