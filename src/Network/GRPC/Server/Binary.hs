-- | Convenience functions for working with binary RPC
--
-- Intended for qualified import.
--
-- import Network.GRPC.Server.Binary qualified as Binary
module Network.GRPC.Server.Binary (
    -- * Streaming types
    mkNonStreaming
  , mkClientStreaming
  , mkServerStreaming
  , mkBiDiStreaming
  ) where

import Control.Monad.Catch
import Data.Binary

import Network.GRPC.Common.StreamType qualified as StreamType
import Network.GRPC.Common.Binary
import Network.GRPC.Common.StreamElem

{-------------------------------------------------------------------------------
  Handlers for specific streaming types
-------------------------------------------------------------------------------}

mkNonStreaming :: forall m inp out serv meth.
     (MonadThrow m, Binary inp, Binary out)
  => (    inp
       -> m out
     )
  -> StreamType.NonStreamingHandler m (BinaryRpc serv meth)
mkNonStreaming f = StreamType.mkNonStreaming $ \inp -> do
    inp' <- decodeOrThrow inp
    encode <$> f inp'

mkClientStreaming :: forall m out serv meth.
     (MonadThrow m, Binary out)
  => (    (forall inp. Binary inp => m (StreamElem () inp))
       -> m out
     )
  -> StreamType.ClientStreamingHandler m (BinaryRpc serv meth)
mkClientStreaming f = StreamType.mkClientStreaming $ \recv -> do
    out <- f (recv >>= traverse decodeOrThrow)
    return $ encode out

mkServerStreaming :: forall m inp serv meth.
     (MonadThrow m, Binary inp)
  => (    inp
       -> (forall out. Binary out => out -> m ())
       -> m ()
     )
  -> StreamType.ServerStreamingHandler m (BinaryRpc serv meth)
mkServerStreaming f = StreamType.mkServerStreaming $ \inp send -> do
    inp' <- decodeOrThrow inp
    f inp' (send . encode)

mkBiDiStreaming :: forall m serv meth.
     MonadThrow m
  => (    (forall inp. Binary inp => m (StreamElem () inp))
      -> (forall out. Binary out => out -> m ())
      -> m ()
    )
  -> StreamType.BiDiStreamingHandler m (BinaryRpc serv meth)
mkBiDiStreaming f = StreamType.mkBiDiStreaming $ \recv send ->
    f (recv >>= traverse decodeOrThrow) (send . encode)