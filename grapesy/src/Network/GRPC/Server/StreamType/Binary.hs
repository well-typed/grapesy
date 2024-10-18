module Network.GRPC.Server.StreamType.Binary (
    -- * Construct handlers
    mkNonStreaming
  , mkClientStreaming
  , mkServerStreaming
  , mkBiDiStreaming
  ) where

import Control.Monad.IO.Class
import Data.Binary
import Data.ByteString.Lazy qualified as Lazy (ByteString)

import Network.GRPC.Common
import Network.GRPC.Common.Binary (decodeOrThrow)
import Network.GRPC.Common.StreamType
import Network.GRPC.Server.StreamType qualified as StreamType
import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Handlers for specific streaming types

  It may sometimes be useful to use explicit type applications with these
  functions, which is why the @rpc@ type variable is always first, followed by
  the @inp@ and @out@ parameters, which may also be ambiguous in certain cases.
-------------------------------------------------------------------------------}

mkNonStreaming :: forall rpc inp out m.
     ( SupportsStreamingType rpc NonStreaming
     , Binary inp, Input  rpc ~ Lazy.ByteString
     , Binary out, Output rpc ~ Lazy.ByteString
     , MonadIO m
     )
  => (    inp
       -> m out
     )
  -> ServerHandler' NonStreaming m rpc
mkNonStreaming f = StreamType.mkNonStreaming $ \inp -> do
    inp' <- decodeOrThrow inp
    encode <$> f inp'

mkClientStreaming :: forall rpc out m.
     ( SupportsStreamingType rpc ClientStreaming
     , Binary out, Output rpc ~ Lazy.ByteString
     , MonadIO m
     )
  => (    (forall inp.
                (Binary inp, Input rpc ~ Lazy.ByteString)
             => m (NextElem inp)
          )
       -> m out
     )
  -> ServerHandler' ClientStreaming m rpc
mkClientStreaming f = StreamType.mkClientStreaming $ \recv -> do
    out <- f (liftIO recv >>= traverse decodeOrThrow)
    return $ encode out

mkServerStreaming :: forall rpc inp m.
     ( SupportsStreamingType rpc ServerStreaming
     , Binary inp, Input rpc ~ Lazy.ByteString
     , MonadIO m
     )
  => (    inp
       -> (forall out.
                (Binary out, Output rpc ~ Lazy.ByteString)
             => NextElem out -> m ()
          )
       -> m ()
     )
  -> ServerHandler' ServerStreaming m rpc
mkServerStreaming f = StreamType.mkServerStreaming $ \inp send -> do
    inp' <- decodeOrThrow inp
    f inp' (liftIO . send . fmap encode)

mkBiDiStreaming :: forall rpc m.
     ( SupportsStreamingType rpc BiDiStreaming
     , MonadIO m
     )
  => (    (forall inp.
                (Binary inp, Input rpc ~ Lazy.ByteString)
             => m (NextElem inp)
          )
       -> (forall out.
                (Binary out, Output rpc ~ Lazy.ByteString)
             => NextElem out -> m ()
          )
       -> m ()
     )
  -> ServerHandler' BiDiStreaming m rpc
mkBiDiStreaming f = StreamType.mkBiDiStreaming $ \recv send ->
    f (liftIO recv >>= traverse decodeOrThrow) (liftIO . send . fmap encode)
