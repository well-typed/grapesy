-- | Wrap "Network.GRPC.Client.StreamType.IO" to handle binary serialization
--
-- **NOTE**: The custom binary gRPC format provided by @grapesy@ is designed
-- so that the type of each message during communication can be different.
-- The wrappers in this module lose this generalization: every input and every
-- output must be of the same type. If this is too severe a limitaiton, you
-- should use the functionality from "Network.GRPC.Client.Binary" instead.
module Network.GRPC.Client.StreamType.IO.Binary (
    -- * Run client handlers
    nonStreaming
  , clientStreaming
  , clientStreaming_
  , serverStreaming
  , biDiStreaming
  ) where

import Control.Monad.Reader
import Data.Binary
import Data.ByteString.Lazy qualified as Lazy (ByteString)

import Network.GRPC.Client (Connection)
import Network.GRPC.Client.StreamType.IO qualified as IO
import Network.GRPC.Common
import Network.GRPC.Common.Binary (decodeOrThrow)
import Network.GRPC.Common.StreamType
import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Run client handlers
-------------------------------------------------------------------------------}

-- | Wrapper for 'IO.nonStreaming' that handles binary serialization
nonStreaming :: forall inp out rpc m.
     ( Binary inp, Input  rpc ~ Lazy.ByteString
     , Binary out, Output rpc ~ Lazy.ByteString
     , MonadIO m
     )
  => Connection
  -> ClientHandler' NonStreaming (ReaderT Connection m) rpc
  -> inp -> m out
nonStreaming conn h inp =
    IO.nonStreaming conn h (encode inp) >>= decodeOrThrow

-- | Wrapper for 'IO.clientStreaming' that handles binary serialization
clientStreaming :: forall inp out rpc m r.
     ( Binary inp, Input  rpc ~ Lazy.ByteString
     , Binary out, Output rpc ~ Lazy.ByteString
     , MonadIO m
     )
  => Connection
  -> ClientHandler' ClientStreaming (ReaderT Connection m) rpc
  -> (    (NextElem inp -> m ())
       -> m r
     )
  -> m (out, r)
clientStreaming conn h k = do
    (out, r) <- IO.clientStreaming conn h $ \send -> k (send . fmap encode)
    (, r) <$> decodeOrThrow out

-- | Wrapper for 'IO.clientStreaming_' that handles binary serialization
clientStreaming_ :: forall inp out rpc m.
     ( Binary inp, Input  rpc ~ Lazy.ByteString
     , Binary out, Output rpc ~ Lazy.ByteString
     , MonadIO m
     )
  => Connection
  -> ClientHandler' ClientStreaming (ReaderT Connection m) rpc
  -> (   (NextElem inp -> m ())
       -> m ()
     )
  -> m out
clientStreaming_ conn h k = fst <$> clientStreaming conn h k

-- | Wrapper for 'IO.serverStreaming' that binary serialization
serverStreaming :: forall inp out rpc m r.
     ( Binary inp, Input  rpc ~ Lazy.ByteString
     , Binary out, Output rpc ~ Lazy.ByteString
     , MonadIO m
     )
  => Connection
  -> ClientHandler' ServerStreaming (ReaderT Connection m) rpc
  -> inp
  -> (    m (NextElem out)
       -> m r
     )
  -> m r
serverStreaming conn h inp k =
    IO.serverStreaming conn h (encode inp) $ \recv ->
      k (traverse decodeOrThrow =<< recv)

-- | Wrapper for 'IO.biDiStreaming' that handles binary serialization
biDiStreaming :: forall inp out rpc m r.
     ( Binary inp, Input  rpc ~ Lazy.ByteString
     , Binary out, Output rpc ~ Lazy.ByteString
     , MonadIO m
     )
  => Connection
  -> ClientHandler' BiDiStreaming (ReaderT Connection m) rpc
  -> (    (NextElem inp -> m ())
       -> m (NextElem out)
       -> m r
     )
  -> m r
biDiStreaming conn h k =
    IO.biDiStreaming conn h $ \send recv ->
      k (send . fmap encode)
        (traverse decodeOrThrow =<< recv)
