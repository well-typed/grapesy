-- | Wrap "Network.GRPC.Client.StreamType.IO" to handle binary serialization
--
-- **NOTE**: The custom binary gRPC format provided by @grapesy@ is designed
-- so that the type of each message during communication can be different.
-- The wrappers in this module lose this generalization: every input and every
-- output must be of the same type. If this is too severe a limitaiton, you
-- should use the functionality from "Network.GRPC.Client.Binary" instead.
module Network.GRPC.Client.StreamType.IO.Binary (
    module Network.GRPC.Client.StreamType
    -- * Stream types
  , nonStreaming
  , clientStreaming
  , serverStreaming
  , biDiStreaming
  ) where

import Control.Monad
import Control.Monad.Reader
import Data.Binary

import Network.GRPC.Client (Connection)
import Network.GRPC.Client.StreamType hiding (CanCallRPC(..))
import Network.GRPC.Client.StreamType.IO qualified as IO
import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Common.StreamType

{-------------------------------------------------------------------------------
  Wrappers for common streaming types that handle encoding/decoding
-------------------------------------------------------------------------------}

-- | Wrapper for 'IO.nonStreaming' that handles binary serialization
nonStreaming :: forall inp out serv meth m.
     (Binary inp, Binary out, MonadIO m)
  => Connection
  -> NonStreamingHandler (ReaderT Connection m) (BinaryRpc serv meth)
  -> inp -> m out
nonStreaming conn h inp =
    IO.nonStreaming conn h (encode inp) >>= decodeOrThrow

-- | Wrapper for 'IO.clientStreaming' that handles binary serialization
clientStreaming :: forall inp out serv meth m.
     (Binary inp, Binary out, MonadIO m)
  => Connection
  -> ClientStreamingHandler (ReaderT Connection m) (BinaryRpc serv meth)
  -> m (StreamElem NoMetadata inp)
  -> m out
clientStreaming conn h f =
    IO.clientStreaming conn h (fmap encode <$> f) >>= decodeOrThrow

-- | Wrapper for 'IO.serverStreaming' that binary serialization
serverStreaming :: forall inp out serv meth m.
     (Binary inp, Binary out, MonadIO m)
  => Connection
  -> ServerStreamingHandler (ReaderT Connection m) (BinaryRpc serv meth)
  -> inp
  -> (out -> m ())
  -> m ()
serverStreaming conn h inp f =
    IO.serverStreaming conn h (encode inp) (f <=< decodeOrThrow)

-- | Wrapper for 'IO.biDiStreaming' that handles binary serialization
biDiStreaming :: forall inp out serv meth m.
     (Binary inp, Binary out, MonadIO m)
  => Connection
  -> BiDiStreamingHandler (ReaderT Connection m) (BinaryRpc serv meth)
  -> m (StreamElem NoMetadata inp)
  -> (out -> m ())
  -> m ()
biDiStreaming conn h inp f =
    IO.biDiStreaming conn h (fmap encode <$> inp) (f <=< decodeOrThrow)
