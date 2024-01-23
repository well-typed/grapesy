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

    -- * Stream types
  , nonStreaming
  , clientStreaming
  , serverStreaming
  , biDiStreaming
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Binary

import Network.GRPC.Client (Call)
import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Common.StreamType qualified as StreamType

{-------------------------------------------------------------------------------
  Convenience wrappers using @binary@ for serialization/deserialization

  Unlike for the server, we do /not/ wrap the client handlers here, because they
  are not a good match. The standard client streaming handlers expect a /single/
  IO action that produces all inputs and/or a single IO action that handles all
  outputs, but the raw binary protocol allows message types to be different at
  each point in the communication.

  These functions all have the type of the value sent or received as the /first/
  argument, to facilitate the use of type arguments.
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

{-------------------------------------------------------------------------------
  Wrappers for common streaming types that handle encoding/decoding
-------------------------------------------------------------------------------}

-- | Wrapper for 'StreamType.nonStreaming' that handles encoding\/decoding of
-- input\/output.
nonStreaming :: forall inp out serv meth m.
     (Binary inp, Binary out, MonadThrow m)
  => StreamType.NonStreamingHandler m (BinaryRpc serv meth)
  -> inp -> m out
nonStreaming h inp =
    StreamType.nonStreaming h (encode inp) >>= decodeOrThrow

-- | Wrapper for 'StreamType.clientStreaming' that handles encoding\/decoding of
-- input\/output.
clientStreaming :: forall inp out serv meth m.
     (Binary inp, Binary out, MonadThrow m)
  => StreamType.ClientStreamingHandler m (BinaryRpc serv meth)
  -> m (StreamElem NoMetadata inp)
  -> m out
clientStreaming h f =
    StreamType.clientStreaming h (fmap encode <$> f) >>= decodeOrThrow

-- | Wrapper for 'StreamType.serverStreaming' that handles encoding\/decoding of
-- input\/output.
serverStreaming :: forall inp out serv meth m.
     (Binary inp, Binary out, MonadThrow m)
  => StreamType.ServerStreamingHandler m (BinaryRpc serv meth)
  -> inp
  -> (out -> m ())
  -> m ()
serverStreaming h inp f =
    StreamType.serverStreaming h (encode inp) (f <=< decodeOrThrow)

-- | Wrapper for 'StreamType.biDiStreaming' that handles encoding\/decoding of
-- input\/output.
biDiStreaming :: forall inp out serv meth m.
     (Binary inp, Binary out, MonadThrow m)
  => StreamType.BiDiStreamingHandler m (BinaryRpc serv meth)
  -> m (StreamElem NoMetadata inp)
  -> (out -> m ())
  -> m ()
biDiStreaming h inp f =
    StreamType.biDiStreaming h (fmap encode <$> inp) (f <=< decodeOrThrow)
