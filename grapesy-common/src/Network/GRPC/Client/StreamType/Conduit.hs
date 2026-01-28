-- | [Conduit](https://hackage.haskell.org/package/conduit) interface
module Network.GRPC.Client.StreamType.Conduit (
    -- * Run client handlers
    clientStreaming
  , clientStreaming_
  , serverStreaming
  , biDiStreaming
  ) where

import Network.GRPC.Util.Imports

import Control.Monad.Reader (ReaderT, lift)
import Data.Conduit (ConduitT, yield, await)

import Network.GRPC.Client.Connection
import Network.GRPC.Client.StreamType.IO qualified as IO

{-------------------------------------------------------------------------------
  Conduits for different kinds of streaming types (communication patterns)
-------------------------------------------------------------------------------}

clientStreaming ::
     MonadIO m
  => Connection
  -> ClientHandler' ClientStreaming (ReaderT Connection m) rpc
  -> (    ConduitT (Input rpc) Void m ()
       -> m r
     )
  -> m (Output rpc, r)
clientStreaming conn h k =
    IO.clientStreaming conn h $ \send ->
      k $ toSink send

clientStreaming_ ::
     MonadIO m
  => Connection
  -> ClientHandler' ClientStreaming (ReaderT Connection m) rpc
  -> (    ConduitT (Input rpc) Void m ()
       -> m ()
     )
  -> m (Output rpc)
clientStreaming_ conn h k =
    fst <$> clientStreaming conn h k

serverStreaming ::
     MonadIO m
  => Connection
  -> ClientHandler' ServerStreaming (ReaderT Connection m) rpc
  -> Input rpc
  -> (    ConduitT () (Output rpc) m ()
       -> m r
     )
  -> m r
serverStreaming conn h input k =
    IO.serverStreaming conn h input $ \recv ->
      k $ toSource recv

biDiStreaming :: forall rpc m r.
     MonadIO m
  => Connection
  -> ClientHandler' BiDiStreaming (ReaderT Connection m) rpc
  -> (    ConduitT (Input rpc) Void         m ()
       -> ConduitT ()          (Output rpc) m ()
       -> m r
     )
  -> m r
biDiStreaming conn h k =
    IO.biDiStreaming conn h $ \send recv ->
      k (toSink send) (toSource recv)

{-------------------------------------------------------------------------------
  Internal auxiliary: conduit
-------------------------------------------------------------------------------}

toSource :: forall m a. Monad m => m (NextElem a) -> ConduitT () a m ()
toSource f = loop
  where
    loop :: ConduitT () a m ()
    loop = do
        ma <- lift f
        case ma of
          NoNextElem -> return ()
          NextElem a -> yield a >> loop

toSink :: forall m a. Monad m => (NextElem a -> m ()) -> ConduitT a Void m ()
toSink f = loop
  where
    loop :: ConduitT a Void m ()
    loop = do
        ma <- await
        case ma of
          Nothing -> lift (f $ NoNextElem)
          Just a  -> lift (f $ NextElem a) >> loop
