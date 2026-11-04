-- | Execute handlers for specific communication patterns
--
-- See also "Network.GRPC.Common.StreamType" as well as
-- "Network.GRPC.Client.StreamType.CanCallRPC".
module Network.GRPC.Client.StreamType.IO (
    nonStreaming
  , clientStreaming
  , clientStreaming_
  , serverStreaming
  , biDiStreaming
  ) where

import Network.GRPC.Util.Imports
import Control.Monad.Reader (ReaderT, runReaderT, lift)

import Network.GRPC.Client.Connection
import Network.GRPC.Client.StreamType.CanCallRPC qualified as CanCallRPC

{-------------------------------------------------------------------------------
  Run client handlers

  We piggy-back on the definitions for a general monad stack with an instance of
  'CanCallRPC', but /run/ in a 'ReaderT' monad stack which satisfies that
  constraint. The caller can then just provide an explicit 'Connection'.
-------------------------------------------------------------------------------}

-- | Make a non-streaming RPC
--
-- Example usage:
--
-- > type GetFeature = Protobuf RouteGuide "getFeature"
-- >
-- > getFeature :: Connection -> Point -> IO ()
-- > getFeature conn point = do
-- >     features <- nonStreaming conn (rpc @GetFeature) point
-- >     print features
nonStreaming :: forall rpc m.
     Connection
  -> ClientHandler' NonStreaming (ReaderT Connection m) rpc
  -> Input rpc
  -> m (Output rpc)
nonStreaming conn h input = flip runReaderT conn $
    CanCallRPC.nonStreaming h input

-- | Generalization of 'clientStreaming_' with an additional result.
clientStreaming :: forall rpc m r.
     MonadIO m
  => Connection
  -> ClientHandler' ClientStreaming (ReaderT Connection m) rpc
  -> (    (NextElem (Input rpc) -> m ())
       -> m r
     )
  -> m (Output rpc, r)
clientStreaming conn h k = flip runReaderT conn $
    CanCallRPC.clientStreaming h $ \send -> lift $
      k (liftIO . send)

-- | Make a client-side streaming RPC
--
-- Example usage:
--
-- > type RecordRoute = Protobuf RouteGuide "recordRoute"
-- >
-- > recordRoute :: Connection -> [Point] -> IO ()
-- > recordRoute conn points = do
-- >     summary <- clientStreaming_ conn (rpc @RecordRoute) $ \send ->
-- >                  forM_ points send
-- >     print summary
clientStreaming_ :: forall rpc m.
     MonadIO m
  => Connection
  -> ClientHandler' ClientStreaming (ReaderT Connection m) rpc
  -> (   (NextElem (Input rpc) -> m ())
       -> m ()
     )
  -> m (Output rpc)
clientStreaming_ conn h k = fst <$> clientStreaming conn h k

-- | Make a server-side streaming RPC
--
-- Example usage:
--
-- > type ListFeatures = Protobuf RouteGuide "listFeatures"
-- >
-- > listFeatures :: Connection -> Rectangle -> IO ()
-- > listFeatures conn rect =
-- >     serverStreaming conn (rpc @ListFeatures) rect $ \recv ->
-- >       whileJust_ recv print
serverStreaming :: forall rpc m r.
     MonadIO m
  => Connection
  -> ClientHandler' ServerStreaming (ReaderT Connection m) rpc
  -> Input rpc
  -> (    m (NextElem (Output rpc))
       -> m r
     )
  -> m r
serverStreaming conn h input k = flip runReaderT conn $
    CanCallRPC.serverStreaming h input $ \recv -> lift $
      k (liftIO recv)

-- | Make a bidirectional RPC
--
-- Example usage:
--
-- > type RouteChat = Protobuf RouteGuide "routeChat"
-- >
-- > routeChat :: Connection -> [RouteNote] -> IO ()
-- > routeChat conn notes =
-- >     biDiStreaming conn (rpc @RouteChat) $ \send recv ->
-- >       forM_ notes $ \note -> do
-- >         send note
-- >         print =<< recv
biDiStreaming :: forall rpc m r.
     MonadIO m
  => Connection
  -> ClientHandler' BiDiStreaming (ReaderT Connection m) rpc
  -> (    (NextElem (Input rpc) -> m ())
       -> m (NextElem (Output rpc))
       -> m r
     )
  -> m r
biDiStreaming conn h k = flip runReaderT conn $
    CanCallRPC.biDiStreaming h $ \send recv -> lift $
      k (liftIO . send) (liftIO recv)
