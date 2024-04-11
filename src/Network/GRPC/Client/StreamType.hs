-- | Construct client handlers
--
-- This is an internal module only; see "Network.GRPC.Client.StreamType.IO"
-- for the main public module.
module Network.GRPC.Client.StreamType (
    -- * Obtain handler for specific RPC call
    ClientHandler(..)
  , CanCallRPC(..)
  , rpc
  ) where

import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Default
import Data.Proxy

import Network.GRPC.Client.Call
import Network.GRPC.Client.Connection
import Network.GRPC.Common.StreamElem
import Network.GRPC.Common.StreamType
import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  CanCallRPC
-------------------------------------------------------------------------------}

-- | Monads in which we make RPC calls
--
-- In order to be able to make an RPC call, we need
--
-- * 'MonadIO' (obviously)
-- * 'MonadMask' in order to ensure that the RPC call is terminated cleanly
-- * Access to the 'Connection' to the server
class (MonadIO m, MonadMask m) => CanCallRPC m where
  getConnection :: m Connection

instance (MonadIO m, MonadMask m) => CanCallRPC (ReaderT Connection m) where
  getConnection = ask

{-------------------------------------------------------------------------------
  Obtain handler for specific RPC call
-------------------------------------------------------------------------------}

class ClientHandler h where
  rpcWith :: SupportsClientRpc rpc => CallParams rpc -> Proxy rpc -> h rpc

-- | Construct RPC handler
--
-- See 'nonStreaming' and friends for example usage.
--
-- If you want to use non-default 'CallParams', use 'rpcWith'.
rpc :: forall rpc h.
     ( ClientHandler h
     , SupportsClientRpc rpc
     , Default (RequestMetadata rpc)
     )
  => h rpc
rpc = rpcWith def (Proxy @rpc)

instance CanCallRPC m => ClientHandler (NonStreamingHandler m) where
  rpcWith params proxy =
    UnsafeNonStreamingHandler $ \input -> do
      conn <- getConnection
      withRPC conn params proxy $ \call -> do
        sendFinalInput call input
        (output, _trailers) <- recvFinalOutput call
        return output

instance CanCallRPC m => ClientHandler (ClientStreamingHandler m) where
  rpcWith params proxy =
    UnsafeClientStreamingHandler $ \produceInput -> do
      conn <- getConnection
      withRPC conn params proxy $ \call -> do
        sendAllInputs call produceInput
        (output, _trailers) <- recvFinalOutput call
        return output

instance CanCallRPC m => ClientHandler (ServerStreamingHandler m) where
  rpcWith params proxy =
    UnsafeServerStreamingHandler $ \input processOutput -> do
      conn <- getConnection
      withRPC conn params proxy $ \call -> do
        sendFinalInput call input
        _trailers <- recvAllOutputs call processOutput
        return ()

-- | Bidirectional streaming
--
-- Unlike the other functions, we have not generalized this to an arbitrary
-- monad @m@, because it spawns a new thread.
instance ClientHandler (BiDiStreamingHandler (ReaderT Connection IO)) where
  rpcWith :: forall rpc.
       SupportsClientRpc rpc
    => CallParams rpc
    -> Proxy rpc
    -> BiDiStreamingHandler (ReaderT Connection IO) rpc
  rpcWith params proxy =
    UnsafeBiDiStreamingHandler $ \produceInput processOutput ->
      ReaderT $ \conn -> do

        let produceInput' :: IO (StreamElem NoMetadata (Input rpc))
            produceInput' = flip runReaderT conn produceInput

            processOutput' :: Output rpc -> IO ()
            processOutput' = flip runReaderT conn . processOutput

        withRPC conn params proxy $ \call -> do
          withAsync (sendAllInputs call produceInput') $ \_threadId -> do
            _trailers <- recvAllOutputs call processOutput'
            return ()
