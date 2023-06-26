module Network.GRPC.Client.StreamType (
    -- ** Obtain handler for specific RPC call
    ClientHandler(..)
  , rpc
  ) where

import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Default
import Data.Proxy

import Network.GRPC.Client
import Network.GRPC.Common.StreamType
import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  Obtain handler for specific RPC call
-------------------------------------------------------------------------------}

class ClientHandler h where
  rpcWith :: IsRPC rpc => Connection -> CallParams -> Proxy rpc -> h rpc

-- | Construct RPC handler
--
-- See 'nonStreaming' and friends for example usage.
--
-- If you want to use non-default 'CallParams', use 'rpcWith'.
rpc :: forall rpc h. (ClientHandler h, IsRPC rpc)  => Connection -> h rpc
rpc conn = rpcWith conn def (Proxy @rpc)

instance ( MonadIO   m
         , MonadMask m
         ) => ClientHandler (NonStreamingHandler m) where
  rpcWith conn params proxy =
    UnsafeNonStreamingHandler $ \input ->
      withRPC conn params proxy $ \call -> do
        sendFinalInput call input
        (output, _trailers) <- recvFinalOutput call
        return output

instance ( MonadIO   m
         , MonadMask m
         ) => ClientHandler (ClientStreamingHandler m) where
  rpcWith conn params proxy =
    UnsafeClientStreamingHandler $ \produceInput ->
      withRPC conn params proxy $ \call -> do
        sendAllInputs call produceInput
        (output, _trailers) <- recvFinalOutput call
        return output

instance ( MonadIO   m
         , MonadMask m
         ) => ClientHandler (ServerStreamingHandler m) where
  rpcWith conn params proxy =
    UnsafeServerStreamingHandler $ \input processOutput ->
      withRPC conn params proxy $ \call -> do
        sendFinalInput call input
        _trailers <- recvAllOutputs call processOutput
        return ()

-- | Bidirectional streaming
--
-- Unlike the other functions, we have not generalized this to an arbitrary
-- monad @m@, because it spawns a new thread.
--
-- TODO: This is an indication that we should reconsider the signature of this
-- function. Another indication that we should is that the corresponding
-- function in "Network.GRPC.Protobuf.Pipes" cannot be defined in terms
-- of this function, unlike for the other streaming functions.
instance ClientHandler (BiDiStreamingHandler IO) where
  rpcWith conn params proxy =
    UnsafeBiDiStreamingHandler $ \produceInput processOutput ->
      withRPC conn params proxy $ \call -> do
        withAsync (sendAllInputs call produceInput) $ \_threadId -> do
          _trailers <- recvAllOutputs call processOutput
          return ()

