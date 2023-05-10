{-# LANGUAGE OverloadedStrings #-}

-- | gRPC with Protobuf
--
-- Intended for unqualified import.
module Network.GRPC.Protobuf (
    RPC(..)
    -- * Specialized calls
    --
    -- $specialized
  , nonStreaming
  , clientStreaming
  , serverStreaming
  , biDiStreaming
    -- * Exceptions
  , ProtobufException(..)
  , expectedOutput
  , expectedTrailers
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Kind
import Data.ProtoLens.Service.Types
import Data.Proxy
import GHC.TypeLits

import Network.GRPC.Client
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Protobuf
import Network.GRPC.Protobuf.RequireStreamingType

-- $specialized
--
-- The
-- [gRPC specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
-- does not distinguish between different kinds of communication patterns;
-- this is done in the
-- [Protobuf specification](https://protobuf.dev/reference/protobuf/proto3-spec/#service_definition).
-- We provide convenience functions here that capture these communication
-- patterns. It is important to realize however that these wrappers are simple;
-- if they do not provide the functionality you require, using the more general
-- interface from "Network.GRPC.Client" is not much more difficult.

{-------------------------------------------------------------------------------
  Specialized calls

  TODO: We should figure out what happens when gRPC services return failure,
  and deal with this properly (right now this will probably result in
  'ProtobufUnexpectedTrailers', which may or may not be ok).
-------------------------------------------------------------------------------}

nonStreaming ::
     IsRPC (RPC s m)
  => MethodStreamingType s m ~ NonStreaming
  => Connection
  -> PerCallParams
  -> RPC s m
  -> Input (RPC s m)
  -- ^ Single input
  -> IO (Output (RPC s m), Trailers)
  -- ^ Single output
nonStreaming conn params rpc input =
    requireStreamingType rpc (Proxy @NonStreaming) $
      withRPC conn params rpc $ \call -> do
        atomically $ sendInput call Final $ Just input
        output   <- atomically $ recvOutput call
                             >>= either (expectedOutput rpc) return
        trailers <- atomically $ recvOutput call
                             >>= either return (expectedTrailers rpc)
        return (output, trailers)

clientStreaming :: forall s m.
     IsRPC (RPC s m)
  => MethodStreamingType s m ~ ClientStreaming
  => Connection
  -> PerCallParams
  -> RPC s m
  -> IO (IsFinal, Maybe (Input (RPC s m)))
  -- ^ We will repeatedly call this function until it returns a 'Final' 'Input'.
  --
  -- You should return @(Final, Nothing)@ /only/ in the case that the client
  -- sends /no/ messages; see discussion in "Network.GRPC.Client".
  -> IO (MethodOutput s m, Trailers)
  -- ^ Single output
clientStreaming conn params rpc produceInput =
    requireStreamingType rpc (Proxy @ClientStreaming) $
      withRPC conn params rpc $ \call -> do
        sendAll produceInput call
        output   <- atomically $ recvOutput call
                             >>= either (expectedOutput rpc) return
        trailers <- atomically $ recvOutput call
                             >>= either return (expectedTrailers rpc)
        return (output, trailers)

serverStreaming :: forall s m.
     IsRPC (RPC s m)
  => MethodStreamingType s m ~ ServerStreaming
  => Connection
  -> PerCallParams
  -> RPC s m
  -> Input (RPC s m)
  -- ^ Single input
  -> (Output (RPC s m) -> IO ())
  -- ^ We wil call this function for every 'Output' we receive.
  -> IO Trailers
serverStreaming conn params rpc input processOutput =
    requireStreamingType rpc (Proxy @ServerStreaming) $
      withRPC conn params rpc $ \call -> do
        atomically $ sendInput call Final $ Just input
        recvAll processOutput call

biDiStreaming :: forall s m.
     IsRPC (RPC s m)
  => MethodStreamingType s m ~ BiDiStreaming
  => Connection
  -> PerCallParams
  -> RPC s m
  -> IO (IsFinal, Maybe (Input (RPC s m)))
  -- ^ We will repeatedly call this function until it returns a 'Final' 'Input'
  -- or until we receive 'Trailers' from the remote node, whichever comes first.
  -> (Output (RPC s m) -> IO ())
  -- ^ We wil call this function for every 'Output' we receive.
  -> IO Trailers
biDiStreaming conn params rpc produceInput processOutput =
    requireStreamingType rpc (Proxy @BiDiStreaming) $
      withRPC conn params rpc $ \call -> do
        withAsync (sendAll produceInput call) $ \_sendAllThread ->
          recvAll processOutput call

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

sendAll :: IO (IsFinal, Maybe (MethodInput s m)) -> Call (RPC s m) -> IO ()
sendAll produceInput call = loop
  where
    loop :: IO ()
    loop = do
        (isFinal, input) <- produceInput
        atomically $ sendInput call isFinal input
        unless (isFinal == Final) loop

recvAll :: (MethodOutput s m -> IO a) -> Call (RPC s m) -> IO Trailers
recvAll processOutput call = loop
  where
    loop :: IO Trailers
    loop = do
        mOutput <- atomically $ recvOutput call
        case mOutput of
          Left  trailers -> return trailers
          Right output   -> processOutput output >> loop

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data ProtobufException =
    forall s m.
         IsRPC (RPC s m)
      => ProtobufRpcException (RPC s m) (ProtobufRpcException s m)

deriving stock    instance Show      ProtobufException
deriving anyclass instance Exception ProtobufException

data ProtobufRpcException (s :: Type) (m :: Symbol) =
    -- | We expected an output, but got trailers instead
    ProtobufUnexpectedTrailers Trailers

    -- | We expected trailers, but got an output instead
  | ProtobufUnexpectedOutput (Output (RPC s m))

deriving stock instance IsRPC (RPC s m) => Show (ProtobufRpcException s m)

expectedOutput ::
     IsRPC (RPC s m)
  => RPC s m -> Trailers -> STM (MethodOutput s m)
expectedOutput rpc trailers = throwSTM $
    ProtobufRpcException rpc $
      ProtobufUnexpectedTrailers trailers

expectedTrailers ::
     IsRPC (RPC s m)
  => RPC s m -> MethodOutput s m -> STM Trailers
expectedTrailers rpc output = throwSTM $
    ProtobufRpcException rpc $
      ProtobufUnexpectedOutput output


