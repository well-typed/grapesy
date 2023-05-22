{-# LANGUAGE OverloadedStrings #-}

-- | gRPC with Protobuf
--
-- Intended for unqualified import.
module Network.GRPC.Client.Protobuf (
    RPC(..)

    -- * Specialized calls
    --
    -- $specialized
  , nonStreaming
  , clientStreaming
  , serverStreaming
  , biDiStreaming
  ) where

import Control.Concurrent.Async
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.ProtoLens.Service.Types
import Data.Proxy

import Network.GRPC.Client
import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.Protobuf
import Network.GRPC.Util.RedundantConstraint

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
-------------------------------------------------------------------------------}

-- | Non-streaming call (single input, single output)
nonStreaming :: forall m serv meth.
     (MonadIO m, MonadMask m)
  => IsRPC (RPC serv meth)
  => MethodStreamingType serv meth ~ NonStreaming
  => Connection
  -> CallParams
  -> RPC serv meth
  -> Handler m serv meth
nonStreaming conn params rpc input =
    withRPC conn params rpc $ \call -> do
      sendOnlyInput call input
      recvOnlyOutput call
  where
    _ = addConstraint $ Proxy @(MethodStreamingType serv meth ~ NonStreaming)

-- | Client-side streaming
clientStreaming :: forall m serv meth.
     (MonadIO m, MonadMask m)
  => IsRPC (RPC serv meth)
  => MethodStreamingType serv meth ~ ClientStreaming
  => Connection
  -> CallParams
  -> RPC serv meth
  -> Handler m serv meth
clientStreaming conn params rpc produceInput =
    withRPC conn params rpc $ \call -> do
      sendAllInputs call produceInput
      recvOnlyOutput call
  where
    _ = addConstraint $ Proxy @(MethodStreamingType serv meth ~ ClientStreaming)

-- | Server-side streaming
serverStreaming :: forall m serv meth.
     (MonadIO m, MonadMask m)
  => IsRPC (RPC serv meth)
  => MethodStreamingType serv meth ~ ServerStreaming
  => Connection
  -> CallParams
  -> RPC serv meth
  -> Handler m serv meth
serverStreaming conn params rpc input processOutput =
    withRPC conn params rpc $ \call -> do
      sendOnlyInput call input
      recvAllOutputs call processOutput
  where
    _ = addConstraint $ Proxy @(MethodStreamingType serv meth ~ ServerStreaming)

-- | Bidirectional streaming
--
-- Unlike the other functions, we have not generalized this to an arbitrary
-- monad @m@, because it spawns a new thread.
--
-- TODO: This is an indication that we should reconsider the signature of this
-- function. Another indication that we should is that the corresponding
-- function in "Network.GRPC.Client.Protobuf.Pipes" cannot be defined in terms
-- of this function, unlike for the other streaming functions.
biDiStreaming :: forall serv meth.
     IsRPC (RPC serv meth)
  => MethodStreamingType serv meth ~ BiDiStreaming
  => Connection
  -> CallParams
  -> RPC serv meth
  -> Handler IO serv meth
biDiStreaming conn params rpc produceInput processOutput =
    withRPC conn params rpc $ \call -> do
      withAsync (sendAllInputs call produceInput) $ \_sendAllThread ->
        recvAllOutputs call processOutput
  where
    _ = addConstraint $ Proxy @(MethodStreamingType serv meth ~ BiDiStreaming)

